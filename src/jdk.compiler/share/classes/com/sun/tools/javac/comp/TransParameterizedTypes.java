package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.BoundKind;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.Types;
import com.sun.tools.javac.jvm.Target;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class TransParameterizedTypes extends TreeTranslator {

    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private TreeMaker make;
    private final Symtab syms;
    private final Names names;
    private final Target target;
    private final Resolve resolve;
    private final Types types;
    private final Log log;
    private final InstructionVisitor parameterizedMethodCallVisitor;

    private Env<AttrContext> env = null;
    private Symbol.ClassSymbol currentClass = null;
    private JCTree.JCClassDecl currentClassTree = null;
    private Map<Symbol, TransParameterizedTypes.ArgFieldData> currentTypeArgs = Map.of();

    /**
     * Get the instance for this context.
     */
    public static TransParameterizedTypes instance(Context context) {
        var instance = context.get(typeReifierKey);
        if (instance == null)
            instance = new TransParameterizedTypes(context);
        return instance;
    }

    @SuppressWarnings("this-escape")
    private TransParameterizedTypes(Context context) {
        context.put(typeReifierKey, this);
        make = TreeMaker.instance(context);
        syms = Symtab.instance(context);
        names = Names.instance(context);
        target = Target.instance(context);
        resolve = Resolve.instance(context);
        types = Types.instance(context);
        log = Log.instance(context);
        env = Attr.instance(context).env;
        parameterizedMethodCallVisitor = new InstructionVisitor();
    }

    @Override
    public void visitClassDef(JCTree.JCClassDecl tree) {
        result = tree;
        var oldClass = currentClass;
        var oldClassTree = currentClassTree;
        try {
            currentClass = tree.sym;
            currentClassTree = tree;
            if (!tree.sym.owner.packge().name.contentEquals("foo")) return;
            rewriteClass(tree);
        } finally {
            currentClass = oldClass;
            currentClassTree = oldClassTree;
        }
    }

    private void rewriteClass(JCTree.JCClassDecl tree) {
        var isClassParameterized = tree.sym.type.isParameterized();
        var oldTypeArgs = currentTypeArgs;
        try {
            if (isClassParameterized) {
                currentTypeArgs = generateFields(tree);
            } else {
                currentTypeArgs = Map.of();
            }
            rewriteDefs(tree, isClassParameterized);
            currentTypeArgs.forEach((__, data) -> {
                var f = data.field();
                f.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
                f.sym.flags_field |= Flags.SYNTHETIC | Flags.MANDATED;
            });
        } finally {
            currentTypeArgs = oldTypeArgs;
        }
    }

    // region Base arg field generation

    private Map<Symbol, ArgFieldData> generateFields(JCTree.JCClassDecl tree) {
        var result = new LinkedHashMap<Symbol, ArgFieldData>();

        // first generate the base field
        var baseField = generateField(tree, tree.sym);
        result.put(tree.sym, new ArgFieldData(currentClass, baseField, null));

        // then, optionally generate the fields for the super type
        if (tree.extending != null && tree.extending.type.isParameterized() && isParameterizedByClass((JCTree.JCTypeApply) tree.extending)) {
            var extending = (JCTree.JCTypeApply) tree.extending;
            var sym = extending.clazz.type.tsym;
            var field = generateField(tree, sym);
            result.put(sym, new ArgFieldData(sym, field, extending));
        }

        // finally generate the fields for the implementing types
        tree.implementing.forEach(expression -> {
            if (!expression.type.isParameterized()) {
                return;
            }
            var apply = (JCTree.JCTypeApply) expression;
            if (!isParameterizedByClass(apply)) {
                return;
            }

            var sym = apply.clazz.type.tsym;
            var field = generateField(tree, sym);
            result.put(sym, new ArgFieldData(sym, field, apply));
        });

        return result;
    }

    /**
     * This method returns true only if the type parameter represented by typeApply is parameterized by a type parameter
     * declared by the current class.
     * <p>
     * tl;dr it avoid to generates a type arg field for cases where the type parameters are fixed e.g. Foo implements
     * Consumer&lt;String&gt;
     */
    private boolean isParameterizedByClass(JCTree.JCTypeApply typeApply) {
        for (var argument : typeApply.getTypeArguments()) {
            if (argument instanceof JCTree.JCIdent id) {
                if (id.sym instanceof Symbol.TypeVariableSymbol) { // if param is parameterized by the class
                    return true;
                }
            } else if (argument instanceof JCTree.JCTypeApply apply) { // or if it contains a type parameterized by the class
                if (isParameterizedByClass(apply)) {
                    return true;
                }
            } else {
                throw new AssertionError("Unhandled type " + argument.getClass());
            }
        }

        return false;
    }

    private JCTree.JCVariableDecl generateField(JCTree.JCClassDecl tree, Symbol superType) {
        var fieldFlags = Flags.PRIVATE | Flags.FINAL;
        var fieldName = computeArgName(superType);
        var baseField = make.VarDef(
            make.Modifiers(fieldFlags),
            fieldName,
            make.Type(syms.argBaseType),
            null // init is done in constructor
        );
        var fieldSym = new Symbol.VarSymbol(fieldFlags, fieldName, syms.argBaseType, tree.sym);
        baseField.sym = fieldSym;
        baseField.type = syms.argBaseType;

        tree.sym.members_field.enter(fieldSym);
        tree.defs = tree.defs.prepend(baseField); // important to prepend the defs to be able to skip them later
        return baseField;
    }

    // endregion

    private void rewriteDefs(
        JCTree.JCClassDecl tree,
        boolean isClassParameterized
    ) {
        var iterator = tree.defs.iterator();
        for (var i = 0; i < currentTypeArgs.size(); i++) { // skip the fields we just added
            iterator.next();
        }
        while (iterator.hasNext()) {
            var member = iterator.next();

            switch (member.getTag()) {
                case METHODDEF -> {
                    var method = (JCTree.JCMethodDecl) member;

                    if (TreeInfo.isConstructor(member)) {
                        rewriteConstructor(tree, method, isClassParameterized);
                    } else {
                        rewriteBasicMethod(tree, method);
                    }
                }
                // TODO add the class to a queue, to avoid too much recursion
                case CLASSDEF -> rewriteClass((JCTree.JCClassDecl) member);
                case VARDEF -> parameterizedMethodCallVisitor.visitField((JCTree.JCVariableDecl) member);
            }
        }
    }

    // region Constructor rewriting

    private void rewriteConstructor(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl method,
        boolean isClassParameterized
    ) {
        if (isClassParameterized) {
            writeParameterizedClassConstructor(tree, method);
        }

        parameterizedMethodCallVisitor.visitMethod(method);
    }

    private void writeParameterizedClassConstructor(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl method
    ) {
        var copy = copyMethod(method);
        prependArgToMethodParams(method, syms.argBaseType);

        // before modifying the constructor, we create the overload for backward compatibility
        var ident = make.Ident(method.sym);
        ident.name = names._this;
        backwardCompatibilityOverloadMethod(
            tree,
            method,
            copy,
            ident,
            () -> createDefaultTypeArgs(copy.pos, tree)
        );

        // we then check whether the constructor calls another constructor
        var doesCallOverload = TreeInfo.hasConstructorCall(method, names._this);

        // if so, we just need to add the extra argument to the call of the other constructor
        if (doesCallOverload) {
            appendArgToThisCall(method);
        } else { // otherwise, this constructor must set the base field
            setFieldsValues(tree, method);
        }
    }

    private void backwardCompatibilityOverloadMethod(
        JCTree.JCClassDecl clazz,
        JCTree.JCMethodDecl modified,
        JCTree.JCMethodDecl copy,
        JCTree.JCExpression callToBaseMethod,
        Supplier<JCTree.JCExpression> defaultParam
    ) {
        var baseParamTypeArgs = modified.sym.type.getTypeArguments();
        List<JCTree.JCExpression> baseParamTypeArgsCopy = baseParamTypeArgs.isEmpty()
            ? List.nil()
            : make.Types(baseParamTypeArgs);

        var params = new ListBuffer<JCTree.JCExpression>();
        copy.params.forEach(p -> params.add(make.Ident(p)));
        var defaultTypeArgs = defaultParam.get();
        params.prepend(defaultTypeArgs);
        var mtype = methodType(copy.type);
        mtype.argtypes = modified.sym.type.getTypeArguments();
//        TODO uncomment
//        copy.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
//        copy.sym.flags_field |= Flags.SYNTHETIC | Flags.MANDATED;

        var resType = callToBaseMethod.type.asMethodType().restype;
        var call = make.Apply(
            baseParamTypeArgsCopy,
            callToBaseMethod,
            params.toList()
        ).setType(resType);
        call.isParameterized = true;

        var result = types.isSameType(syms.voidType, resType)
            ? make.Exec(call)
            : make.Return(call);

        copy.body = make.at(modified.pos).Block(0L, List.of(result));

        clazz.sym.members_field.enter(copy.sym);
        clazz.defs = clazz.defs.append(copy);
    }

    private JCTree.JCExpression createDefaultTypeArgs(int pos, JCTree.JCClassDecl classDecl) {
        var rawTypeCall = rawTypeOfInvocation(pos);
        var parameterizedTypeCall = parameterizedTypeOfInvocation(pos);
        rawTypeCall.args = List.of(parameterizedTypeCall);

        // MyType.class call as first argument of ParameterizedType.of
        var classFieldAccess = make.Select(
            make.Ident(classDecl.sym),
            syms.getClassField(classDecl.type, types)
        );

        // append of all type arguments wrapped in a ClassType.of call
        var params = defaultParams(classDecl.sym.getTypeParameters());
        parameterizedTypeCall.args = params.prepend(classFieldAccess);

        return rawTypeCall;
    }

    private List<JCTree.JCExpression> defaultParams(List<Symbol.TypeVariableSymbol> params) {
        var buffer = new ListBuffer<JCTree.JCExpression>();
//        params.forEach(parameter -> buffer.add(generateVarSymbolTypes(params, null, parameter)));
        params.forEach(parameter -> {
            var bounds = parameter.getBounds();
            types.erasure(bounds).forEach(t -> {
                var c = classTypeOfInvocation(-1);
                c.args = List.of(
                    make.Select(
                        make.Ident(t.tsym),
                        syms.getClassField(t, types)
                    )
                );
                buffer.append(c);
            });
        });
        return buffer.toList();
    }

    /**
     * Copy the given constructor without its body.
     */
    private JCTree.JCMethodDecl copyMethod(JCTree.JCMethodDecl baseMethod) {
        var treeCopy = make.at(baseMethod.pos).MethodDef(
            baseMethod.mods,
            baseMethod.name,
            baseMethod.restype,
            baseMethod.typarams,
            baseMethod.params,
            baseMethod.thrown,
            null,
            baseMethod.defaultValue
        );
        var sym = baseMethod.sym;
        var symbolCopy = new Symbol.MethodSymbol(
            sym.flags(),
            sym.name,
            sym.type,
            sym.owner
        );
        symbolCopy.params = sym.params;
        symbolCopy.extraParams = sym.extraParams;
        symbolCopy.capturedLocals = sym.capturedLocals;
        symbolCopy.defaultValue = sym.defaultValue;
        symbolCopy.type = copyMethodType(baseMethod);

        treeCopy.sym = symbolCopy;
        treeCopy.type = copyMethodType(baseMethod);

        return treeCopy;
    }

    private Type copyMethodType(JCTree.JCMethodDecl baseMethod) {
        var baseType = new Type.MethodType(
            baseMethod.sym.type.getParameterTypes(),
            baseMethod.sym.type.getReturnType(),
            baseMethod.sym.type.getThrownTypes(),
            baseMethod.type.tsym
        );
        if (baseMethod.getTypeParameters().isEmpty()) {
            return baseType;
        }
        return new Type.ForAll(
            baseMethod.sym.type.getTypeArguments(),
            baseType
        );
    }

    private void prependArgToMethodParams(JCTree.JCMethodDecl method, Type argType) {
        var oldPos = method.pos;
        var sym = method.sym;
        var extraArgParam = make.at(method.pos).Param(
            computeMethodArgName(sym),
            argType,
            sym
        );
        extraArgParam.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
        make.at(oldPos);

        method.params = method.params.prepend(extraArgParam);
        var mtype = method.type.asMethodType();
        mtype.argtypes = mtype.argtypes.prepend(extraArgParam.type);
        sym.extraParams = sym.extraParams.prepend(extraArgParam.sym);
    }

    private void appendArgToThisCall(JCTree.JCMethodDecl constructor) {
        var instructions = constructor.body.stats;
        var call = (JCTree.JCMethodInvocation) ((JCTree.JCExpressionStatement) instructions.getFirst()).expr;
        var args = call.args;
        call.args = args.prepend(make.Ident(constructor.params.getFirst()));
    }

    private void setFieldsValues(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl constructor
    ) {
        var instructions = constructor.body.stats;

        var buffer = new ListBuffer<JCTree.JCStatement>();
        buffer.addAll(instructions);

        var baseArg = constructor.params.getFirst().sym;
        currentTypeArgs.forEach((sym, data) -> {
            var fieldAccess = make.Select(make.This(tree.type), data.field().sym);
            if (tree.sym.equals(sym)) { // base field
                var assign = make.Assign(fieldAccess, make.Ident(baseArg));
                buffer.prepend(make.Exec(assign));
                return;
            }

            var call = createSuperFromConcrete(tree.getTypeParameters(), baseArg, data.typeApply());
            var assign = make.Assign(fieldAccess, call);
            buffer.prepend(make.Exec(assign));
        });

        // we set the field value before calling the super constructor as it is now allowed
        constructor.body.stats = buffer.toList();
    }

    private JCTree.JCMethodInvocation createSuperFromConcrete(
        List<JCTree.JCTypeParameter> classTypeParams,
        Symbol baseArg,
        JCTree.JCTypeApply superTypeParams
    ) {
        var root = parameterizedTypeOfInvocation(-1);
        var args = new ListBuffer<JCTree.JCExpression>();
        var clazz = superTypeParams.clazz;
        args.add(
            make.Select(
                make.Type(clazz.type),
                syms.getClassField(clazz.type, types)
            )
        );
        createSuperArgObject(root, args, classTypeParams, baseArg, superTypeParams);

        root.args = args.toList();
        return root;
    }

    private void createSuperArgObject(
        JCTree.JCMethodInvocation call,
        ListBuffer<JCTree.JCExpression> args,
        List<JCTree.JCTypeParameter> classTypeParams,
        Symbol baseArg,
        JCTree.JCTypeApply superTypeParams
    ) {
        superTypeParams.getTypeArguments().forEach(type -> {
            if (type instanceof JCTree.JCIdent id) { // the param is an identifier, a param type or a concrete type
                // if param is parameterized by the class (Foo<E>), meaning that we must fetch the type from the base arg
                if (id.sym instanceof Symbol.TypeVariableSymbol symbol) {
                    if (baseArg == null) {
                        throw new AssertionError("Base arg is null");
                    }
                    var name = symbol.name;
                    var index = 0;
                    for (var param : classTypeParams) { // this could be changed to a Map<Name, Integer>
                        if (name.equals(param.name)) {
                            break;
                        }
                        index++;
                    }
                    var getArgCall = getGetArgInvocation(-1);
                    getArgCall.args = List.of(make.Ident(baseArg), make.Literal(index));
                    args.add(getArgCall);
                } else { // otherwise, the type is a concrete type (Foo<String>)
                    var clazz = id.sym;
                    var classOfCall = classTypeOfInvocation(-1);
                    classOfCall.args = List.of(
                        make.Select(
                            make.Ident(clazz),
                            syms.getClassField(clazz.type, types)
                        ));
                    args.add(classOfCall);
                }
            } else if (type instanceof JCTree.JCTypeApply apply) { // the param is a parameterized type itself
                var nestedCall = parameterizedTypeOfInvocation(-1);
                var methodArgs = new ListBuffer<JCTree.JCExpression>();
                var clazz = ((JCTree.JCIdent) apply.clazz).sym;
                methodArgs.add(
                    make.Select(
                        make.Ident(clazz),
                        syms.getClassField(clazz.type, types)
                    )
                );
                createSuperArgObject(nestedCall, methodArgs, classTypeParams, baseArg, apply);
                nestedCall.args = methodArgs.toList();
                args.add(nestedCall);
            }
        });

        call.args = args.toList();
    }

    private void rewriteSuperCall(JCTree.JCMethodInvocation superCall) {
        var superClass = currentClass.getSuperclass();
        var superTypeArguments = superClass.getTypeArguments();
        if (superTypeArguments.isEmpty()) return;

        if (currentClass.type.getTypeArguments().isEmpty()) { // this class is not parameterized, we must create the type args
            var call = createSuperFromConcrete(List.nil(), null, (JCTree.JCTypeApply) currentClassTree.extending);
            superCall.args = superCall.args.prepend(call);
        } else {
            // this class is parameterized, we can juste use the type args representing the super class
            var superSym = superClass.tsym;
            // TODO maybe directly store the super field
            var field = currentTypeArgs
                .values()
                .stream()
                .filter(f -> superSym.equals(f.owner()))
                .map(f -> f.field().sym)
                .findFirst()
                .orElseThrow();
            superCall.args = superCall.args.prepend(make.Select(make.This(currentClass.type), field));
        }

        var methodType = superCall.meth.type.asMethodType();
        if (
            methodType.argtypes.isEmpty() || !types.isSameType(syms.argBaseType, methodType.argtypes.getFirst())
        ) {
            methodType.argtypes = methodType.argtypes.prepend(syms.argBaseType);
        }
    }

    // endregion

    // region Basic method rewriting

    private void rewriteBasicMethod(JCTree.JCClassDecl tree, JCTree.JCMethodDecl method) {
        var isMethodParameterized = !method.typarams.isEmpty();

        if (isMethodParameterized) {
            var copy = copyMethod(method);
            prependArgToMethodParams(method, syms.methodTypeArgs);
            backwardCompatibilityOverloadMethod(
                tree,
                method,
                copy,
                make.Ident(method.sym),
                () -> {
                    var call = methodTypeArgsInvocation(-1);
                    call.args = defaultParams(method.sym.getTypeParameters());
                    return call;
                }
            );
        }

        parameterizedMethodCallVisitor.visitMethod(method);
    }

    private final class InstructionVisitor extends TreeTranslator {

        private boolean shouldIgnoreCasts;

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            super.visitApply(tree);
            result = tree;

            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) {
                throw new AssertionError("No symbol for " + tree.meth);
            }

            // if the method is not parameterized, we do not need to do anything
            if (sym.type.getTypeArguments().isEmpty() || ((sym.owner.flags_field & Flags.NEW_GENERICS) == 0)) {
                var mname = TreeInfo.name(tree.meth);
                if (mname != null && mname.equals(names._super)) {
                    rewriteSuperCall(tree);
                }
                return;
            }
            tree.isParameterized = true; // method is parameterized

            var methodType = tree.meth.type.asMethodType();
            var call = methodTypeArgsInvocation(-1);

            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
            // use the inferred types `String s = foo();`
            if (tree.typeargs.isEmpty()) {
                call.args = argTypeParam(methodType.inferredTypes);
            } else {
                var buffer = new ListBuffer<JCTree.JCExpression>();
                tree.typeargs.forEach(t -> buffer.add(generateArgs(null, t.type)));
                call.args = buffer.toList();
            }

            if (
                methodType.argtypes.isEmpty() || !types.isSameType(syms.methodTypeArgs, methodType.argtypes.getFirst())
            ) {
                methodType.argtypes = methodType.argtypes.prepend(syms.methodTypeArgs);
            }

            tree.args = tree.args.prepend(call);
        }

        @Override
        public void visitNewClass(JCTree.JCNewClass tree) {
            super.visitNewClass(tree);
            result = tree;

            var sym = (Symbol.MethodSymbol) tree.constructor;
            var clazz = sym.owner;

            if (clazz.type.getTypeArguments().isEmpty() || ((clazz.flags_field & Flags.NEW_GENERICS) == 0)) {
                return;
            }
            tree.isParameterized = true;

            var cl = (Type.ClassType) tree.type;

            var call = internalTypeArgsConstruction(cl);
            var methodType = tree.constructorType.asMethodType();
            if (
                methodType.argtypes.isEmpty() || !types.isSameType(syms.argBaseType, methodType.argtypes.getFirst())
            ) {
                methodType.argtypes = methodType.argtypes.prepend(syms.argBaseType);
            }

            tree.args = tree.args.prepend(call);
        }

        @Override
        public void visitTypeCast(JCTree.JCTypeCast tree) {
            super.visitTypeCast(tree);
            result = tree;
            if (shouldIgnoreCasts) {
                return;
            }
            JCTree.JCMethodInvocation call;
            if (tree.expr.hasTag(JCTree.Tag.NEWARRAY)) { // if the cast is on a new array, we add the type arg
                call = externalMethodInvocation(
                    -1,
                    "addArrayTypeArg",
                    syms.typeArgUtils,
                    syms.objectType,
                    List.of(syms.objectType, syms.arrayTypeArgs),
                    make::Ident
                );
            } else if (tree.clazz.type.isPrimitive()) { // if the cast is on a primitive, we skip it
                return;
            } else { // otherwise, we replace the cast
                call = externalMethodInvocation(
                    -1,
                    "checkCast",
                    syms.typeOperationsType,
                    syms.objectType,
                    List.of(syms.objectType, syms.argBaseType),
                    make::Ident
                );
            }

            var args = generateArgs(null, tree.clazz.type);
            call.args = List.of(tree.expr, args);
            tree.expr = call;
        }

        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) { // do not recurse on inner classes
            rewriteClass(tree);
            result = tree;
        }

        public void visitField(JCTree.JCVariableDecl field) {
            if (field.init == null) {
                return;
            }

            var old = this.shouldIgnoreCasts;
            this.shouldIgnoreCasts = syms.typeOperationsType.equals(field.sym.owner.type);

            try {
                field.init.accept(this);
            } finally {
                this.shouldIgnoreCasts = old;
            }
        }

        public void visitMethod(JCTree.JCMethodDecl method) {
            if (method.body == null) { // abstract method
                return;
            }

            var oldIgnoreCasts = this.shouldIgnoreCasts;
            this.shouldIgnoreCasts = syms.typeOperationsType.equals(method.sym.owner.type);

            try {
                method.body.accept(this);
            } finally {
                this.shouldIgnoreCasts = oldIgnoreCasts;
            }
        }

    }

    // endregion

    // region utils

    private JCTree.JCMethodInvocation externalMethodInvocation(
        int pos,
        String name,
        Type ownerType,
        Type returnType,
        List<Type> paramTypes,
        Function<Symbol.MethodSymbol, JCTree.JCExpression> methFactory
    ) {
        var methodName = names.fromString(name);
        var sym = resolve.resolveInternalMethod(
            new JCDiagnostic.SimpleDiagnosticPosition(pos),
            env,
            ownerType,
            methodName,
            paramTypes,
            List.nil()
        );
        var methodCall = make.Apply(
            List.nil(),
            null,
            List.nil()
        ).setType(returnType);
        if (sym.isVarArgs()) {
            methodCall.varargsElement = ((Type.ArrayType) sym.params.last().type).elemtype;
        }
        methodCall.meth = methFactory.apply(sym);
        return methodCall;
    }

    private JCTree.JCMethodInvocation rawTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.rawTypeTypeArgs,
            syms.rawTypeTypeArgs,
            List.of(syms.parameterizedTypeTypeArgs),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation parameterizedTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.parameterizedTypeTypeArgs,
            syms.argBaseType,
            List.of(syms.classType, types.makeArrayType(syms.argBaseType)),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation classTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.classTypeArgs,
            syms.classTypeArgs,
            List.of(syms.classType),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation wildcardTypeOfInvocation(boolean isSuper, int pos) {
        var callName = isSuper ? "ofLower" : "ofUpper";
        return externalMethodInvocation(
            pos,
            callName,
            syms.wildcardTypeArgs,
            syms.wildcardTypeArgs,
            List.of(syms.wildcardTypeArgs),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation arrayTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.arrayTypeArgs,
            syms.arrayTypeArgs,
            List.of(syms.argBaseType),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation innerClassTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.innerClassTypeArgs,
            syms.innerClassTypeArgs,
            List.of(syms.argBaseType, syms.argBaseType),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation methodTypeArgsInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.methodTypeArgs,
            syms.methodTypeArgs,
            List.of(types.makeArrayType(syms.argBaseType)),
            make::Ident
        );
    }

    private JCTree.JCMethodInvocation getGetArgInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "getArg",
            syms.typeArgUtils,
            syms.argBaseType,
            List.of(syms.argBaseType, types.makeArrayType(syms.intType)),
            make::Ident
        );
    }

    private Type.MethodType methodType(Type type) {
        return switch (type) {
            case Type.MethodType methodType -> methodType;
            case Type.ForAll forAll -> forAll.qtype.asMethodType();
            default -> throw new AssertionError("Unhandled type " + type + " (" + type.getClass() + ")");
        };
    }

    private Name computeArgName(Symbol owner) {
        var synChar = target.syntheticNameChar();
        var pkg = owner.packge().fullname.toString().replace('.', synChar);
        var name = owner.getSimpleName().toString();
        var strName = "0" + synChar + "typeArgs" + synChar + pkg + synChar + synChar + name;
        return names.fromString(strName);
    }

    private Name computeMethodArgName(Symbol owner) {
        return depthDependentName(owner, "methodTypeArgs");
    }

    private Name depthDependentName(Symbol owner, String name) {
        var depth = 0;
        var current = owner;
        while (current != null) {
            current = current.owner;
            depth++;
        }
        var strName = Integer.toString(depth) + target.syntheticNameChar() + name;
        return names.fromString(strName);
    }

    private JCTree.JCMethodInvocation internalTypeArgsConstruction(Type type) {
        var call = parameterizedTypeOfInvocation(-1);
        var args = type.getTypeArguments();
        if (args.isEmpty()) { // raw call
            throw new UnsupportedOperationException("Raw call not supported yet");
        }
        call.args = argTypeParam(args);
        call.args = call.args.prepend(
            make.Select(
                make.Ident(type.tsym),
                syms.getClassField(type.tsym.type, types)
            )
        );
        return call;
    }

    private List<JCTree.JCExpression> argTypeParam(List<Type> types) {
        var buffer = new ListBuffer<JCTree.JCExpression>();
        types.forEach(t -> buffer.prepend(generateArgs(null, t)));
        return buffer.toList();
    }

    private JCTree.JCMethodInvocation generateArgs(Type previous, Type current) {
        var res = switch (current) {
            case Type.ArrayType ignored -> { // Foo[]
                var call = arrayTypeOfInvocation(-1);
                var arrayType = (Type.ArrayType) current;
                call.args = List.of(generateArgs(current, arrayType.elemtype));
                yield call;
            }
            case Type.WildcardType wildcardType -> { // <?> / <? extends Foo> / <? super Foo>
                var call = wildcardTypeOfInvocation(wildcardType.kind == BoundKind.SUPER, -1);
                if (!wildcardType.isUnbound()) { // <? extends Foo> / <? super Foo>
                    var bound = wildcardType.isSuperBound() ? wildcardType.getSuperBound() : wildcardType.getExtendsBound();
                    call.args = List.of(generateArgs(current, bound));
                    yield call;
                }
                // <?>
                var ctype = (Type.ClassType) previous;
                var index = ctype.typarams_field.indexOf(current);
                var sym = (Symbol.ClassSymbol) previous.tsym;
                var typeVarSym = sym.getTypeParameters().get(index);
                var bounds = typeVarSym.getBounds();
                var buffer = new ListBuffer<JCTree.JCExpression>();
                bounds.forEach(b -> buffer.add(generateArgs(current, b)));
                call.args = buffer.toList();
                yield call;
            }
            case Type.IntersectionClassType intersectionClassType -> {
                var call = externalMethodInvocation(
                    -1,
                    "of",
                    syms.intersectionTypeArgs,
                    syms.intersectionTypeArgs,
                    List.of(types.makeArrayType(syms.argBaseType)),
                    make::Ident
                );
                var buffer = new ListBuffer<JCTree.JCExpression>();
                intersectionClassType.getComponents().forEach(c -> buffer.add(generateArgs(current, c)));
                call.args = buffer.toList();
                yield call;
            }
            case Type.ClassType ignored -> { // Foo / Foo<E> / Foo(raw)
                var classFieldAcc = make.Select(
                    make.Ident(current.tsym),
                    syms.getClassField(current, types)
                );
                if (current.isRaw()) { // Foo(raw)
                    var rawTypeCall = rawTypeOfInvocation(-1);
                    var parameterizedTypeCall = parameterizedTypeOfInvocation(-1);
                    rawTypeCall.args = List.of(parameterizedTypeCall);
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    buffer.add(classFieldAcc);
                    var ctype = (Type.ClassType) current.tsym.type;
                    ctype.typarams_field.forEach(param -> buffer.add(generateArgs(current, param.getUpperBound())));
                    parameterizedTypeCall.args = buffer.toList();
                    yield rawTypeCall;
                } else if (current.isParameterized()) { // Foo<E>
                    var call = parameterizedTypeOfInvocation(-1);
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    buffer.add(classFieldAcc);
                    var ctype = (Type.ClassType) current;
                    ctype.typarams_field.forEach(param -> buffer.add(generateArgs(
                        current,
                        param
                    )));
                    call.args = buffer.toList();
                    yield call;
                } else { // Foo
                    var call = classTypeOfInvocation(-1);
                    call.args = List.of(classFieldAcc);
                    yield call;
                }
            }
            case Type.CapturedType capturedType -> generateArgs(previous, capturedType.getUpperBound());
            case Type.TypeVar ignored -> {
                var owner = current.tsym.owner;
                var index = owner.type.getTypeArguments().indexOf(current);
                if (owner instanceof Symbol.MethodSymbol methodSymbol) {
                    var name = computeMethodArgName(owner);
                    var symbol = methodSymbol.extraParams
                        .stream()
                        .filter(p -> p.name.equals(name))
                        .findFirst()
                        .orElseThrow();
                    var call = externalMethodInvocation(
                        -1,
                        "arg",
                        syms.methodTypeArgs,
                        syms.argBaseType,
                        List.of(syms.intType),
                        s -> make.Select(make.Ident(symbol), s)
                    );
                    call.args = List.of(make.Literal(index));
                    yield call;
                }
                var name = computeArgName(owner);
                var symbol = resolve.resolveInternalField(
                    new JCDiagnostic.SimpleDiagnosticPosition(-1),
                    env,
                    owner.type,
                    name
                );

                var call = getGetArgInvocation(-1);
                call.args = List.of(
                    make.Ident(symbol),
                    make.Literal(index)
                );
                yield call;
            }
            case Type.JCPrimitiveType primitiveType -> {
                var classFieldAcc = make.Select(
                    make.TypeIdent(primitiveType.getTag()).setType(primitiveType),
                    syms.getClassField(current, types)
                );
                var call = classTypeOfInvocation(-1);
                call.args = List.of(classFieldAcc);
                yield call;
            }
            default -> throw new AssertionError("Unhandled type " + current + " (" + current.getClass() + ")");
        };

        var enclosingType = current.getEnclosingType();
        if (enclosingType != null && !Type.noType.equals(enclosingType)) { // nested class
            var outerRes = generateArgs(current, enclosingType);
            var params = List.<JCTree.JCExpression>of(outerRes, res);
            res = innerClassTypeOfInvocation(-1);
            res.args = params;
        }

        return res;
    }

    private record ArgFieldData(Symbol owner, JCTree.JCVariableDecl field, JCTree.JCTypeApply typeApply) {
    }

    // endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree cdef, TreeMaker make) {
        try {
            this.make = make;
            this.env = env;
//            return cdef;
            return translate(cdef);
        } finally {
            this.make = null;
            this.env = null;
        }
    }

    public void debug(Object... o) {
        if (o.length == 0) {
            throw new AssertionError("No arguments provided");
        }
        var str = Stream.of(o)
            .map(String::valueOf)
            .collect(Collectors.joining(", ", "debug: ", ""));
        log.printRawLines(str);
    }

    // TODO interet value class vs escape analysis
    // TODO smallint jvm

}
