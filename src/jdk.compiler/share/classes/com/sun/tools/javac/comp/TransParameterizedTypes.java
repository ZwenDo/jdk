package com.sun.tools.javac.comp;

import com.sun.source.tree.Tree;
import com.sun.tools.javac.code.BoundKind;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.Types;
import com.sun.tools.javac.jvm.Target;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Names;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

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
    private Env<AttrContext> env = null;

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
        env = Attr.instance(context).env;
    }

    @Override
    public void visitClassDef(JCTree.JCClassDecl tree) {
        // generate the base arg field
        if (!tree.name.contentEquals("Foo")) {
            result = tree;
            return;
        }
        var isClassParameterized = tree.sym.type.isParameterized();
        Map<Symbol, TransParameterizedTypes.ArgFieldData> fields;
        if (isClassParameterized) {
            fields = generateFields(tree);
        } else {
            fields = Map.of();
        }
        rewriteMethods(tree, fields, isClassParameterized);

        result = tree;
    }

    // region Base arg field generation

    private Map<Symbol, ArgFieldData> generateFields(JCTree.JCClassDecl tree) {
        var result = new LinkedHashMap<Symbol, ArgFieldData>();

        // first generate the base field
        var baseField = generateField(tree, tree.sym);
        result.put(tree.sym, new ArgFieldData(baseField, null));

        // then, optionally generate the fields for the super type
        if (tree.extending != null && tree.extending.type.isParameterized() && isParameterizedByClass((JCTree.JCTypeApply) tree.extending)) {
            var extending = (JCTree.JCTypeApply) tree.extending;
            var classExpression = (JCTree.JCIdent) extending.clazz;
            var field = generateField(tree, classExpression.sym);
            result.put(classExpression.sym, new ArgFieldData(field, extending));
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

            var classExpression = (JCTree.JCIdent) apply.clazz;
            var field = generateField(tree, classExpression.sym);
            result.put(classExpression.sym, new ArgFieldData(field, apply));
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
        var fieldName = names.fromString(computeArgName(superType));
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
        tree.defs = tree.defs.append(baseField);
        return baseField;
    }

    private String computeArgName(Symbol owner) {
        var synChar = target.syntheticNameChar();
        var pkg = owner.packge().fullname.toString().replace('.', synChar);
        var name = owner.getSimpleName().toString();
        return "0" + synChar + "typeArgs" + synChar + pkg + synChar + synChar + name;
    }

    // endregion

    private void rewriteMethods(
        JCTree.JCClassDecl tree,
        Map<Symbol, ArgFieldData> fields,
        boolean isClassParameterized
    ) {
        for (var member : tree.defs) {
            if (member.getKind() != Tree.Kind.METHOD) {
                continue;
            }
            var method = (JCTree.JCMethodDecl) member;

            if (method.getName().equals(names.init)) {
                if (isClassParameterized) {
                    rewriteConstructor(tree, method, fields);
                }
            } else {
                rewriteBasicMethod(tree, method, fields, isClassParameterized);
            }

        }
    }

    // region Constructor rewriting

    private void rewriteConstructor(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl method,
        Map<Symbol, ArgFieldData> fields
    ) {
        var copy = copyMethod(method);
        prependArgToMethodParams(method, syms.argBaseType, m -> m.type.asMethodType());

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
        var doesCallOverload = doesCallConstructor(method);

        // if so, we just need to add the extra argument to the call of the other constructor
        if (doesCallOverload) {
            appendArgToThisCall(method);
        } else { // otherwise, this constructor must set the base field
            setFieldsValues(tree, method, fields);
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
        var visitor = new MethodTypeApplier(t -> t.argtypes = modified.sym.type.getTypeArguments());
        copy.type.accept(visitor, null);
//        TODO uncomment
//        copy.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
//        copy.sym.flags_field |= Flags.SYNTHETIC | Flags.MANDATED;

        copy.body = make.at(modified.pos)
            .Block(
                0L,
                List.of(
                    make.Exec(
                        make.Apply(
                            baseParamTypeArgsCopy,
                            callToBaseMethod,
                            params.toList()
                        ).setType(syms.voidType)
                    )
                )
            );

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
        params.forEach(parameter -> buffer.add(generateVarSymbolTypes(params, null, parameter)));
        return buffer.toList();
    }

    private JCTree.JCMethodInvocation generateVarSymbolTypes(
        List<Symbol.TypeVariableSymbol> base,
        Type previous,
        Symbol.TypeVariableSymbol parameter
    ) {
        var bounds = parameter.getBounds();
        if (bounds.size() == 1) {
            var bound = bounds.head;
            return generateType(base, previous, bound);
        } else {
            var intersectionBuffer = new ListBuffer<JCTree.JCExpression>();
            bounds.forEach(current -> intersectionBuffer.add(generateType(base, previous, current)));
            var call = externalMethodInvocation(
                -1,
                "of",
                syms.intersectionTypeArgs,
                syms.intersectionTypeArgs,
                List.of(types.makeArrayType(syms.argBaseType))
            );
            call.args = intersectionBuffer.toList();
            return call;
        }
    }

    private JCTree.JCMethodInvocation generateType(List<Symbol.TypeVariableSymbol> base, Type previous, Type current) {
        var res = switch (current.getTag()) {
            case ARRAY -> { // Foo[]
                var call = externalMethodInvocation(
                    -1,
                    "of",
                    syms.arrayTypeArgs,
                    syms.arraysType,
                    List.of(syms.argBaseType)
                );
                var arrayType = (Type.ArrayType) current;
                call.args = List.of(generateType(base, current, arrayType.elemtype));
                yield call;
            }
            case WILDCARD -> { // ? / ? extends Foo / ? super Foo
                var wildcardType = (Type.WildcardType) current;
                var callName = wildcardType.kind == BoundKind.SUPER ? "ofLower" : "ofUpper";
                var call = externalMethodInvocation(
                    -1,
                    callName,
                    syms.wildcardTypeArgs,
                    syms.wildcardTypeArgs,
                    List.of(syms.argBaseType)
                );
                Type bound;
                if (wildcardType.isUnbound()) {
                    // TODO fetch the default upper bound
                    var index = previous.allparams().indexOf(current);
                    if (true) {
                        var sym = (Symbol.ClassSymbol) previous.tsym;
                        var symbol = sym.getTypeParameters().get(index);
                        throw new AssertionError(sym);
                    }
                    bound = null;
                } else {
                    bound = wildcardType.isSuperBound() ? wildcardType.getSuperBound() : wildcardType.getExtendsBound();
                }
                call.args = List.of(generateType(base, current, bound));
                yield call;
            }
            case CLASS -> { // Foo / Foo<E> / Foo(raw)
                var classFieldAcc = make.Select(
                    make.Type(current),
                    syms.getClassField(current, types)
                );
                if (current.isRaw()) { // raw type
                    var rawTypeCall = rawTypeOfInvocation(-1);
                    var parameterizedTypeCall = parameterizedTypeOfInvocation(-1);
                    rawTypeCall.args = List.of(parameterizedTypeCall);
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    buffer.add(classFieldAcc);
                    current.tsym.type.allparams().forEach(param -> {
                        buffer.add(generateType(base, current, param.getUpperBound()));
                    });
                    parameterizedTypeCall.args = buffer.toList();
                    yield rawTypeCall;
                } else if (current.isParameterized()) { // Foo<E>
                    var call = parameterizedTypeOfInvocation(-1);
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    buffer.add(classFieldAcc);
                    current.allparams().forEach(param -> buffer.add(generateType(base, current, param)));
                    call.args = buffer.toList();
                    yield call;
                } else { // Foo
                    var call = classTypeOfInvocation(-1);
                    call.args = List.of(classFieldAcc);
                    yield call;
                }
            }
            case TYPEVAR -> {
                var name = current.tsym.name;
                var param = base
                    .stream()
                    .filter(p -> name.equals(p.name))
                    .findFirst()
                    .orElseThrow(() -> new AssertionError("Type variable " + name + " not found in " + base + " " + current));
                yield generateVarSymbolTypes(base, current, param);
            }
            default -> throw new AssertionError("Unhandled type " + current + " (" + current.getClass() + ")");
        };

        var enclosingType = current.getEnclosingType();
        if (enclosingType != null && !Type.noType.equals(enclosingType)) { // nested class
            var outerRes = generateType(base, current, enclosingType);
            var params = List.<JCTree.JCExpression>of(outerRes, res);
            res = externalMethodInvocation(
                -1,
                "of",
                syms.innerClassTypeArgs,
                syms.innerClassTypeArgs,
                List.of(syms.argBaseType, syms.argBaseType)
            );
            res.args = params;
        }

        return res;
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
        var symbolCopy = new Symbol.MethodSymbol(
            baseMethod.sym.flags(),
            baseMethod.sym.name,
            baseMethod.sym.type,
            baseMethod.sym.owner
        );
        symbolCopy.params = baseMethod.sym.params;
        symbolCopy.extraParams = baseMethod.sym.extraParams;
        symbolCopy.capturedLocals = baseMethod.sym.capturedLocals;
        symbolCopy.defaultValue = baseMethod.sym.defaultValue;
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

    private void prependArgToMethodParams(
        JCTree.JCMethodDecl method,
        Type argType,
        Function<JCTree.JCMethodDecl, Type.MethodType> methodTypeAccessor
    ) {
        var oldPos = method.pos;
        var extraArgParam = make.at(method.pos).Param(
            names.fromString("0" + target.syntheticNameChar() + "arg"),
            argType,
            method.sym
        );
        extraArgParam.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
        make.at(oldPos);

        method.params = method.params.prepend(extraArgParam);
        var mtype = methodTypeAccessor.apply(method);
        mtype.argtypes = mtype.argtypes.prepend(extraArgParam.type);
        method.sym.extraParams = method.sym.extraParams.prepend(extraArgParam.sym);
    }

    /**
     * This method checks if the first instruction of the constructor is a call to another constructor.
     */
    private boolean doesCallConstructor(JCTree.JCMethodDecl constructor) {
        var instructions = constructor.body.stats;
        if (instructions.isEmpty()) { // empty constructor (theoretically impossible because it at least contains super)
            return false;
        }
        var firstInstruction = instructions.getFirst();
        if (!(firstInstruction instanceof JCTree.JCExpressionStatement expressionStatement)) {
            return false;
        }
        var expression = expressionStatement.expr;
        if (!(expression instanceof JCTree.JCMethodInvocation call)) {
            return false;
        }
        var called = call.meth;
        if (!(called instanceof JCTree.JCIdent identifier)) {
            return false;
        }
        var calledName = identifier.name;
        return names._this.equals(calledName);
    }

    private void appendArgToThisCall(JCTree.JCMethodDecl constructor) {
        var instructions = constructor.body.stats;
        var call = (JCTree.JCMethodInvocation) ((JCTree.JCExpressionStatement) instructions.getFirst()).expr;
        var args = call.args;
        call.args = args.prepend(make.Ident(constructor.params.getFirst()));
    }

    private void setFieldsValues(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl constructor,
        Map<Symbol, ArgFieldData> fields
    ) {
        var instructions = constructor.body.stats;

        var buffer = new ListBuffer<JCTree.JCStatement>();
        buffer.addAll(instructions);

        var baseArg = constructor.params.getFirst().sym;
        fields.forEach((sym, data) -> {
            if (tree.sym.equals(sym)) { // base field
                var assign = make.Assign(
                    make.Ident(data.field().sym),
                    make.Ident(baseArg)
                );
                buffer.prepend(make.Exec(assign));
                return;
            }

            var call = createSuperFromConcrete(tree, baseArg, data);
            var assign = make.Assign(make.Ident(data.field().sym), call);
            buffer.prepend(make.Exec(assign));
        });

        // we set the field value before calling the super constructor as it is now allowed
        constructor.body.stats = buffer.toList();
    }

    private JCTree.JCMethodInvocation createSuperFromConcrete(
        JCTree.JCClassDecl tree,
        Symbol baseArg,
        ArgFieldData fieldData
    ) {
        var root = parameterizedTypeOfInvocation(-1);
        var args = new ListBuffer<JCTree.JCExpression>();
        var clazz = fieldData.typeApply().clazz;
        args.add(
            make.Select(
                make.Type(clazz.type),
                syms.getClassField(clazz.type, types)
            )
        );
        createSuperArgObject(root, args, tree, baseArg, fieldData.typeApply());

        root.args = args.toList();
        return root;
    }

    private void createSuperArgObject(
        JCTree.JCMethodInvocation call,
        ListBuffer<JCTree.JCExpression> args,
        JCTree.JCClassDecl tree,
        Symbol baseArg,
        JCTree.JCTypeApply fieldData
    ) {
        fieldData.getTypeArguments().forEach(type -> {
            if (type instanceof JCTree.JCIdent id) {
                if (id.sym instanceof Symbol.TypeVariableSymbol symb) { // if param is parameterized by the class
                    var name = symb.name;
                    var index = 0;
                    for (var param : tree.getTypeParameters()) {
                        if (name.equals(param.name)) {
                            break;
                        }
                        index++;
                    }
                    var getArgCall = externalMethodInvocation(
                        -1,
                        "getArg",
                        syms.typeArgUtils,
                        syms.argBaseType,
                        List.of(syms.argBaseType, types.makeArrayType(syms.intType))
                    );
                    getArgCall.args = List.of(make.Ident(baseArg), make.Literal(index));
                    args.add(getArgCall);
                } else {
                    var clazz = id.sym;
                    var classOfCall = classTypeOfInvocation(-1);
                    classOfCall.args = List.of(
                        make.Select(
                            make.Ident(clazz),
                            syms.getClassField(clazz.type, types)
                        ));
                    args.add(classOfCall);
                }
            } else if (type instanceof JCTree.JCTypeApply apply) { // or if it contains a type parameterized by the class
                var nestedCall = parameterizedTypeOfInvocation(-1);
                var methodArgs = new ListBuffer<JCTree.JCExpression>();
                var clazz = ((JCTree.JCIdent) apply.clazz).sym;
                methodArgs.add(
                    make.Select(
                        make.Ident(clazz),
                        syms.getClassField(clazz.type, types)
                    )
                );
                createSuperArgObject(nestedCall, methodArgs, tree, baseArg, apply);
                nestedCall.args = methodArgs.toList();
                args.add(nestedCall);
            }
        });

        call.args = args.toList();
    }

    // endregion

    // region Basic method rewriting

    private void rewriteBasicMethod(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl method,
        Map<Symbol, ArgFieldData> fields,
        boolean isClassParameterized
    ) {
        var isMethodParameterized = !method.typarams.isEmpty();
        if (!isMethodParameterized && !isClassParameterized) {
            return;
        }

        if (isMethodParameterized) {
            var copy = copyMethod(method);
            prependArgToMethodParams(method, syms.methodTypeArgs, m -> ((Type.ForAll) m.type).qtype.asMethodType());
            backwardCompatibilityOverloadMethod(
                tree,
                method,
                copy,
                make.Ident(method.sym),
                () -> methodDefaultTypeArgs(method)
            );
        }
    }

    private JCTree.JCExpression methodDefaultTypeArgs(JCTree.JCMethodDecl method) {
        var call = externalMethodInvocation(
            -1,
            "of",
            syms.methodTypeArgs,
            syms.methodTypeArgs,
            List.of(types.makeArrayType(syms.argBaseType))
        );
        call.args = defaultParams(method.sym.getTypeParameters());
        return call;
    }


    // endregion

    // region utils

    private JCTree.JCMethodInvocation externalMethodInvocation(
        int pos,
        String name,
        Type ownerType,
        Type returnType,
        List<Type> paramTypes
    ) {
        var methodName = names.fromString(name);
        var ident = make.Ident(methodName);
        var methodCall = make.Apply(
            List.nil(),
            ident,
            List.nil()
        ).setType(returnType);
        var sym = resolve.resolveInternalMethod(
            new JCDiagnostic.SimpleDiagnosticPosition(pos),
            env,
            ownerType,
            methodName,
            paramTypes,
            List.nil()
        );
        ident.sym = sym;
        if (sym.isVarArgs()) {
            methodCall.varargsElement = ((Type.ArrayType) sym.params.last().type).elemtype;
        }
        ident.type = ident.sym.type;
        return methodCall;
    }

    private JCTree.JCMethodInvocation rawTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.rawTypeTypeArgs,
            syms.rawTypeTypeArgs,
            List.of(syms.parameterizedTypeTypeArgs)
        );
    }

    private JCTree.JCMethodInvocation parameterizedTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.parameterizedTypeTypeArgs,
            syms.argBaseType,
            List.of(syms.classType, types.makeArrayType(syms.argBaseType))
        );
    }

    private JCTree.JCMethodInvocation classTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.classTypeArgs,
            syms.classTypeArgs,
            List.of(syms.classType)
        );
    }

    private record ArgFieldData(JCTree.JCVariableDecl field, JCTree.JCTypeApply typeApply) {
    }

    private static class MethodTypeApplier extends Types.DefaultTypeVisitor<Void, Void> {
        private final Consumer<Type.MethodType> action;


        private MethodTypeApplier(Consumer<Type.MethodType> action) {
            this.action = action;
        }

        @Override
        public Void visitForAll(Type.ForAll t, Void unused) {
            return visitMethodType(t.qtype.asMethodType(), unused);
        }

        @Override
        public Void visitMethodType(Type.MethodType t, Void unused) {
            action.accept(t);
            return null;
        }

        @Override
        public Void visitType(Type t, Void unused) {
            throw new AssertionError("Unexpected type " + t);
        }
    }

    // endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree cdef, TreeMaker make) {
        try {
            this.make = make;
            this.env = env;
            return translate(cdef);
        } finally {
            this.make = null;
            this.env = null;
        }
    }

}
