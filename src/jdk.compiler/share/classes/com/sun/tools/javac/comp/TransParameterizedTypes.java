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
    private Env<AttrContext> env = null;
    private final ParameterizedMethodCallVisitor parameterizedMethodCallVisitor;

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
        parameterizedMethodCallVisitor = new ParameterizedMethodCallVisitor();
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
//        can not uncomment this because Resolve.resolveInternalField ignores synthetic fields
//        baseField.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
//        baseField.sym.flags_field |= Flags.SYNTHETIC | Flags.MANDATED;

        tree.sym.members_field.enter(fieldSym);
        tree.defs = tree.defs.append(baseField);
        return baseField;
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

            if (TreeInfo.isConstructor(member)) {
                if (isClassParameterized) { // TODO visit anyway but only work with field if this is true (need to visit the body in any case)
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
        var mtype = methodType(copy.type);
        mtype.argtypes = modified.sym.type.getTypeArguments();
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

//    private JCTree.JCMethodInvocation generateVarSymbolTypes(
//        List<Symbol.TypeVariableSymbol> base,
//        Type previous,
//        Symbol.TypeVariableSymbol parameter
//    ) {
//        var bounds = parameter.getBounds();
//        if (bounds.size() == 1) {
//            var bound = bounds.head;
//            return generateType(base, previous, bound);
//        } else {
//            var intersectionBuffer = new ListBuffer<JCTree.JCExpression>();
//            bounds.forEach(current -> intersectionBuffer.add(generateType(base, previous, current)));
//            var call = externalMethodInvocation(
//                -1,
//                "of",
//                syms.intersectionTypeArgs,
//                syms.intersectionTypeArgs,
//                List.of(types.makeArrayType(syms.argBaseType))
//            );
//            call.args = intersectionBuffer.toList();
//            return call;
//        }
//    }

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
            computeMethodArgName(method.sym),
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
                    for (var param : tree.getTypeParameters()) { // this could be changed to a Map<Name, Integer>
                        if (name.equals(param.name)) {
                            break;
                        }
                        index++;
                    }
                    var getArgCall = getGetArgInvocation(-1);
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

        if (isMethodParameterized) {
            var copy = copyMethod(method);
            prependArgToMethodParams(method, syms.methodTypeArgs, m -> ((Type.ForAll) m.type).qtype.asMethodType());
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

    private class ParameterizedMethodCallVisitor extends TreeTranslator {

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            super.visitApply(tree);

            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) {
                throw new AssertionError("No symbol for " + tree.meth);
            }

            if (sym.type.getTypeArguments().isEmpty() || ((sym.owner.flags_field & Flags.NEW_GENERICS) == 0)) {
                return;
            }

            var methodType = tree.meth.type.asMethodType();
            var call = methodTypeArgsInvocation(-1);
            call.args = argTypeParam(methodType.inferredTypes);
            // TODO not sure about this
            var t = tree.meth.type.asMethodType();
            t.argtypes = t.argtypes.prepend(call.type);

            tree.args = tree.args.prepend(call);
        }

        @Override
        public void visitNewClass(JCTree.JCNewClass tree) {
            super.visitNewClass(tree);

            var sym = (Symbol.MethodSymbol) tree.constructor;
            var clazz = sym.owner;

            if (clazz.type.getTypeArguments().isEmpty() || ((clazz.flags_field & Flags.NEW_GENERICS) == 0)) {
                return;
            }

            var cl = (Type.ClassType) tree.type;
            var call = parameterizedTypeOfInvocation(-1);
            var args = cl.getTypeArguments();
            if (args.isEmpty()) { // raw call
                throw new UnsupportedOperationException("Raw call not supported yet");
            }
            call.args = argTypeParam(args);
            call.args = call.args.prepend(
                make.Select(
                    make.Ident(clazz),
                    syms.getClassField(clazz.type, types)
                )
            );
            var methodType = tree.constructorType.asMethodType();
            if (!syms.argBaseType.equals(methodType.argtypes.head)) {
                methodType.argtypes = methodType.argtypes.prepend(syms.argBaseType);
            }

            tree.args = tree.args.prepend(call);
        }

        @Override
        public void visitNewArray(JCTree.JCNewArray tree) {
            super.visitNewArray(tree);
        }

        private List<JCTree.JCExpression> argTypeParam(List<Type> types) {
            var buffer = new ListBuffer<JCTree.JCExpression>();
            types.forEach(t -> buffer.prepend(generateArgs(null, t)));
            return buffer.toList();
        }

        private JCTree.JCMethodInvocation generateArgs(Type previous, Type current) {
            var res = switch (current.getTag()) {
                case ARRAY -> { // Foo[]
                    var call = arrayTypeOfInvocation(-1);
                    var arrayType = (Type.ArrayType) current;
                    call.args = List.of(generateArgs(current, arrayType.elemtype));
                    yield call;
                }
                case WILDCARD -> { // <?> / <? extends Foo> / <? super Foo>
                    var wildcardType = (Type.WildcardType) current;
                    var call = wildcardTypeOfInvocation(wildcardType.kind == BoundKind.SUPER, -1);
                    if (!wildcardType.isUnbound()) { // <? extends Foo> / <? super Foo>
                        var bound = wildcardType.isSuperBound() ? wildcardType.getSuperBound() : wildcardType.getExtendsBound();
                        call.args = List.of(generateArgs(current, bound));
                        yield call;
                    }
                    // <?>
                    var index = previous.allparams().indexOf(current);
                    var sym = (Symbol.ClassSymbol) previous.tsym;
                    var typeVarSym = sym.getTypeParameters().get(index);
                    var bounds = typeVarSym.getBounds();
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    bounds.forEach(b -> buffer.add(generateArgs(current, b)));
                    call.args = buffer.toList();
                    yield call;
                }
                case CLASS -> { // Foo / Foo<E> / Foo(raw)
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
                        current.tsym.type.allparams().forEach(param -> {
                            buffer.add(generateArgs(current, param.getUpperBound()));
                        });
                        parameterizedTypeCall.args = buffer.toList();
                        yield rawTypeCall;
                    } else if (current.isParameterized()) { // Foo<E>
                        var call = parameterizedTypeOfInvocation(-1);
                        var buffer = new ListBuffer<JCTree.JCExpression>();
                        buffer.add(classFieldAcc);
                        current.allparams().forEach(param -> buffer.add(generateArgs(
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
                case TYPEVAR -> {
                    var owner = current.tsym.owner;
                    var index = owner.type.getTypeArguments().indexOf(current);
                    yield switch (owner) {
                        case Symbol.MethodSymbol methodSymbol -> {
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
                        case Symbol.ClassSymbol ignored -> {
                            var name = computeArgName(owner);
                            var symbol = resolve.resolveInternalField(
                                new JCDiagnostic.SimpleDiagnosticPosition(-1),
                                env,
                                owner.type,
                                name
                            );

                            var call = getGetArgInvocation(-1);
                            call.args = List.of(make.Ident(symbol), make.Literal(index));
                            yield call;
                        }
                        default -> throw new AssertionError("Unhandled type " + owner + " (" + owner.getClass() + ")");
                    };
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

        public void visitMethod(JCTree.JCMethodDecl method) {
            method.body.accept(this);
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

    public Type.MethodType methodType(Type type) {
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
        var depth = 0;
        var current = owner;
        while (current != null) {
            current = current.owner;
            depth++;
        }
        var synChar = target.syntheticNameChar();
        var strName = depth + synChar + "methodTypeArgs";
        return names.fromString(strName);
    }


    private record ArgFieldData(JCTree.JCVariableDecl field, JCTree.JCTypeApply typeApply) {
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

    private void debug(Object... o) {
        var str = Stream.of(o)
            .map(String::valueOf)
            .collect(Collectors.joining(", "));
        log.rawWarning(-1, str);
    }

}
