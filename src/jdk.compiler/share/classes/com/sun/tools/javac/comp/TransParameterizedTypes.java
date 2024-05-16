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
import com.sun.tools.javac.tree.TreeScanner;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;
import com.sun.tools.javac.util.Pair;

import java.util.ArrayList;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class TransParameterizedTypes extends TreeTranslator {

    //region fields
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

    private JCTreeCopier treeCopier;
    private Env<AttrContext> env = null;
    private Symbol.ClassSymbol currentClass = null;
    private JCTree.JCClassDecl currentClassTree = null;
    private ArrayList<Pair<JCTree, Symbol>> generatedDecl = null;

    /**
     * NOTE: The base arg field is always the first element of the list.
     */
    private ArrayList<Pair<JCTree.JCTypeApply, JCTree.JCVariableDecl>> currentClassArgFields = null;

    /**
     * Iterated over in reverse order to find the first parameterized type that matches the name
     *
     * <pre>
     * class Foo&#60;T&#62; {
     *
     *     class Bar&#60;T&#62; { // shadowing of Foo T
     *         class Baz implements Consumer&#60;T&#62; { // uses Bar's T
     *             // ...
     *
     *             public &#60;T&#62; void myMethod(T t) { // shadowing of Bar's T
     *             }
     *         }
     *     }
     * }
     * </pre>
     * <p>
     * In this example we need this list to be able to find the correct T when building the Arg field of Baz or use the
     * correct T in 'myMethod'.
     */
    private List<ScopeTypeParameter> inScopeBaseTypeParams = null;

    /**
     * Fields used for overload constructor generation.
     * <pre>
     * class Foo&#60;T&#62; {
     *     Foo(T t) {
     *         this(t, null);
     *     }
     *     ...
     * }
     * </pre>
     */
    private Symbol enclosingArgParamDecl = null;
    private Symbol enclosingMethodTypeArgsParamDecl = null;
    /**
     * Field containing all the fields and blocks that are declared in the current class. These declarations must be
     * moved to the constructor to be able to access the method parameters.
     */
    private ArrayList<JCTree.JCStatement> inlineAndBlockDecls = null;
    //endregion

    //region instantiation

    /**
     * Get the instance for this context.
     */
    public static TransParameterizedTypes instance(Context context) {
        var instance = context.get(typeReifierKey);
        if (instance == null) instance = new TransParameterizedTypes(context);
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
        parameterizedMethodCallVisitor = new InstructionVisitor();
    }
    //endregion

    /**
     * this method is only called for the top-level class
     */
    @Override
    public void visitClassDef(JCTree.JCClassDecl tree) {
        result = tree;
        if (!tree.sym.hasNewGenerics()) return;
        try {
            inScopeBaseTypeParams = List.nil();
            rewriteClass(tree);
        } catch (Throwable t) {
            debug("error in class: " + tree.sym.fullname);
            throw t;
        } finally {
            inScopeBaseTypeParams = null;
        }
    }

    // TODO also handle parameterized classes that do not have any constructor;
    private void rewriteClass(JCTree.JCClassDecl tree) {
        var oldClass = currentClass;
        var oldClassTree = currentClassTree;
        var oldCurrentClassArgFields = currentClassArgFields;
        var oldInScopeBaseTypeParams = inScopeBaseTypeParams;
        var oldGeneratedDecl = generatedDecl;
        var oldInlineAndBlockDecls = inlineAndBlockDecls;

        var isClassParameterized = tree.sym.type.getTypeArguments().nonEmpty();
        try {
            generatedDecl = new ArrayList<>();
            currentClass = tree.sym;
            inlineAndBlockDecls = new ArrayList<>();
            if (currentClass.isStatic()) { // static classes cannot access the type of the outer classes
                inScopeBaseTypeParams = List.nil();
            }
            currentClassTree = tree;
            if (isClassParameterized) {
                var params = tree.getTypeParameters().stream().map(t -> t.name).toList();
                IntFunction<JCTree.JCMethodInvocation> argAccessFactory;
                if (currentClass.isInterface()) {
                    currentClassArgFields = new ArrayList<>();
                    argAccessFactory = interfaceArgHandle(currentClass);
                } else {
                    // if the class is not an interface, we generate the fields for the type parameters
                    currentClassArgFields = generateFields(tree);
                    var classBaseField = currentClassArgFields.getFirst().snd;
                    argAccessFactory = concreteClassArgHandle(classBaseField);
                }
                var scopeTypeParameter = new ScopeTypeParameter(params, argAccessFactory);
                inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
            } else {
                currentClassArgFields = new ArrayList<>();
            }

            rewriteDefs(tree);

            // once we have done rewriting the current class, we apply the modifiers
            currentClassArgFields.forEach(field -> {
                field.snd.mods.flags |= Flags.SYNTHETIC;
                field.snd.sym.flags_field |= Flags.SYNTHETIC;
            });
            generatedDecl.forEach(pair -> {
                tree.sym.members_field.enter(pair.snd);
                tree.defs = tree.defs.prepend(pair.fst);
            });
        } finally {
            currentClass = oldClass;
            currentClassTree = oldClassTree;
            currentClassArgFields = oldCurrentClassArgFields;
            inScopeBaseTypeParams = oldInScopeBaseTypeParams;
            generatedDecl = oldGeneratedDecl;
            inlineAndBlockDecls = oldInlineAndBlockDecls;
        }
    }

    //region fields generation
    private ArrayList<Pair<JCTree.JCTypeApply, JCTree.JCVariableDecl>> generateFields(JCTree.JCClassDecl tree) {
        var result = new ArrayList<Pair<JCTree.JCTypeApply, JCTree.JCVariableDecl>>();

        // first generate the base field
        var baseField = generateField(tree, tree.sym);
        result.add(Pair.of(null, baseField));

        // then, optionally generate the fields for the super type
        if (tree.extending != null && tree.extending.type.isParameterized() && isParameterizedByClass((JCTree.JCTypeApply) tree.extending)) {
            var extending = (JCTree.JCTypeApply) tree.extending;
            var sym = (Symbol.ClassSymbol) extending.clazz.type.tsym;
            var field = generateField(tree, sym);
            result.add(Pair.of(extending, field));
        }

        // finally generate the fields for the implementing types
        tree.implementing.forEach(expression -> {
            if (!expression.type.isParameterized()) { // if the type is not parameterized, no need to generate a field
                return;
            }
            var apply = (JCTree.JCTypeApply) expression;
            if (!isParameterizedByClass(apply)) { // if the type is not parameterized by the class, no need to generate a field
                return;
            }
            var field = generateField(tree, (Symbol.ClassSymbol) apply.clazz.type.tsym);
            result.add(Pair.of(apply, field));
        });

        return result;
    }

    private JCTree.JCVariableDecl generateField(JCTree.JCClassDecl tree, Symbol.ClassSymbol superType) {
        var fieldFlags = Flags.PRIVATE | Flags.FINAL | Flags.TRANSIENT;
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

        generatedDecl.add(Pair.of(baseField, fieldSym));
        return baseField;
    }
    //endregion

    private void rewriteDefs(JCTree.JCClassDecl tree) {
        // only filter if the class is parameterized
        if (currentClassTree.type.getTypeArguments().nonEmpty()) {
            // first, we filter out all the fields and blocks to avoid visiting them twice
            var filteredDefs = new ListBuffer<JCTree>();
            tree.defs.forEach(member -> {
                switch (member.getTag()) {
                    case VARDEF -> { // for field, we only filter out the INIT part
                        var field = (JCTree.JCVariableDecl) member;
                        filteredDefs.add(field);
                        if (!field.sym.isStatic() && field.init != null) {
                            inlineAndBlockDecls.add(make.Assignment(field.sym, field.init));
                            field.init = null;
                        }
                    }
                    case BLOCK -> {
                        var block = (JCTree.JCBlock) member;
                        if ((block.flags & Flags.STATIC) != 0) { // static blocks are ok
                            filteredDefs.add(block);
                        } else {
                            inlineAndBlockDecls.add(block);
                        }
                    }
                    default -> filteredDefs.add(member);
                }
            });
            tree.defs = filteredDefs.toList();
        }

        tree.defs.forEach(member -> {
            switch (member.getTag()) {
                case METHODDEF -> {
                    var method = (JCTree.JCMethodDecl) member;

                    if (TreeInfo.isConstructor(member)) {
                        rewriteConstructor(method);
                    } else {
                        rewriteBasicMethod(method);
                    }
                }
                case CLASSDEF -> rewriteClass((JCTree.JCClassDecl) member);

                // vardef and blocks here can only be fields without initialization and static blocks
                case VARDEF -> parameterizedMethodCallVisitor.visitField((JCTree.JCVariableDecl) member);
                case BLOCK -> parameterizedMethodCallVisitor.visitClassBlock((JCTree.JCBlock) member);

                default -> throw new AssertionError("Unexpected member type: " + member.getTag());
            }
        });
    }

    private void rewriteConstructor(JCTree.JCMethodDecl tree) {
        var positions = types.typeArgPositions(tree.sym).fitEnumConstructorPositionsDecl(tree.params, syms);
        var oldEnclosingArgParamDecl = enclosingArgParamDecl;
        var oldEnclosingMethodTypeArgsParamDecl = enclosingMethodTypeArgsParamDecl;
        if (!positions.hasNoArg()) {
            // generate backward compatibility overload only if method is not private or if the class is not anonymous
            // because anonymous classes constructors cannot be called anyway
            if (!currentClass.isEnum() && !currentClass.isAnonymous() && (tree.mods.flags & Flags.PRIVATE) == 0) {
                insertArgsInMethod(tree, positions);
                var copy = copyMethod(tree, positions);
                generateBackwardCompatibilityOverload(tree, copy, positions, kind -> switch (kind) {
                    case ARG -> createDefaultTypeArgs(-1);
                    case METHOD_TYPE_ARG -> {
                        var call = methodTypeArgsInvocation(-1);
                        call.args = defaultParams(tree.sym.getTypeParameters());
                        yield call;
                    }
                });
            } else {
                insertArgsInMethod(tree, positions);
            }

            // we need to shadow the class scope type parameters to only use the arg param and not potentially non
            // initialized fields
            if (positions.hasArg()) {
                enclosingArgParamDecl = tree.params.get(positions.argPosition()).sym;
                var scopeTypeParameter = new ScopeTypeParameter(
                    currentClassTree.getTypeParameters().stream().map(t -> t.name).toList(),
                    constructorArgHandle(tree.sym, positions)
                );
                inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
            } else {
                enclosingArgParamDecl = null;
            }
            // we prepend the method type parameters to the scope AFTER the class type parameters because the method
            // type parameters has the precedence over the class type parameters
            if (positions.hasMethodTypeArg()) { // TODO make it work with anonymous classes
                enclosingMethodTypeArgsParamDecl = tree.params.get(positions.methodTypeArgPosition()).sym;
                var scopeTypeParameter = new ScopeTypeParameter(
                    tree.typarams.stream().map(t -> t.name).toList(),
                    basicMethodMArgHandle(tree.sym, positions)
                );
                inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
            } else {
                enclosingMethodTypeArgsParamDecl = null;
            }
            // we then check whether the constructor calls another constructor
            var doesCallOverload = TreeInfo.hasConstructorCall(tree, names._this);
            if (!doesCallOverload && !inlineAndBlockDecls.isEmpty()) {
                var superCall = TreeInfo.findConstructorCall(tree);
                if (superCall != null) { // if there is a superCall, we must insert instruction right after it
                    var buffer = new ListBuffer<JCTree.JCStatement>();
                    var bodyIterator = tree.body.stats.iterator();
                    while (bodyIterator.hasNext()) {
                        var next = bodyIterator.next();
                        buffer.add(next);
                        if ((next instanceof JCTree.JCExpressionStatement expr) && expr.expr == superCall) {
                            break;
                        }
                    }
                    inlineAndBlockDecls.forEach(e -> buffer.add(treeCopier.translate(e)));
                    bodyIterator.forEachRemaining(buffer::add);
                    tree.body.stats = buffer.toList();
                } else { // otherwise we can just prepend the instructions
                    for (var i = inlineAndBlockDecls.size() - 1; i >= 0; i--) {
                        tree.body.stats = tree.body.stats.prepend(treeCopier.translate(inlineAndBlockDecls.get(i)));
                    }
                }
            }
            if (!currentClass.isAnonymous() && positions.hasArg() && !doesCallOverload) {
                setFieldsValues(tree);
            }
        } else {
            inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(ScopeTypeParameter.EMPTY);
        }

        parameterizedMethodCallVisitor.visitMethod(tree);
        inScopeBaseTypeParams = inScopeBaseTypeParams.tail;
        enclosingArgParamDecl = oldEnclosingArgParamDecl;
        enclosingMethodTypeArgsParamDecl = oldEnclosingMethodTypeArgsParamDecl;
    }


    private void generateBackwardCompatibilityOverload(
        JCTree.JCMethodDecl method,
        JCTree.JCMethodDecl copy,
        Types.ArgPosition position,
        Function<ArgKind, JCTree.JCExpression> defaultParamsFactory
    ) {
        // create a list of idents for the args of the overload call // this(a, foo, b)
        var params = new ListBuffer<JCTree.JCExpression>();
        copy.params.forEach(p -> params.add(make.Ident(p)));

        var actualParams = insertAtArgPositions(params.toList(), position, defaultParamsFactory);

        var typeArguments = method.sym.type.getTypeArguments();
        var resType = method.type.asMethodType().restype;
        var sym = method.sym.clone(method.sym.owner);
        sym.type = copyTypeAndInsertTypeArgIfNeeded(sym.type, position);
        var callIdent = make.Ident(sym);

        if (names.init == method.sym.name) {
            callIdent.name = names._this;
        }
        var call = make.Apply(
            make.Types(typeArguments),
            callIdent,
            actualParams
        ).setType(method.type.asMethodType().restype);

        var result = types.isSameType(syms.voidType, resType) ? make.Exec(call) : make.Return(call);

        copy.body = make.at(method.pos).Block(0L, List.of(result));
        generatedDecl.add(Pair.of(copy, copy.sym));
    }

    private JCTree.JCExpression createDefaultTypeArgs(int pos) {
        var rawTypeCall = rawTypeOfInvocation(pos);
        rawTypeCall.args = List.of(make.ClassLiteral(currentClass));
        return rawTypeCall;
    }

    /**
     * Generate a list of erased types representing the type arguments of a method.
     */
    private List<JCTree.JCExpression> defaultParams(List<Symbol.TypeVariableSymbol> params) {
        var buffer = new ListBuffer<JCTree.JCExpression>();
        params.forEach(parameter -> {
            var bounds = parameter.getBounds();
            types.erasure(bounds).forEach(t -> {
                var c = classTypeOfInvocation(-1);
                c.args = List.of(make.ClassLiteral((Symbol.ClassSymbol) t.tsym));
                buffer.append(c);
            });
        });
        return buffer.toList();
    }

    private Type copyMethodType(Type methodType) {
        var baseType = new Type.MethodType(
            methodType.getParameterTypes(),
            methodType.getReturnType(),
            methodType.getThrownTypes(),
            methodType.tsym
        );
        baseType.inferredTypes = methodType.asMethodType().inferredTypes;
        if (methodType.getTypeArguments().isEmpty()) {
            return baseType;
        }
        return new Type.ForAll(methodType.getTypeArguments(), baseType);
    }

    private void insertArgsInMethod(JCTree.JCMethodDecl method, Types.ArgPosition position) {
        var sym = method.sym;
        position.fitEnumConstructorPositionsDecl(method.params, syms).forEach(syms, (index, arg) -> {
            var extraArgParam = make.at(method.pos).Param(computeMethodArgName(sym, arg), arg, sym);
            extraArgParam.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
            method.params = insert(method.params, index, extraArgParam);
            sym.params = insert(sym.params, index, extraArgParam.sym);
        });
        method.type = copyTypeAndInsertTypeArgIfNeeded(method.type, position);
        sym.type = copyTypeAndInsertTypeArgIfNeeded(sym.type, position);
        sym.erasure_field = null;
    }

    private void setFieldsValues(JCTree.JCMethodDecl constructor) {
        var instructions = constructor.body.stats;

        var buffer = new ListBuffer<JCTree.JCStatement>();
        buffer.addAll(instructions);

        var baseArg = constructor.params.getFirst().sym;

        var isBase = true; // to set the base field which is always the first field
        for (var field : currentClassArgFields) {
            var fieldAccess = make.Select(make.This(currentClassTree.type), field.snd.sym);
            if (isBase) { // base field
                var assign = make.Assign(fieldAccess, make.Ident(baseArg));
                buffer.prepend(make.Exec(assign));
                isBase = false;
                continue;
            }

            var call = createSuperFromConcrete(field.fst);
            var assign = make.Assign(fieldAccess, call);
            buffer.prepend(make.Exec(assign));
        }

        // we set the field value before calling the super constructor as it is now allowed
        constructor.body.stats = buffer.toList();
    }

    private JCTree.JCMethodInvocation createSuperFromConcrete(JCTree.JCTypeApply superTypeParams) {
        var root = parameterizedTypeOfInvocation(-1);
        var args = new ListBuffer<JCTree.JCExpression>();
        var clazz = superTypeParams.clazz;

        args.add(make.Select(make.Type(clazz.type), syms.getClassField(clazz.type, types)));
        createSuperArgObject(args, superTypeParams.getTypeArguments());

        root.args = args.toList();
        return root;
    }

    private void createSuperArgObject(
        ListBuffer<JCTree.JCExpression> args, List<JCTree.JCExpression> superTypeParams
    ) {
        class SuperArgBuildingVisitor extends TreeScanner {

            private ListBuffer<JCTree.JCExpression> currentArgs = args;

            @Override
            public void visitIdent(JCTree.JCIdent tree) {
                if (tree.sym instanceof Symbol.TypeVariableSymbol symbol) {
                    var resolution = typeVarResolution(symbol.name);
                    currentArgs.add(resolution);
                } else if (tree.sym instanceof Symbol.ClassSymbol symbol) {
                    // otherwise, the type is a concrete type (Foo<String>)
                    var classOfCall = classTypeOfInvocation(-1);
                    classOfCall.args = List.of(make.ClassLiteral(symbol));
                    currentArgs.add(classOfCall);
                }
                // last case is a packageSymbol, we do not need to do anything
            }

            @Override
            public void visitTypeApply(JCTree.JCTypeApply tree) {
                var nestedCall = parameterizedTypeOfInvocation(-1);
                var oldArgs = currentArgs;
                try {
                    currentArgs = new ListBuffer<>();
                    var clazz = tree.clazz.type.tsym;
                    currentArgs.add(make.Select(make.Ident(clazz), syms.getClassField(clazz.type, types)));
                    super.visitTypeApply(tree);
                    nestedCall.args = currentArgs.toList();
                } finally {
                    currentArgs = oldArgs;
                    currentArgs.add(nestedCall);
                }
            }

            @Override
            public void visitTypeArray(JCTree.JCArrayTypeTree tree) {
                var nestedCall = arrayTypeOfInvocation(-1);
                var oldArgs = currentArgs;
                try {
                    currentArgs = new ListBuffer<>();
                    super.visitTypeArray(tree);
                    nestedCall.args = currentArgs.toList();
                } finally {
                    currentArgs = oldArgs;
                    currentArgs.add(nestedCall);
                }
            }

            @Override
            public void visitTypeIdent(JCTree.JCPrimitiveTypeTree tree) {
                var classOfCall = classTypeOfInvocation(-1);
                classOfCall.args = List.of(make.ClassLiteral((Symbol.ClassSymbol) tree.type.tsym));
                currentArgs.add(classOfCall);
            }

            @Override
            public void visitSelect(JCTree.JCFieldAccess tree) {
                // if the class is static, we do not need to create an inner class type or if the owner is not a class
                // (a package, i.e. Consumer<java.lang.String>)
                if (tree.sym.isStatic() || !(tree.sym.owner instanceof Symbol.ClassSymbol)) {
                    super.visitSelect(tree);
                    return;
                }
                var innerClassCall = innerClassTypeOfInvocation(-1);
                currentArgs.add(innerClassCall);
                var oldArgs = currentArgs;
                currentArgs = new ListBuffer<>();
                currentArgs.add(make.ClassLiteral(tree.sym.owner.type));
                try {
                    super.visitSelect(tree);
                } finally {
                    innerClassCall.args = currentArgs.toList();
                    currentArgs = oldArgs;
                }
            }


        }
        var visitor = new SuperArgBuildingVisitor();
        superTypeParams.forEach(type -> type.accept(visitor));
    }

    private void rewriteBasicMethod(JCTree.JCMethodDecl method) {
        var positions = types.typeArgPositions(method.sym);
        var oldInScopeBaseTypeParams = inScopeBaseTypeParams;
        if (method.sym.isStatic()) { // static methods do not have access to the class type parameters
            inScopeBaseTypeParams = List.nil();
        }
        try {
            if (!positions.hasMethodTypeArg()) {
                parameterizedMethodCallVisitor.visitMethod(method);
                return;
            }

            // generate backward compatibility overload
            if ((method.mods.flags & Flags.PRIVATE) == 0) {
                insertArgsInMethod(method, positions);
                var copy = copyMethod(method, positions);
                generateBackwardCompatibilityOverload(method, copy, positions, kind -> switch (kind) {
                    case ARG -> throw new AssertionError("No Arg for basic method");
                    case METHOD_TYPE_ARG -> {
                        var call = methodTypeArgsInvocation(-1);
                        call.args = defaultParams(method.sym.getTypeParameters());
                        yield call;
                    }
                });
            } else {
                insertArgsInMethod(method, positions);
            }

            // update scopes
            var scopeTypeParameter = new ScopeTypeParameter(
                method.typarams.stream().map(t -> t.name).toList(),
                basicMethodMArgHandle(method.sym, positions)
            );
            inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);

            parameterizedMethodCallVisitor.visitMethod(method);
        } finally {
            inScopeBaseTypeParams = oldInScopeBaseTypeParams;
        }
    }

    private final class InstructionVisitor extends TreeTranslator {

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            super.visitApply(tree);

            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) throw new AssertionError("No symbol for " + tree.meth);

            var methOwner = sym.owner;
            if (!methOwner.hasNewGenerics()) return; // if the owner does not have new generics, we have nothing to do

            var positions = types.typeArgPositions(sym);
            if (positions.hasNoArg()) return; // if the method does not have any type arguments, we have nothing to do

            if (sym.isConstructor()) {
                tree.args = insertAtArgPositions(
                    tree.args,
                    positions.fitEnumConstructorPositions(tree.args, syms),
                    kind -> switch (kind) {
                        case ARG -> {
                            var isSuper = TreeInfo.name(tree.meth) == names._super;
                            JCTree.JCExpression arg;
                            if (isSuper) {
                                var superClass = currentClass.getSuperclass();
                                var superTypeArguments = superClass.getTypeArguments();
                                if (superTypeArguments.isEmpty()) yield null;

                                // we need to generate a fake type apply for enums because the tree does not contain the 'extends' node
                                // TODO problem with rawtypes extending
                                if (currentClass.isAnonymous()) {
                                    arg = make.Ident(enclosingArgParamDecl);
                                } else {
                                    var typeApply = syms.enumSym == superClass.tsym
                                        ? enumTypeApply()
                                        : (JCTree.JCTypeApply) currentClassTree.extending;
                                    arg = createSuperFromConcrete(typeApply);
                                }
                            } else { // `this` case
                                if (enclosingArgParamDecl == null) {
                                    throw new AssertionError("No enclosing arg param decl");
                                }
                                arg = make.Ident(enclosingArgParamDecl);
                            }
                            yield arg;
                        }
                        case METHOD_TYPE_ARG -> { // TODO not sure if it can have the same code
                            var call = methodTypeArgsInvocation(-1);
                            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
                            // use the inferred types `String s = foo();`
                            if (tree.typeargs.isEmpty()) { // inference
                                call.args = argTypeParam(tree.meth.type.asMethodType().inferredTypes);
                            } else { // provided type arguments
                                var buffer = new ListBuffer<JCTree.JCExpression>();
                                tree.typeargs.forEach(t -> buffer.add(generateArgs(null, t.type)));
                                call.args = buffer.toList();
                            }
                            yield call;
                        }
                    }
                );
            } else {
                // we insert the args at the correct positions
                tree.args = insertAtArgPositions(tree.args, positions, kind -> switch (kind) {
                    case ARG -> throw new AssertionError("No Arg for method invocation");
                    case METHOD_TYPE_ARG -> {
                        var call = methodTypeArgsInvocation(-1);
                        // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
                        // use the inferred types `String s = foo();`
                        if (tree.typeargs.isEmpty()) { // inference
                            call.args = argTypeParam(tree.meth.type.asMethodType().inferredTypes);
                        } else { // provided type arguments
                            var buffer = new ListBuffer<JCTree.JCExpression>();
                            tree.typeargs.forEach(t -> buffer.add(generateArgs(null, t.type)));
                            call.args = buffer.toList();
                        }
                        yield call;
                    }
                });
            }

            // we then update the method type and the method symbol accordingly to the new signature
            tree.meth.type = copyTypeAndInsertTypeArgIfNeeded(tree.meth.type, positions);
            sym.type = copyTypeAndInsertTypeArgIfNeeded(sym.type, positions);
            sym.erasure_field = null;
        }

        @Override
        public void visitNewClass(JCTree.JCNewClass tree) {
            var sym = (Symbol.MethodSymbol) tree.constructor;
            var positions = types.typeArgPositions(sym).fitEnumConstructorPositions(tree.args, syms);
            // new EnumClass() does not have type args because it is the same for all labels, it can therefore be
            // created directly in the constructor body
            if (positions.hasNoArg()) {
                super.visitNewClass(tree);
                return;
            }

            // we update the method type and the method symbol accordingly to the new signature
            tree.constructorType = copyTypeAndInsertTypeArgIfNeeded(tree.constructorType, positions);
            // copy is maybe not always needed but at least for anonymous classes which shares the same method symbol
            // with the constructor MethodDecl in their class body

            sym.type = copyTypeAndInsertTypeArgIfNeeded(sym.type, positions);
            sym.erasure_field = null;

            Type.ClassType clazz;
            // for anonymous classes, we need to get the super class
            if (sym.owner.isAnonymous()) {
                clazz = (Type.ClassType) ((Type.ClassType) tree.type).supertype_field;
            } else {
                clazz = (Type.ClassType) tree.type;
            }
            // we insert the args at the correct positions
            tree.args = insertAtArgPositions(tree.args, positions, kind -> switch (kind) {
                case ARG -> internalTypeArgsConstruction(clazz);
                case METHOD_TYPE_ARG -> {
                    // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
                    // use the inferred types `String s = foo();`
                    var call = methodTypeArgsInvocation(-1);
                    if (tree.typeargs.isEmpty()) { // inference
                        call.args = argTypeParam(tree.constructorType.asMethodType().inferredTypes);
                    }
                    // provided type arguments
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    tree.typeargs.forEach(t -> buffer.add(generateArgs(null, t.type)));
                    call.args = buffer.toList();
                    yield call;
                }
            });
            super.visitNewClass(tree);
        }

        @Override
        public void visitTypeCast(JCTree.JCTypeCast tree) {
            super.visitTypeCast(tree);
            JCTree.JCMethodInvocation call;
            if (tree.expr.hasTag(JCTree.Tag.NEWARRAY)) { // if the cast is on a new array, we add the type arg
                call = externalMethodInvocation(
                    -1,
                    "addArrayTypeArg",
                    syms.typeArgUtils,
                    syms.objectType,
                    List.of(syms.objectType, syms.arrayTypeArgs)
                );
                // if the cast is not on a parameterized type, we have nothing to do
            } else if (tree.clazz.type.isPrimitive() || !tree.clazz.type.isParameterized()) {
                return;
            } else { // otherwise, we replace the cast
                call = checkCastInvocation(-1);
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

        @Override
        public void visitLambda(JCTree.JCLambda tree) {
            tree.scopeTypeParameters = inScopeBaseTypeParams;
            super.visitLambda(tree);
        }

        @Override
        public void visitReference(JCTree.JCMemberReference tree) {
            tree.scopeTypeParameters = inScopeBaseTypeParams;
            super.visitReference(tree);
        }

        public void visitField(JCTree.JCVariableDecl field) {
            if (field.init == null) {
                return;
            }

            var oldScope = inScopeBaseTypeParams;
            if (field.sym.isStatic()) {
                inScopeBaseTypeParams = List.nil();
            }
            try {
                field.init.accept(this);
            } finally {
                inScopeBaseTypeParams = oldScope;
            }
        }

        public void visitMethod(JCTree.JCMethodDecl method) {
            if (method.body == null) { // abstract method
                return;
            }
            addParameterizedArgumentTypeChecking(method);
            method.body.accept(this);
        }

        public void visitClassBlock(JCTree.JCBlock block) {
            var oldScope = inScopeBaseTypeParams;
            if ((block.flags & Flags.STATIC) != 0) {
                inScopeBaseTypeParams = List.nil();
            }
            try {
                block.accept(this);
            } finally {
                inScopeBaseTypeParams = oldScope;
            }
        }

        private void addParameterizedArgumentTypeChecking(JCTree.JCMethodDecl method) {
            var instructions = method.body.stats;
            for (JCTree.JCVariableDecl param : method.params) {
                // we check parameterized type and type vars
                if (!param.type.isParameterized() && !(param.type instanceof Type.TypeVar)) {
                    continue;
                }

                var arg = internalTypeArgsConstruction(param.type);
                var cast = checkCastInvocation(-1);
                cast.args = List.of(make.Ident(param.sym), arg);

                instructions = instructions.prepend(make.Exec(cast));
            }
            method.body.stats = instructions;
        }

    }

    //region type arg factories calls factories
    public JCTree.JCMethodInvocation enumGenericMethodTypeArg(
        Symbol classSymbol,
        TreeMaker maker,
        Env<AttrContext> env
    ) {
        Objects.requireNonNull(classSymbol);
        if (!classSymbol.isEnum()) {
            throw new AssertionError("Class " + classSymbol + " is not an enum");
        }
        var oldEnv = this.env;
        var oldMaker = this.make;
        this.env = env;
        this.make = maker;
        try {
            var methodTypeArgCall = methodTypeArgsInvocation(-1);
            var classTypeCall = classTypeOfInvocation(-1);
            classTypeCall.args = List.of(maker.ClassLiteral(classSymbol.type));
            methodTypeArgCall.args = List.of(classTypeCall);
            return methodTypeArgCall;
        } finally {
            this.env = oldEnv;
            this.make = oldMaker;
        }
    }

    private JCTree.JCMethodInvocation externalMethodInvocation(
        int pos, String name, Type ownerType, Type returnType, List<Type> paramTypes
    ) {
        return externalMethodInvocation(pos, name, ownerType, returnType, paramTypes, TreeMaker::Ident);
    }


    private JCTree.JCMethodInvocation externalMethodInvocation(
        int pos,
        String name,
        Type ownerType,
        Type returnType,
        List<Type> paramTypes,
        BiFunction<TreeMaker, Symbol, JCTree.JCExpression> methFactory
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
        var methodCall = make.Apply(List.nil(), null, List.nil()).setType(returnType);
        if (sym.isVarArgs()) {
            methodCall.varargsElement = ((Type.ArrayType) sym.params.last().type).elemtype;
        }
        methodCall.meth = methFactory.apply(make, sym);
        return methodCall;
    }

    private JCTree.JCMethodInvocation rawTypeOfInvocation(int pos) {
        return externalMethodInvocation(pos, "of", syms.rawTypeTypeArgs, syms.rawTypeTypeArgs, List.of(syms.classType));
    }

    private JCTree.JCMethodInvocation parameterizedTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.parameterizedTypeTypeArgs,
            syms.parameterizedTypeTypeArgs,
            List.of(syms.classType, types.makeArrayType(syms.argBaseType))
        );
    }

    private JCTree.JCMethodInvocation classTypeOfInvocation(int pos) {
        return externalMethodInvocation(pos, "of", syms.classTypeArgs, syms.classTypeArgs, List.of(syms.classType));
    }

    private JCTree.JCMethodInvocation wildcardTypeOfInvocation(boolean isSuper, int pos) {
        var callName = isSuper ? "ofLower" : "ofUpper";
        return externalMethodInvocation(
            pos,
            callName,
            syms.wildcardTypeArgs,
            syms.wildcardTypeArgs,
            List.of(syms.wildcardTypeArgs)
        );
    }

    private JCTree.JCMethodInvocation arrayTypeOfInvocation(int pos) {
        return externalMethodInvocation(pos, "of", syms.arrayTypeArgs, syms.arrayTypeArgs, List.of(syms.argBaseType));
    }

    private JCTree.JCMethodInvocation innerClassTypeOfInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.innerClassTypeArgs,
            syms.innerClassTypeArgs,
            List.of(syms.argBaseType, syms.argBaseType)
        );
    }

    private JCTree.JCMethodInvocation methodTypeArgsInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "of",
            syms.methodTypeArgs,
            syms.methodTypeArgs,
            List.of(types.makeArrayType(syms.argBaseType))
        );
    }

    private JCTree.JCMethodInvocation getGetArgInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "getArg",
            syms.typeArgUtils,
            syms.argBaseType,
            List.of(syms.argBaseType, types.makeArrayType(syms.intType))
        );
    }

    private JCTree.JCMethodInvocation checkCastInvocation(int pos) {
        return externalMethodInvocation(
            pos,
            "checkCast",
            syms.typeOperationsType,
            syms.objectType,
            List.of(syms.objectType, syms.argBaseType)
        );
    }
    //endregion

    //region type arg name computation
    private Name computeArgName(Symbol.ClassSymbol owner) {
        var synChar = target.syntheticNameChar();
        var pkg = owner.packge().fullname.toString().replace('.', synChar);
        var name = owner.getSimpleName().toString();
        var strName = "0" + synChar + "typeArgs" + synChar + pkg + synChar + synChar + name;
        return names.fromString(strName);
    }

    private Name computeMethodArgName(Symbol owner, Type kind) {
        String name;
        if (types.isSameType(kind, syms.argBaseType)) {
            name = "arg";
        } else if (types.isSameType(kind, syms.methodTypeArgs)) {
            name = "methodTypeArgs";
        } else {
            throw new AssertionError("Unexpected kind: " + kind);
        }
        return depthDependentName(owner, name);
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
    //endregion

    private JCTree.JCExpression internalTypeArgsConstruction(Type type) {
        var args = type.getTypeArguments();
        if (args.isEmpty()) {
            if (type.tsym instanceof Symbol.ClassSymbol csym) {
                JCTree.JCMethodInvocation call;
                if (csym.type.isParameterized()) { // raw call
                    call = rawTypeOfInvocation(-1);
                } else { // class call
                    call = classTypeOfInvocation(-1);
                }
                call.args = List.of(make.ClassLiteral(csym));
                return call;
            } else if (type.tsym instanceof Symbol.TypeVariableSymbol tsym) {
                return typeVarResolution(tsym.name);
            } else {
                throw new AssertionError("Unexpected type " + type);
            }
        }
        var call = parameterizedTypeOfInvocation(-1);
        call.args = argTypeParam(args.reverse());
        call.args = call.args.prepend(make.Select(make.Ident(type.tsym), syms.getClassField(type.tsym.type, types)));
        return call;
    }

    private List<JCTree.JCExpression> argTypeParam(List<Type> types) {
        var buffer = new ListBuffer<JCTree.JCExpression>();
        types.forEach(t -> buffer.prepend(generateArgs(null, t)));
        return buffer.toList();
    }

    public JCTree.JCExpression generateArgs(
        Type type, Env<AttrContext> env, TreeMaker make, List<ScopeTypeParameter> scope
    ) {
        Objects.requireNonNull(type);
        Objects.requireNonNull(env);
        Objects.requireNonNull(scope);
        var oldEnv = this.env;
        var oldMake = this.make;
        var oldScope = this.inScopeBaseTypeParams;
        try {
            this.env = env;
            this.make = make;
            this.inScopeBaseTypeParams = scope;
            return generateArgs(null, type);
        } finally {
            this.env = oldEnv;
            this.make = oldMake;
            this.inScopeBaseTypeParams = oldScope;
        }
    }

    private JCTree.JCExpression generateArgs(Type previous, Type current) {
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
                var c = classTypeOfInvocation(-1);
                c.args = List.of(make.ClassLiteral(syms.objectType));
                call.args = List.of(c);
                yield call;
            }
            case Type.IntersectionClassType intersectionClassType -> {
                var call = externalMethodInvocation(
                    -1,
                    "of",
                    syms.intersectionTypeArgs,
                    syms.intersectionTypeArgs,
                    List.of(types.makeArrayType(syms.argBaseType))
                );
                var buffer = new ListBuffer<JCTree.JCExpression>();
                intersectionClassType.getComponents().forEach(c -> buffer.add(generateArgs(current, c)));
                call.args = buffer.toList();
                yield call;
            }
            case Type.ClassType ignored -> { // Foo / Foo<E> / Foo(raw)
                var classFieldAcc = make.ClassLiteral((Symbol.ClassSymbol) current.tsym);
                if (current.isRaw()) { // Foo(raw)
                    var rawTypeCall = rawTypeOfInvocation(-1);
                    rawTypeCall.args = List.of(classFieldAcc);
                    yield rawTypeCall;
                } else if (current.getTypeArguments().nonEmpty()) { // Foo<E> (E can be a wildcard)
                    var call = parameterizedTypeOfInvocation(-1);
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    buffer.add(classFieldAcc);
                    var ctype = (Type.ClassType) current;
                    ctype.typarams_field.forEach(param -> buffer.add(generateArgs(current, param)));
                    call.args = buffer.toList();
                    yield call;
                } else { // Foo
                    var call = classTypeOfInvocation(-1);
                    call.args = List.of(classFieldAcc);
                    yield call;
                }
            }
            case Type.CapturedType capturedType -> {
                var bound = capturedType.getUpperBound();
                if (bound.equals(previous)) { // to avoid infinite recursion on recursive types
                    var call = classTypeOfInvocation(-1);
                    call.args = List.of(make.ClassLiteral(syms.objectType));
                    yield call;
                }
                yield generateArgs(previous, bound);
            }
            case Type.TypeVar typeVar -> {
                var owner = typeVar.tsym.owner;
                // TODO unsure
                // if the owner of this type does not have it in its declared type parameters, it is a wildcard
                var index = owner.type.getTypeArguments().indexOf(typeVar);
                if (index == -1) { // wildcard
                    var call = wildcardTypeOfInvocation(false, -1);
                    var classCall = classTypeOfInvocation(-1);
                    classCall.args = List.of(make.Select(
                        make.Ident(syms.objectType.tsym),
                        syms.getClassField(syms.objectType, types)
                    ));
                    call.args = List.of(classCall);
                    yield call;
                }
                yield typeVarResolution(typeVar.tsym.name);
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
            var params = List.of(outerRes, res);
            var innerCall = innerClassTypeOfInvocation(-1);
            innerCall.args = params;
            res = innerCall;
        }

        return res;
    }

    private JCTree.JCTypeApply enumTypeApply() {
        return make.TypeApply(make.Ident(syms.enumSym), List.of(make.Ident(currentClass)));
    }

    /**
     * If the baseType parameter does not contain the expectedArg at the index position, we copy the baseType and insert
     * the expectedArg at the index position. Otherwise, we return the baseType.
     */
    private Type copyTypeAndInsertTypeArgIfNeeded(Type baseType, Types.ArgPosition positions) {
        if (positions.hasNoArg()) return baseType;
        var copy = copyMethodType(baseType);
        var copyMtype = copy.asMethodType();
        copyMtype.argtypes = insertAtIndex(copyMtype.argtypes, (i, old) -> {
            var atPos = positions.atPos(i, syms);
            // only insert if atPos exists and is different from the old type
            return atPos != null && (old == null || !types.isSameType(old, atPos)) ? atPos : null;
        });
        return copy;
    }

    public Type copyTypeAndInsertTypeArgIfNeeded(Symbol symbol, Type baseType) {
        Objects.requireNonNull(symbol);
        Objects.requireNonNull(baseType);
        return copyTypeAndInsertTypeArgIfNeeded(baseType, types.typeArgPositions(symbol));
    }

    //region type variable resolution
    private JCTree.JCExpression typeVarResolution(Name typeVarName) {
        ScopeTypeParameter foundScope = null;
        int foundIndex = -1;
        for (var current = inScopeBaseTypeParams; current.nonEmpty(); current = current.tail) {
            var scope = current.head;
            var index = scope.indexOf(typeVarName);
            if (index != -1) {
                foundScope = scope;
                foundIndex = index;
                break;
            }
        }

        if (foundScope == null) {
            throw new AssertionError("Could not find type var " + typeVarName + " in " + currentClass + " with the following scope " + inScopeBaseTypeParams);
        }

        return foundScope.argAccessFactory().apply(foundIndex);
    }

    /**
     * Handler for type arguments access. The returned lambda is used everytime the information of a specific type
     * parameter concrete value is needed.
     * <p/>
     * This method uses the provided methodSymbol parameter to retrieves the MethodTypeArgs parameter at its position,
     * and then calls the MethodTypeArgs.arg(int) to get the type parameter value.
     */
    private IntFunction<JCTree.JCMethodInvocation> basicMethodMArgHandle(
        Symbol.MethodSymbol methodSymbol,
        Types.ArgPosition position
    ) {
        var param = methodSymbol.params.get(position.methodTypeArgPosition());
        return index -> {
            var call = externalMethodInvocation(
                -1,
                "arg",
                syms.methodTypeArgs,
                syms.argBaseType,
                List.of(syms.intType),
                (m, s) -> m.Select(m.Ident(param), s)
            );
            call.args = List.of(make.Literal(index));
            return call;
        };
    }

    /**
     * Handler for type arguments access. The returned lambda is used everytime the information of a specific type
     * parameter concrete value is needed.
     * <p/>
     * This method uses the provided methodSymbol parameter to retrieves the Arg parameter at its position, and then
     * calls the TypeArgUtils.getArg(Arg, int...) to get the type parameter value.
     */
    private IntFunction<JCTree.JCMethodInvocation> constructorArgHandle(
        Symbol.MethodSymbol methodSymbol,
        Types.ArgPosition position
    ) {
        var param = methodSymbol.params.get(position.argPosition());
        return index -> {
            var call = getGetArgInvocation(-1);
            call.args = List.of(make.Ident(param), make.Literal(index));
            return call;
        };
    }

    /**
     * Handler for type arguments access. The returned lambda is used everytime the information of a specific type
     * parameter concrete value is needed.
     * <p/>
     * This method uses the provided classBaseField storing the class type parameters information of the instance of the
     * class, and then calls the TypeArgUtils.getArg(Arg, int...) to get the type parameter value.
     * <p/>
     * This handle is used everywhere in the class EXCEPT in constructors, whenever a type parameter of the class is
     * needed.
     */
    private IntFunction<JCTree.JCMethodInvocation> concreteClassArgHandle(JCTree.JCVariableDecl classBaseField) {
        return index -> {
            var call = getGetArgInvocation(-1);
            call.args = List.of(make.Ident(classBaseField), make.Literal(index));
            return call;
        };
    }

    /**
     * Handler for type arguments access. The returned lambda is used everytime the information of a specific type
     * parameter concrete value is needed.
     * <p/>
     * This implementation is used for interfaces, as they do not have access to the fields, we need to first get the
     * Arg representing the interface from the concrete class implementing it, and then get the actual type parameter
     * value.
     */
    private IntFunction<JCTree.JCMethodInvocation> interfaceArgHandle(Symbol.ClassSymbol interfaceClass) {
        // generates: TyperArgUtils.getArg(TypeArgUtils.getArg(this, CurrentClass.class), index);
        return index -> {
            // first getArg that retrieves the Arg representing the type parameters of the interface from the concrete
            // class implementing it
            var argAccessingCall = externalMethodInvocation(
                -1,
                "getArg",
                syms.typeArgUtils,
                syms.argBaseType,
                List.of(syms.objectType, syms.classType)
            );
            argAccessingCall.args = List.of(
                make.This(interfaceClass.type),
                make.ClassLiteral(interfaceClass)
            );
            // then a second getArg on the retrieved Arg to get the actual type parameter value
            var getInnerArgCall = getGetArgInvocation(-1);
            getInnerArgCall.args = List.of(argAccessingCall, make.Literal(index));
            return getInnerArgCall;
        };
    }

    /**
     * This class is used to represent the scope of type parameters. Anytime a type parameter is referenced, we need to
     * know its actual value. To do so each time a type parameter is declared (either in a method or a class), we create
     * a new ScopeTypeParameter instance that will store all the names of the type parameters declared in the scope, and
     * a factory that generates the method call to get the actual value of the type parameter.
     */
    public static final class ScopeTypeParameter {

        private static final ScopeTypeParameter EMPTY = new ScopeTypeParameter(
            com.sun.tools.javac.util.List.nil(),
            i -> {
                throw new AssertionError("No argument access factory for empty scope");
            }
        );

        // we could use a HashMap<Name, Integer> to avoid the indexOf calls but as most classes have few type parameters,
        // it does not change much
        private final java.util.List<Name> variableParamNames;
        private final IntFunction<? extends JCTree.JCExpression> argAccessFactory;

        private ScopeTypeParameter(
            java.util.List<Name> variableParamNames,
            IntFunction<? extends JCTree.JCExpression> argAccessFactory
        ) {
            this.variableParamNames = java.util.List.copyOf(variableParamNames);
            this.argAccessFactory = argAccessFactory;
        }

        public int indexOf(Name name) {
            return variableParamNames.indexOf(name);
        }


        private IntFunction<? extends JCTree.JCExpression> argAccessFactory() {
            return argAccessFactory;
        }

    }
    //endregion

    //region utils

    /**
     * Copy the given method and set the flags to synthetic. the body is not copied. This method also remove Arg
     * parameters if they are present.
     */
    private JCTree.JCMethodDecl copyMethod(JCTree.JCMethodDecl baseMethod, Types.ArgPosition positions) {
        var treeCopy = make.at(baseMethod.pos).MethodDef(
            make.Modifiers(
                baseMethod.mods.flags,
                baseMethod.mods.annotations
            ),
            baseMethod.name,
            baseMethod.restype,
            baseMethod.typarams,
            removeAtArgPositions(baseMethod.params, positions),
            baseMethod.thrown,
            null,
            baseMethod.defaultValue
        );
        treeCopy.mods.flags |= Flags.SYNTHETIC;

        var sym = baseMethod.sym;
        var symbolCopy = sym.clone(sym.owner);
        symbolCopy.flags_field |= Flags.SYNTHETIC | Flags.NEW_GENERICS;
        symbolCopy.params = removeAtArgPositions(sym.params, positions);
        symbolCopy.extraParams = sym.extraParams;
        symbolCopy.capturedLocals = sym.capturedLocals;
        symbolCopy.defaultValue = sym.defaultValue;

        var actualType = copyMethodType(baseMethod.type);
        var mt = actualType.asMethodType();
        mt.argtypes = removeAtArgPositions(mt.argtypes, positions);
        symbolCopy.type = actualType;

        treeCopy.sym = symbolCopy;
        treeCopy.type = copyMethodType(actualType);

        if ((baseMethod.mods.flags & Flags.ABSTRACT) != 0) {
            // remove the abstract flag
            treeCopy.mods.flags = (treeCopy.mods.flags & ~Flags.ABSTRACT);
            symbolCopy.flags_field = (symbolCopy.flags_field & ~Flags.ABSTRACT);
            // if the class is an interface, we need to add the default modifier
            if (currentClass.isInterface()) {
                treeCopy.mods.flags |= Flags.DEFAULT;
                symbolCopy.flags_field |= Flags.DEFAULT;
            }
        }

        return treeCopy;
    }

    private static <T> List<T> removeAtArgPositions(List<T> list, Types.ArgPosition position) {
        var buffer = new ListBuffer<T>();
        var index = 0;
        for (var e : list) {
            if (!position.anyAtPos(index++)) {
                buffer.add(e);
            }
        }
        return buffer.toList();
    }

    private <T> List<T> insert(List<T> list, int index, T element) {
        if (index == 0) {
            return list.prepend(element);
        } else if (index < 0) {
            return list;
        }

        var buffer = new ListBuffer<T>();
        var i = 0;
        for (var t : list) {
            if (i == index) {
                buffer.add(element);
            }
            buffer.add(t);
            i++;
        }

        if (index == i) {
            buffer.add(element);
        } else if (index > i) {
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for list of size " + i);
        }

        return buffer.toList();
    }

    private interface IntBiFunction<T, R> {
        R apply(int i, T t);
    }

    private static <T> List<T> insertAtIndex(List<T> list, IntBiFunction<T, T> positionMapper) {
        var buffer = new ListBuffer<T>();
        var i = 0;
        var current = list;
        while (current.nonEmpty()) {
            var element = positionMapper.apply(i, current.head);
            if (element != null) {
                buffer.add(element);
            } else {
                buffer.add(current.head);
                current = current.tail;
            }
            i++;
        }

        var element = positionMapper.apply(i, null);
        if (element != null) {
            buffer.add(element);
        }
        return buffer.toList();
    }

    private static <T> List<T> insertAtIndex(List<T> list, IntFunction<T> positionMapper) {
        return insertAtIndex(list, (i, t) -> positionMapper.apply(i));
    }

    private enum ArgKind {
        ARG,
        METHOD_TYPE_ARG
    }

    private static <T> List<T> insertAtArgPositions(
        List<T> list,
        Types.ArgPosition position,
        Function<ArgKind, T> positionMapper
    ) {
        return insertAtIndex(list, i -> {
            if (i == position.methodTypeArgPosition()) {
                return positionMapper.apply(ArgKind.METHOD_TYPE_ARG);
            } else if (i == position.argPosition()) {
                return positionMapper.apply(ArgKind.ARG);
            }
            return null;
        });
    }

    /**
     * This method returns true only if the type parameter represented by typeApply is parameterized by a type parameter
     * declared by the current class.
     * <p>
     * tl;dr it avoid to generates a type arg field for cases where the type parameters are fixed e.g. Foo implements
     * Consumer&lt;String&gt;
     */
    private boolean isParameterizedByClass(JCTree.JCTypeApply typeApply) {
        class Scan extends TreeScanner {

            private boolean parameterized = false;

            @Override
            public void visitIdent(JCTree.JCIdent tree) {
                if (tree.sym instanceof Symbol.TypeVariableSymbol) { // if param is parameterized by the class
                    parameterized = true;
                }
            }

            @Override
            public void scan(JCTree tree) {
                if (parameterized) return; // short-circuit
                super.scan(tree);
            }

            @Override
            public void visitTypeApply(JCTree.JCTypeApply tree) {
                for (var argument : tree.getTypeArguments()) {
                    scan(argument);
                    if (parameterized) break; // short-circuit
                }
            }

        }
        var visitor = new Scan();
        typeApply.accept(visitor);
        return visitor.parameterized;
    }

    private static final class JCTreeCopier extends TreeTranslator {

        private final TreeMaker make;

        public JCTreeCopier(TreeMaker make) {
            this.make = make;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T extends JCTree> T translate(T tree) {
            if (tree == null) {
                return null;
            } else {
                ((T) tree.clone()).accept(this);
                JCTree tmpResult = this.result;
                this.result = null;
                return (T) tmpResult;
            }
        }

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            result = make.Apply(
                tree.typeargs,
                translate(tree.meth),
                translate(tree.args)
            ).setType(tree.type);
        }

        @Override
        public void visitNewClass(JCTree.JCNewClass tree) {
            var cl = make.NewClass(
                translate(tree.encl),
                translate(tree.typeargs),
                translate(tree.clazz),
                translate(tree.args),
                translate(tree.def)
            );
            cl.setType(tree.type);
            cl.constructor = tree.constructor;
            cl.constructorType = tree.constructorType;
            result = cl;
        }

        @Override
        public void visitNewArray(JCTree.JCNewArray tree) {
            var jcNewArray = make.NewArray(
                translate(tree.elemtype),
                translate(tree.dims),
                translate(tree.elems)
            );
            jcNewArray.setType(tree.type);
            jcNewArray.annotations = translate(tree.annotations);
            var dimAnnotations = new ListBuffer<List<JCTree.JCAnnotation>>();
            for (var dimAnnos : tree.dimAnnotations) {
                var buffer = new ListBuffer<JCTree.JCAnnotation>();
                for (var anno : dimAnnos) {
                    buffer.add(translate(anno));
                }
                dimAnnotations.add(buffer.toList());
            }
            jcNewArray.dimAnnotations = dimAnnotations.toList();
            result = jcNewArray.setType(tree.type);
        }

        @Override
        public void visitTypeCast(JCTree.JCTypeCast tree) {
            result = make.TypeCast(translate(tree.clazz), translate(tree.expr)).setType(tree.type);
        }
    }
    //endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree cdef, TreeMaker make) {
        try {
            this.make = make;
            this.env = env;
            this.treeCopier = new JCTreeCopier(make);
//            return cdef;
            return translate(cdef);
        } finally {
            this.make = null;
            this.env = null;
            this.treeCopier = null;
        }
    }

    private void debug(Object... o) {
        if (o.length == 0) {
            throw new AssertionError("No arguments provided");
        }
        var str = Stream.of(o).map(String::valueOf).collect(Collectors.joining(", ", "debug: ", ""));
        log.printRawLines(str);
    }

}
