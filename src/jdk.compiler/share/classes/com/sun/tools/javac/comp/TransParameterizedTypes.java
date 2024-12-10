package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.BoundKind;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Kinds;
import com.sun.tools.javac.code.Scope;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.TypeTag;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
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
    private final LocalClassIndex indices;
    private final Resolve rs;

    private JCTreeCopier treeCopier;
    private Env<AttrContext> env = null;
    private Symbol.ClassSymbol currentClass = null;
    private JCTree.JCClassDecl currentClassTree = null;
    private ArrayList<Pair<JCTree, Symbol>> generatedDecl = null;
    private boolean shouldProcessCurrentClass;
    private boolean inJavaUtilPtype;

    /**
     * NOTE: The base arg field is always the first element of the list.
     */
    private java.util.List<Pair<Type, JCTree.JCVariableDecl>> currentClassArgFields = null;

    private List<Map<Symbol, Type>> typeMappingScope = null;
//    private ArrayList<Pair<JCTree.JCTypeApply, JCTree.JCVariableDecl>> currentClassArgFields = null;

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
    private java.util.List<JCTree.JCStatement> inlineAndBlockDecls = null;

    /**
     * This list is only used when compiling a non-processed class. It will contain all the methods coming from its
     * super classes and interfaces that are:
     * <ul>
     * <li>coming from a processed class or interface</li>
     * <li>parameterized (meaning that the method have a leading MethodTypeArgs param)</li>
     * <li>not declared final or private (and can therefore potentially be overridden in the current class)</li>
     * </ul>
     * <p/>
     * Each time a non-private method will be visited for this class, we will check that this method is not
     */
    private java.util.List<Symbol.MethodSymbol> openMethods = null;

    /**
     * We assume that there will not be more thant MAX_LONG casts to generate during the whole compilation process
     */
    private long locationCount;

    /**
     * These two fields are used when dealing with an external field assign
     */
    private Symbol externalFieldOwner;
    private JCTree.JCExpression externalSymbolAccess;
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
        rs = Resolve.instance(context);
        parameterizedMethodCallVisitor = new InstructionVisitor();
        indices = new LocalClassIndex();
    }
    //endregion


    /**
     * this method is only called for the top-level class
     */
    @Override
    public void visitClassDef(JCTree.JCClassDecl tree) {
        result = tree;
//        if (true) return;
//        if (!tree.sym.hasNewGenerics()) return;
        shouldProcessCurrentClass = shouldProcess(tree.sym);
        inJavaUtilPtype = names.java_util_ptype.equals(tree.sym.packge().getQualifiedName());
        try {
            inScopeBaseTypeParams = List.nil();
            typeMappingScope = List.nil();
            rewriteClass(tree);
        } catch (Throwable t) {
            debug("error in class: " + tree.sym.fullname);
            throw t;
        } finally {
            inScopeBaseTypeParams = null;
            typeMappingScope = null;
        }
    }

    public boolean shouldProcess(Symbol.ClassSymbol clazz) {
        Objects.requireNonNull(clazz);
        var packageName = clazz.packge().getQualifiedName().toString();
        var fullName = clazz.fullname.toString();
        var ignored = packageName.startsWith("java.lang")
            || packageName.startsWith("jdk.internal")
            || packageName.startsWith("java.security")
            || "java.util.WeakHashMap".equals(fullName)
            || (clazz.owner instanceof Symbol.ClassSymbol && !clazz.owner.hasNewGenerics())
            || packageName.startsWith("sun.reflect.generics");
        return !ignored;
//        return false;
    }

    private void rewriteClass(JCTree.JCClassDecl tree) {
        var oldClass = currentClass;
        var oldClassTree = currentClassTree;
        var oldCurrentClassArgFields = currentClassArgFields;
        var oldInScopeBaseTypeParams = inScopeBaseTypeParams;
        var oldGeneratedDecl = generatedDecl;
        var oldInlineAndBlockDecls = inlineAndBlockDecls;
        var oldTypeMappingScope = typeMappingScope;
        var oldOpenMethods = openMethods;
        indices.pushIndex();

        var isClassParameterized = tree.sym.type.getTypeArguments().nonEmpty();
        try {
            generatedDecl = new ArrayList<>();
            currentClass = tree.sym;
            inlineAndBlockDecls = new ArrayList<>();
            if (currentClass.isStatic()) { // static classes cannot access the type of the outer classes
                inScopeBaseTypeParams = List.nil();
                typeMappingScope = List.nil();
            }
            currentClassTree = tree;
            if (shouldProcessCurrentClass) {
                if (isClassParameterized) {
                    var params = tree.sym.type.getTypeArguments().stream().map(t -> t.tsym).toList();
                    IntFunction<JCTree.JCMethodInvocation> argAccessFactory;
                    if (currentClass.isInterface()) {
                        currentClassArgFields = List.nil();
                        argAccessFactory = interfaceArgHandle(currentClass);
                    } else {
                        // if the class is not an interface, we generate the fields for the type parameters
                        var fieldGeneration = generateFields(tree);
                        currentClassArgFields = fieldGeneration.fields();
                        var classBaseField = currentClassArgFields.getFirst().snd;
                        argAccessFactory = concreteClassArgHandle(classBaseField);
                    }
                    var scopeTypeParameter = new ScopeTypeParameter(params, argAccessFactory);
                    inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
                } else {
                    currentClassArgFields = List.nil();
                }
            } else {
                currentClassArgFields = List.nil();
                inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(
                    new ScopeTypeParameter(
                        tree.sym.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                        rawTypeArgHandle(currentClass)
                    )
                );
            }

            openMethods = openMethods();
            rewriteDefs(tree);

            // once we have done rewriting the current class, we apply the modifiers
            currentClassArgFields.forEach(field -> {
                field.snd.mods.flags |= Flags.SYNTHETIC;
                field.snd.sym.flags_field |= Flags.SYNTHETIC;
            });
            var buffer = new ListBuffer<JCTree>();
            buffer.addAll(tree.defs);
            generatedDecl.forEach(pair -> {
                tree.sym.members_field.enter(pair.snd);
                buffer.append(pair.fst);
            });
            tree.defs = buffer.toList();
        } finally {
            currentClass = oldClass;
            currentClassTree = oldClassTree;
            currentClassArgFields = oldCurrentClassArgFields;
            inScopeBaseTypeParams = oldInScopeBaseTypeParams;
            generatedDecl = oldGeneratedDecl;
            inlineAndBlockDecls = oldInlineAndBlockDecls;
            typeMappingScope = oldTypeMappingScope;
            openMethods = oldOpenMethods;
            indices.popIndex();
        }
    }

    //region fields generation
    private FieldGeneration generateFields(JCTree.JCClassDecl tree) {
        var result = new ArrayList<Pair<Type, JCTree.JCVariableDecl>>();

        // we must generate only one field per class, meaning that we must keep track of the classes we have already
        // generated the field for. It is important when a class transitively implements the same interface multiple
        // times
        var fields = new HashSet<Symbol>();

        // this map is used to map the type variable from transitive super interfaces to the concrete type variable
        // e.g.     A<T> implements B<E>  and B<E> implements C<F>
        // this map provides the following mappings:
        // - T -> T
        // - E -> T
        // - F -> T
        // this is later used for retrieve the actual argument from the current scope
        var paramToConcrete = new HashMap<Symbol, Type>();
        typeMappingScope = typeMappingScope.prepend(paramToConcrete);

        // generate the base field
        var baseField = generateField(tree.sym);
        result.add(Pair.of(null, baseField));
        tree.sym.type.getTypeArguments().forEach(t -> paramToConcrete.put(t.tsym, t));

        generateFields(tree.sym, result, fields, paramToConcrete);
        return new FieldGeneration(Map.copyOf(paramToConcrete), java.util.List.copyOf(result));
//        var result = new ArrayList<Pair<JCTree.JCTypeApply, JCTree.JCVariableDecl>>();
//
//        // no need to generate super field, as it will be generated in the super class
//
//        // generate the fields for the implementing types
//        tree.implementing.forEach(expression -> {
//            if (!expression.type.isParameterized()) { // if the type is not parameterized, no need to generate a field
//                return;
//            }
//            var apply = (JCTree.JCTypeApply) expression;
//            if (!isParameterizedByClass(apply)) { // if the type is not parameterized by the class, no need to generate a field
//                return;
//            }
//            var superInterface = (Symbol.ClassSymbol) apply.clazz.type.tsym;
//            var field = generateField(superInterface);
//            result.add(Pair.of(apply, field));
//            if (superInterface.getInterfaces().isEmpty()) {
//                return;
//            }
//            var filtered = superInterface.getInterfaces().filter(t -> t.getTypeArguments().nonEmpty());
//            if (filtered.isEmpty()) {
//                return;
//            }
//            var firstParamInterface = filtered.getFirst();
//            var firstParam = firstParamInterface.getTypeArguments().getFirst();
//            var f = superInterface.type.getTypeArguments().head;
//            debug(currentClass.type, apply, firstParamInterface, firstParam, firstParam.tsym.owner, f, f == firstParam);
//        });
//
//        return result;
    }

    /**
     * recursively generate the fields for the transitive super interfaces
     */
    private void generateFields(
        Symbol.ClassSymbol classSymbol,
        ArrayList<Pair<Type, JCTree.JCVariableDecl>> fields,
        HashSet<Symbol> encountered,
        HashMap<Symbol, Type> mapping
    ) {
        if (!encountered.add(classSymbol)) return;

        var queue = new ArrayList<Symbol.ClassSymbol>();
        classSymbol.getInterfaces().forEach(interfce -> {
            var tsym = (Symbol.ClassSymbol) interfce.tsym;

            queue.add(tsym);
            // we must check the tsym type in case of rawtype inheritance
            if (tsym.type.getTypeArguments().isEmpty()) return;

            var field = generateField(tsym);
            fields.add(Pair.of(interfce, field));

            // if the type is not parameterized, while the symbol is, it means that rawtype inheritance is happening
            if (interfce.getTypeArguments().isEmpty()) {
                // TODO rawtype inheritance
                debug("Rawtype inheritance", tsym.type.getTypeArguments());
                return;
            }

            if (interfce.getTypeArguments().size() != tsym.type.getTypeArguments().size()) {
                throw new AssertionError("Type arguments size mismatch. expected:" + tsym.type.getTypeArguments().size() + " actual:" + interfce.getTypeArguments().size());
            }

            // we must now add all args to the mapping
            var interfceArgs = interfce.getTypeArguments().iterator();
            var csymArgs = tsym.type.getTypeArguments().iterator();

            while (interfceArgs.hasNext()) {
                var interfceArg = interfceArgs.next();
                var csymArg = csymArgs.next();
                // if the arg is not a type variable, we do not need to add it to the mapping
                if (!(interfceArg.tsym instanceof Symbol.TypeVariableSymbol)) {
                    mapping.put(csymArg.tsym, interfceArg);
                    continue;
                }

                // should already be in the mapping, added in previous calls
                var concrete = findType(interfceArg);
                Objects.requireNonNull(
                    concrete,
                    "Concrete type '" + interfceArg + "' not found in " + mapping + ". Current class:" + currentClass + ", interface:" + interfce + " classSymbol: " + classSymbol
                );

                // we must put both the type and the symbol type variable in the mapping
                // - the type will be used when building the actual arg in constructor
                // - the symbol will be used in recursive calls to find the concrete
                mapping.put(interfceArg.tsym, concrete);
                mapping.put(csymArg.tsym, concrete);
            }

        });

        queue.forEach(tsym -> generateFields(tsym, fields, encountered, mapping));
    }

    private JCTree.JCVariableDecl generateField(Symbol.ClassSymbol superType) {
        var fieldFlags = Flags.PRIVATE | Flags.FINAL | Flags.TRANSIENT;
        var fieldName = computeArgName(superType);
        var baseField = make.VarDef(
            make.Modifiers(fieldFlags),
            fieldName,
            make.Type(syms.argBaseType),
            null // init is done in constructor
        );
        var fieldSym = new Symbol.VarSymbol(fieldFlags, fieldName, syms.argBaseType, currentClass);
        baseField.sym = fieldSym;
        baseField.type = syms.argBaseType;

        generatedDecl.add(Pair.of(baseField, fieldSym));
        return baseField;
    }

    private record FieldGeneration(Map<Symbol, Type> paramToConcrete,
                                   java.util.List<Pair<Type, JCTree.JCVariableDecl>> fields) {
        public FieldGeneration {
            Objects.requireNonNull(paramToConcrete);
            Objects.requireNonNull(fields);
        }

    }

    private Type findType(Type typeVar) {
        // because in interfaces typeMappingScope is empty OR if the type does not come from a class
        if (typeMappingScope.isEmpty() || typeVar.tsym.owner.kind != Kinds.Kind.TYP) return typeVar;

        var current = typeMappingScope;
        while (!current.isEmpty()) {
            var mapping = current.head;
            var type = mapping.get(typeVar.tsym);
            if (type != null) {
                return type;
            }
            current = current.tail;
        }
        // workaround to ignore if the symbol is external
        if (externalFieldOwner == null) {
            throw new AssertionError("Type not found in mapping: " + typeVar + " in " + typeMappingScope);
        }
        return typeVar;
    }
    //endregion

    private void rewriteDefs(JCTree.JCClassDecl tree) {
        // only filter if the class is parameterized
        List<JCTree> filteredDefs = tree.defs;
        if (currentClass.hasNewGenerics() && currentClassTree.type.getTypeArguments().nonEmpty()) {
            // first, we filter out all the fields and blocks to avoid visiting them twice
            var filteredDefsBuffer = new ListBuffer<JCTree>();
            tree.defs.forEach(member -> {
                switch (member.getTag()) {
                    case VARDEF -> { // for field, we only filter out the INIT part
                        var field = (JCTree.JCVariableDecl) member;
                        filteredDefsBuffer.add(field);
                        if (!field.sym.isStatic() && field.init != null) {
                            inlineAndBlockDecls.add(make.Assignment(field.sym, field.init));
                            field.init = null;
                        }
                    }
                    case BLOCK -> {
                        var block = (JCTree.JCBlock) member;
                        if ((block.flags & Flags.STATIC) != 0) { // static blocks are ok
                            filteredDefsBuffer.add(block);
                        } else {
                            inlineAndBlockDecls.add(block);
                        }
                    }
                    default -> filteredDefsBuffer.add(member);
                }
            });
            filteredDefs = filteredDefsBuffer.toList();
        }

        filteredDefs.forEach(member -> {
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

                // fields init have been moved to constructors EXCEPT we are in an ignored class
                case VARDEF -> parameterizedMethodCallVisitor.visitField((JCTree.JCVariableDecl) member);
                // blocks here can only be fields without initialization and static blocks
                case BLOCK -> parameterizedMethodCallVisitor.visitClassBlock((JCTree.JCBlock) member);

                default -> throw new AssertionError("Unexpected member type: " + member.getTag());
            }
        });
    }

    private void rewriteConstructor(JCTree.JCMethodDecl tree) {
        var positions = types.typeArgPositions(tree.sym).fitEnumConstructorPositionsDecl(tree.params, syms);
        var oldEnclosingArgParamDecl = enclosingArgParamDecl;
        var oldEnclosingMethodTypeArgsParamDecl = enclosingMethodTypeArgsParamDecl;
        var oldInScopeBaseTypeParams = inScopeBaseTypeParams;
        try {
            if (!positions.hasNoArg()) {
                if (shouldProcessCurrentClass) {
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
                }

                // we need to shadow the class scope type parameters to only use the arg param and not potentially non
                // initialized fields
                if (positions.hasArg() && shouldProcessCurrentClass) {
                    enclosingArgParamDecl = tree.params.get(positions.argPosition()).sym;
                    var scopeTypeParameter = new ScopeTypeParameter(
                        currentClass.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                        constructorArgHandle(tree.sym, positions)
                    );
                    inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
                } else {
                    enclosingArgParamDecl = null;
                    inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(
                        new ScopeTypeParameter(
                            currentClass.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                            rawTypeArgHandle(tree.sym)
                        )
                    );
                }
                // we prepend the method type parameters to the scope AFTER the class type parameters because the method
                // type parameters has the precedence over the class type parameters
                if (positions.hasMethodTypeArg() && shouldProcessCurrentClass) { // TODO make it work with anonymous classes
                    enclosingMethodTypeArgsParamDecl = tree.params.get(positions.methodTypeArgPosition()).sym;
                    var scopeTypeParameter = new ScopeTypeParameter(
                        tree.sym.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                        basicMethodMArgHandle(tree.sym, positions)
                    );
                    inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
                } else {
                    enclosingMethodTypeArgsParamDecl = null;
                    inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(
                        new ScopeTypeParameter(
                            tree.sym.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                            rawTypeArgHandle(tree.sym)
                        )
                    );
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
                if (shouldProcessCurrentClass && !currentClass.isAnonymous() && positions.hasArg() && !doesCallOverload) {
                    setFieldsValues(tree);
                }
            }

            parameterizedMethodCallVisitor.visitMethod(tree);
        } finally {
            inScopeBaseTypeParams = oldInScopeBaseTypeParams;
            enclosingArgParamDecl = oldEnclosingArgParamDecl;
            enclosingMethodTypeArgsParamDecl = oldEnclosingMethodTypeArgsParamDecl;
        }
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
                var tsym = (Symbol.ClassSymbol) t.tsym;
                var isAccessible = isAccessible(tsym);
                var c = classTypeOfInvocation(-1, isAccessible);
                c.args = List.of(classArgParam(tsym, isAccessible));
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
        baseType.inferrenceMapping = methodType.asMethodType().inferrenceMapping;
        if (methodType.getTypeArguments().isEmpty()) {
            return baseType;
        }
        return new Type.ForAll(methodType.getTypeArguments(), baseType);
    }

    private void insertArgsInMethod(JCTree.JCMethodDecl method, Types.ArgPosition position) {
        var sym = method.sym;
        position.fitEnumConstructorPositionsDecl(method.params, syms).forEach(syms, (index, arg) -> {
            var extraArgParam = make.at(method.pos).Param(computeArgParamName(sym, arg), arg, sym);
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

            var call = parameterizedTypeOfInvocation(-1);
            var type = field.fst;
            var args = new ListBuffer<JCTree.JCExpression>();

            args.add(make.ClassLiteral((Symbol.ClassSymbol) type.tsym));

            type.getTypeArguments()
                .forEach(t -> args.add(generateArgs(type, t)));
            call.args = args.toList();
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

        superTypeParams.getTypeArguments().forEach(t -> {
            args.add(generateArgs(null, t.type));
        });

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
                    var resolution = typeVarResolution(symbol);
                    currentArgs.add(resolution);
                } else if (tree.sym instanceof Symbol.ClassSymbol symbol) {
                    // otherwise, the type is a concrete type (Foo<String>)

                    var isAccessible = isAccessible(symbol);
                    var classOfCall = classTypeOfInvocation(-1, isAccessible);
                    classOfCall.args = List.of(classArgParam(symbol, isAccessible));
                    currentArgs.add(classOfCall);
                }
                // last case is a packageSymbol, we do not need to do anything
            }

            @Override
            public void visitTypeApply(JCTree.JCTypeApply tree) {
                var classSymbol = (Symbol.ClassSymbol) tree.clazz.type.tsym;
                var isAccessible = isAccessible(classSymbol);
                var nestedCall = parameterizedTypeOfInvocation(-1, isAccessible);
                var oldArgs = currentArgs;
                try {
                    currentArgs = new ListBuffer<>();
                    currentArgs.add(classArgParam(classSymbol, isAccessible));
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
                var classOfCall = classTypeOfInvocation(-1, true);
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
                try {
                    // outer
                    super.visitSelect(tree);

                    // inner
                    var inner = (Symbol.ClassSymbol) tree.sym;
                    var isAccessible = isAccessible(inner);
                    var classOfCall = classTypeOfInvocation(-1, isAccessible);
                    classOfCall.args = List.of(classArgParam(inner, isAccessible));
                    currentArgs.add(classOfCall);
                } finally {
                    innerClassCall.args = currentArgs.toList();
                    currentArgs = oldArgs;
                }
            }


        }
        var visitor = new SuperArgBuildingVisitor();
        superTypeParams.forEach(type -> {
            debug("superTypeParams", type, type.type);
            type.accept(visitor);
        });
    }

    private void rewriteBasicMethod(JCTree.JCMethodDecl method) {
        if (isNative(method.sym)) {
            return;
        }

        var positions = types.typeArgPositions(method.sym)
            .doNotModifyIfOverriddesNotModifiedMethod(method.sym, types);

//        if (!shouldProcessCurrentClass && !method.sym.isPrivate() && positions.hasMethodTypeArg()) {
//            var doesOverrides = false;
//            var sym = method.sym;
//            for (var openMethod : openMethods) {
//                if (sym.overrides(openMethod, (Symbol.ClassSymbol) openMethod.owner, types, true)) {
//                    doesOverrides = true;
//                    break;
//                }
//            }
//            if (doesOverrides) {
//
//            }
//        }

        var oldInScopeBaseTypeParams = inScopeBaseTypeParams;
        if (method.sym.isStatic()) { // static methods do not have access to the class type parameters
            inScopeBaseTypeParams = List.nil();
        }
        try {
            if (positions.hasMethodTypeArg() && shouldProcessCurrentClass && !isIntrinsic(method.sym)) {
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
                    method.sym.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                    basicMethodMArgHandle(method.sym, positions)
                );
                inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(scopeTypeParameter);
            } else {
                inScopeBaseTypeParams = inScopeBaseTypeParams.prepend(
                    new ScopeTypeParameter(
                        method.sym.type.getTypeArguments().stream().map(t -> t.tsym).toList(),
                        rawTypeArgHandle(method.sym)
                    )
                );
            }

            // Here we should also check that the method is not in an inner class to really only target main methods, but
            // generate the flag for a wrong method is not a problem, so we can ignore it for now
            var isMain = method.sym.owner.hasNewGenerics()
                && method.sym.isStatic()
                && !method.sym.isPrivate() // public || package-private || protected
                && names.main.equals(method.sym.name)
                && !currentClass.flatName().toString().equals("com.sun.tools.javac.launcher.SourceLauncher");
            parameterizedMethodCallVisitor.visitMethod(method);
            if (isMain) {
                var end = mainEndReached(-1);
                end.args = List.of(mainId(method.pos));
                List<JCTree.JCStatement> finalList = List.of(make.Exec(end));

                var start = mainStartReached(-1);
                start.args = List.of(mainId(method.pos));
                finalList = finalList.prependList(method.body.stats);
                finalList = finalList.prepend(make.Exec(start));
                method.body.stats = finalList;
            }
        } finally {
            inScopeBaseTypeParams = oldInScopeBaseTypeParams;
        }
    }

    private final class InstructionVisitor extends TreeTranslator {

        private Type currentReturnType = null;
        // if this field is null it means we are in a class init
        private Symbol.MethodSymbol enclosingMethod;
        private boolean doGenerateVerification;

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            super.visitApply(tree);
            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) throw new AssertionError("No symbol for " + tree.meth);

            var methOwner = sym.owner;
            if (!methOwner.hasNewGenerics()) return; // if the owner does not have new generics, we have nothing to do

            var positions = types.typeArgPositions(sym).doNotModifyIfOverriddesNotModifiedMethod(sym, types);
            if (positions.hasNoArg()) return; // if the method does not have any type arguments, we have nothing to do

            // we should not process current class content but the called method has args, meaning that we must use the
            // backward compatibility overload
            if (!shouldProcessCurrentClass) {
                TreeInfo.setSymbol(tree.meth, useBackwardCompatibility(sym, positions));
                tree.meth.type = copyTypeAndRemoveTypeArgIfNeeded(tree.meth.type, positions);
                return;
            }

            if (sym.isConstructor()) {
                tree.args = insertAtArgPositions(
                    tree.args,
                    positions.fitEnumConstructorPositions(tree.args, syms),
                    kind -> switch (kind) {
                        case ARG -> {
                            var isSuper = TreeInfo.name(tree.meth) == names._super;
                            JCTree.JCExpression arg;
                            if (isSuper) {
                                var superClass = (Symbol.ClassSymbol) currentClass.getSuperclass().tsym;
                                if (!shouldProcessCurrentClass) {
                                    var call = rawTypeOfInvocation(-1);
                                    call.args = List.of(make.ClassLiteral(superClass));
                                    yield call;
                                }

                                if (currentClass.isAnonymous()) {
                                    arg = make.Ident(enclosingArgParamDecl);
                                } else {
                                    JCTree.JCTypeApply typeApply;
                                    // we need to generate a fake type apply for enums because the tree does not contain the 'extends' node
                                    if (syms.enumSym == superClass) {
                                        typeApply = enumTypeApply();
                                    } else {
                                        var extending = currentClassTree.extending;
                                        if (extending instanceof JCTree.JCTypeApply apply) {
                                            typeApply = apply;
                                        } else { // raw type
                                            var call = rawTypeOfInvocation(-1);
                                            call.args = List.of(make.ClassLiteral(superClass));
                                            yield call;
                                        }
                                    }
                                    arg = createSuperFromConcrete(typeApply);
                                }
                            } else { // `this` case (cannot happen in java.lang)
                                if (enclosingArgParamDecl == null) {
                                    throw new AssertionError("No enclosing arg param decl");
                                }
                                arg = make.Ident(enclosingArgParamDecl);
                            }
                            yield arg;
                        }
                        case METHOD_TYPE_ARG -> basicMethodArgConstruction(
                            sym,
                            tree.typeargs,
                            tree.meth.type.asMethodType().inferrenceMapping
                        );
                    }
                );
            } else {
                // we insert the args at the correct positions
                tree.args = insertAtArgPositions(tree.args, positions, kind -> switch (kind) {
                    case ARG -> throw new AssertionError("No Arg for method invocation");
                    case METHOD_TYPE_ARG -> basicMethodArgConstruction(
                        sym,
                        tree.typeargs,
                        tree.meth.type.asMethodType().inferrenceMapping
                    );
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

            // we should not process current class content but the called method has args, meaning that we must use the
            // backward compatibility overload
            if (!shouldProcessCurrentClass) {
                super.visitNewClass(tree);
                tree.constructor = useBackwardCompatibility(sym, positions);
                tree.constructorType = copyTypeAndRemoveTypeArgIfNeeded(tree.constructorType, positions);
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
                case ARG -> generateArgs(null, clazz);
                case METHOD_TYPE_ARG -> basicMethodArgConstruction(
                    sym,
                    tree.typeargs,
                    tree.constructorType.asMethodType().inferrenceMapping
                );
            });
            super.visitNewClass(tree);
        }

        @Override
        public void visitTypeCast(JCTree.JCTypeCast tree) {
            super.visitTypeCast(tree);
            if (!doGenerateVerification || inJavaUtilPtype || !shouldProcessCurrentClass || !shouldBeChecked(tree.clazz.type.tsym.type)) {
                return;
            }
            JCTree.JCMethodInvocation call;
            var args = generateArgs(null, tree.clazz.type);
            if (tree.expr.hasTag(JCTree.Tag.NEWARRAY)) { // if the cast is on a new array, we add the type arg
                call = externalMethodInvocation(
                    -1,
                    "addArrayTypeArg",
                    syms.typeArgUtils,
                    syms.objectType,
                    List.of(syms.objectType, syms.arrayTypeArgs)
                );
                // if the cast is not on a parameterized type, we have nothing to do
                call.args = List.of(tree.expr, args);
            } else if (tree.clazz.type.isPrimitive() || !tree.clazz.type.isParameterized()) {
                return;
            } else { // otherwise, we replace the cast
                call = checkCastInvocation(-1);
                var target = checkTarget(tree.clazz.type.tsym.type);
                call.args = List.of(tree.expr, args, checkCastLocation(tree.pos), make.Literal("CAST"), target);
            }
            tree.expr = call;
        }

        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) { // do not recurse on inner classes
            indices.visitLocalClass();
            rewriteClass(tree);
            result = tree;
        }

        @Override
        public void visitLambda(JCTree.JCLambda tree) {
            tree.scopeTypeParameters = inScopeBaseTypeParams;
            tree.typeMappingScope = typeMappingScope;
            var oldReturnType = currentReturnType;
            var addedCheck = false;
            try {
                currentReturnType = tree.getDescriptorType(types).asMethodType().getReturnType();
                if (shouldProcessCurrentClass && doGenerateVerification) {
                    // we first change the body to a block if it is an expression
                    JCTree.JCExpression expr = tree.body instanceof JCTree.JCExpression ex ? ex : null;

                    if (expr != null) {
                        var stmt = syms.voidType.equals(types.findDescriptorType(tree.type).getReturnType())
                            ? make.Exec(expr)
                            : make.Return(expr);
                        tree.body = make.at(tree.body.pos).Block(0L, List.of(stmt));
                    }

                    var body = (JCTree.JCBlock) tree.body;
                    var oldSize = body.stats.size();
                    body.stats = addParameterizedArgumentTypeChecking(tree.params, tree.pos, body.stats);
                    addedCheck = body.stats.size() > oldSize;

                    if (!addedCheck && expr != null) {
                        // if we did not add any check, we can remove the block
                        tree.body = expr;
                    }
                }
                super.visitLambda(tree);
            } finally {
                currentReturnType = oldReturnType;
            }
        }

        @Override
        public void visitReference(JCTree.JCMemberReference tree) {
            super.visitReference(tree);
            tree.scopeTypeParameters = inScopeBaseTypeParams;
            tree.typeMappingScope = typeMappingScope;
            var positions = types.typeArgPositions(tree.sym);
            if (!positions.hasNoArg() && (!tree.target.tsym.hasNewGenerics() || !shouldProcessCurrentClass)) {
                // impl has new generics AND takes an arg
                // but SAM does not have new generics OR we should not process the current class
                // we need to replace the sym by the backward compatibility overload
                // we directly create a symbol with the correct type in case the method has not been generated yet
                tree.sym = useBackwardCompatibility((Symbol.MethodSymbol) tree.sym, positions);
            } else if (!positions.hasNoArg()) {
                tree.sym = tree.sym.clone(tree.sym.owner);
                tree.sym.type = copyTypeAndInsertTypeArgIfNeeded(tree.sym.type, positions);
            }
        }

        @Override
        public void visitAssign(JCTree.JCAssign tree) {
            super.visitAssign(tree);
            if (
                !shouldProcessCurrentClass
                    || (tree.rhs.hasTag(JCTree.Tag.LITERAL)
                    && tree.rhs.type.hasTag(TypeTag.BOT))
            ) return;

            var actualLhs = TreeInfo.skipParens(tree.lhs);
            var sym = TreeInfo.symbol(actualLhs);
            // if the assign is not on a field or an array, we have nothing to do
            if (actualLhs.hasTag(JCTree.Tag.INDEXED)) {
                result = basicAssign(tree, actualLhs.type);
            } else if (sym != null && sym.owner.kind == Kinds.Kind.TYP) {
                result = putFieldAssign(tree, actualLhs, sym);
            }
            // no need to else because the super has already set result to tree
        }

        private JCTree.JCAssign basicAssign(JCTree.JCAssign base, Type lhsType) {
            if (!shouldBeChecked(lhsType)) return base;

            var call = checkCastInvocation(-1);
            var args = generateArgs(null, lhsType);
            call.args = List.of(
                base.rhs,
                args,
                checkCastLocation(base.pos),
                make.Literal("STORAGE"),
                checkTarget(lhsType.tsym.type)
            );

            base.rhs = call;
            return base;
        }

        private JCTree putFieldAssign(JCTree.JCAssign base, JCTree lhs, Symbol lhsSym) {
            if (!shouldBeChecked(lhsSym.type)) return base;
            // if the field is static or if it is in the current class, we can treat it as a basic assign
            if (lhsSym.owner == currentClass || lhsSym.isStatic()) return basicAssign(base, lhs.type);

            externalFieldOwner = lhsSym.owner;
            try {
                return switch (lhs) {
                    case JCTree.JCFieldAccess fieldAccess -> {
                        if (!TreeInfo.skipParens(fieldAccess.selected).hasTag(JCTree.Tag.APPLY)) {
                            if (
//                                base.rhs.hasTag(JCTree.Tag.SELECT)
//                                    || base.rhs.hasTag(JCTree.Tag.LITERAL)
                                (base.rhs.hasTag(JCTree.Tag.NEWCLASS) && currentClass.packge().fullname.toString().startsWith("java.util.concurrent"))
//                                    || base.rhs.hasTag(JCTree.Tag.NEWARRAY)
//                                    || base.rhs.hasTag(JCTree.Tag.TYPECAST)
                                    || (base.rhs.hasTag(JCTree.Tag.IDENT) && currentClass.packge().fullname.toString().startsWith("java.util.concurrent"))
//                                    || base.rhs.hasTag(JCTree.Tag.APPLY)
                            ) {
//                                if (base.rhs.hasTag(JCTree.Tag.IDENT)) {
//                                    debug(base.rhs.getTag(), currentClass, base);
//                                }
                                yield base;
                            }
                            externalSymbolAccess = fieldAccess.selected;
                            yield basicAssign(base, lhsSym.type);
                        }

                        var isParameterizedByClass = false;
                        for (var typeArgument : lhsSym.type.getTypeArguments()) {
                            // if the type is parameterized, we must use the type parameters of its owner
                            if (typeArgument.hasTag(TypeTag.TYPEVAR) || typeArgument.hasTag(TypeTag.WILDCARD)) {
                                isParameterizedByClass = true;
                                break;
                            }
                        }
                        if (!isParameterizedByClass) {
                            externalSymbolAccess = fieldAccess.selected;
                            yield basicAssign(base, lhs.type);
                        }

                        // if the owner of the field is the return of a function, we need to create a local variable to
                        // store it. To do so, we change the whole assign to a block.
                        var r = make.Block(0L, List.nil());
                        var ownerSymbolOwner = enclosingMethod != null ?
                            enclosingMethod :
                            new Symbol.MethodSymbol(
                                0L,
                                names.empty,
                                null,
                                currentClass
                            );
                        var ownerSymbol = new Symbol.VarSymbol(
                            0L,
                            names.fromString("$owner"),
                            fieldAccess.selected.type,
                            ownerSymbolOwner
                        );
                        var ownerDecl = make.VarDef(ownerSymbol, fieldAccess.selected);
                        externalSymbolAccess = make.Ident(ownerSymbol); // we set an identified to the owner as the access

                        var actualAssign = basicAssign(base, lhsSym.type.tsym.type);
                        actualAssign.lhs = make.Select(make.Ident(ownerSymbol), lhsSym);
                        r.stats = List.of(ownerDecl, make.Exec(actualAssign));
                        yield r;
                    }
                    case JCTree.JCIdent ignored -> {
                        externalSymbolAccess = make.This(currentClass.type); // not the correct type but we don't really care
                        yield basicAssign(base, lhsSym.type);
                    }
                    default -> throw new AssertionError("Unexpected assign lhs: " + lhs);
                };
            } finally {
                externalFieldOwner = null;
                externalSymbolAccess = null;
            }
        }

        private Symbol useBackwardCompatibility(Symbol.MethodSymbol current, Types.ArgPosition positions) {
            var newSym = current.clone(current.owner);
            newSym.type = copyTypeAndRemoveTypeArgIfNeeded(newSym.type, positions);
            return newSym;
        }

        @Override
        public void visitReturn(JCTree.JCReturn tree) {
            if (tree.expr == null) {
                result = tree;
                return;
            }
            var expr = tree.expr;

            var r = this.<JCTree>translate(tree.expr);
            var isBlock = r.hasTag(JCTree.Tag.BLOCK);
            if (isBlock) {
                var block = (JCTree.JCBlock) r;
                var assign = (JCTree.JCExpressionStatement) block.stats.tail.head;
                expr = assign.expr;
            }
            if (doGenerateVerification && shouldProcessCurrentClass && currentReturnType != null && shouldBeChecked(
                currentReturnType)) {
                expr = basicReturn(tree.pos, expr);
                tree.expr = expr;
            }
            if (isBlock) {
                var block = (JCTree.JCBlock) r;
                var decl = block.stats.head;
                result = make.Block(0L, List.of(decl, make.Return(expr).setType(tree.type)));
            } else {
                result = tree;
            }
        }

        private JCTree.JCExpression basicReturn(int pos, JCTree.JCExpression expr) {
            if (expr == null) {
                throw new AssertionError("Should not be null " + currentReturnType);
            }
            var call = checkCastInvocation(-1);
            var args = generateArgs(null, currentReturnType);
            call.args = List.of(expr, args, checkCastLocation(pos), make.Literal("EXIT"), checkTarget(currentReturnType.tsym.type));
            return call;
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

        @Override
        public void visitExec(JCTree.JCExpressionStatement tree) {
            JCTree r = translate(tree.expr);
            if (!r.hasTag(JCTree.Tag.BLOCK)) {
                tree.expr = (JCTree.JCExpression) r;
                result = tree;
            } else {
                result = r;
            }
        }

        public void visitMethod(JCTree.JCMethodDecl method) {
            if (method.body == null) { // abstract method
                return;
            }
            var oldReturnType = currentReturnType;
            var oldDoGenerateVerification = doGenerateVerification;
            var oldEnclosingMethod = enclosingMethod;
            doGenerateVerification = doVerify(method);
            try {
                enclosingMethod = method.sym;
                currentReturnType = method.sym.type.asMethodType().restype;
                if (shouldProcessCurrentClass && doGenerateVerification) {
                    method.body.stats = addParameterizedArgumentTypeChecking(method.params, method.pos, method.body.stats);
                }
                method.body.accept(this);
            } finally {
                currentReturnType = oldReturnType;
                doGenerateVerification = oldDoGenerateVerification;
                enclosingMethod = oldEnclosingMethod;
            }
        }

        public void visitClassBlock(JCTree.JCBlock block) {
            var oldScope = inScopeBaseTypeParams;
            if ((block.flags & Flags.STATIC) != 0) {
                inScopeBaseTypeParams = List.nil();
            }
            var oldEnclosingMethod = enclosingMethod;
            try {
                enclosingMethod = null;
                block.accept(this);
            } finally {
                inScopeBaseTypeParams = oldScope;
                enclosingMethod = oldEnclosingMethod;
            }
        }

        private boolean doVerify(JCTree.JCMethodDecl method) {
            var className = method.sym.owner.getQualifiedName();
            var methName = method.sym.name;
            if (className.contentEquals("java.util.concurrent.ConcurrentHashMap")) {
                return !(methName.contentEquals("tabAt") || methName.contentEquals("get") || methName.contentEquals(
                    "casTabAt"));
            }
            if (className.contentEquals("java.util.ArrayList")) {
                return !methName.contentEquals("iterator");
            }
            return true;
        }

        private List<JCTree.JCStatement> addParameterizedArgumentTypeChecking(
            List<JCTree.JCVariableDecl> params,
            int pos,
            List<JCTree.JCStatement> body
        ) {
            var instructions = body;
            for (JCTree.JCVariableDecl param : params.reverse()) {
                // we check parameterized type and type vars
                if (!shouldBeChecked(param.type)) {
                    continue;
                }

                var arg = generateArgs(null, param.type);
                var cast = checkCastInvocation(-1);
                cast.args = List.of(make.Ident(param.sym), arg, checkCastLocation(pos), make.Literal("ENTRY"), checkTarget(param.type.tsym.type));

                instructions = instructions.prepend(make.Exec(cast));
            }
            return instructions;
        }

        private boolean shouldBeChecked(Type type) {
            if (type instanceof Type.ArrayType arrayType) return shouldBeChecked(arrayType.elemtype);
            if (type instanceof Type.ClassType && !type.tsym.hasNewGenerics()) return false;
            return type instanceof Type.TypeVar || type.isParameterized();
        }

        /**
         * This method assumes that the type has new generics, should be checked and if it is a class it is assumed to
         * be a parameterized type.
         */
        private JCTree.JCExpression checkTarget(Type type) {
            return make.Literal(target(type));
        }

        private String target(Type type) {
            return switch (type) {
                case Type.TypeVar ignored -> "TYPE_PARAMETER";
                case Type.ClassType ignored -> "PARAMETERIZED_TYPE";
                case Type.ArrayType arrayType -> target(arrayType.elemtype);
                case null, default -> throw new AssertionError("Unexpected type: " + type);
            };
        }

        private JCTree.JCLiteral checkCastLocation(int pos) {
            var line = env.toplevel.lineMap.getLineNumber(pos);
            // id!module/flatname.method(sourceFileName.java:line)
            var module = currentClass.packge().modle;
            var stringLocation = locationCount++ +
                "!" +
                (module != null && module.name != null && !module.name.isEmpty() ? (module.name + "/") : "") +
                currentClass.flatName() +
                "." +
                (enclosingMethod != null ? enclosingMethod.name : names.clinit) +
                "(" +
                env.enclClass.name +
                ".java:" +
                line +
                ")";
            return make.Literal(stringLocation);
        }


    }

    private JCTree.JCLiteral mainId(int pos) {
        var line = env.toplevel.lineMap.getLineNumber(pos);
        var module = currentClass.packge().modle;
        var stringLocation =
            (module != null && module.name != null && !module.name.isEmpty() ? (module.name + "/") : "") +
                currentClass.flatName() +
                "(" +
                line +
                ")";
        return make.Literal(stringLocation);
    }

    private JCTree.JCMethodInvocation basicMethodArgConstruction(
        Symbol sym,
        List<JCTree.JCExpression> explicitTypes,
        List<Pair<Type, Type>> paramToArgument
    ) {
        var call = methodTypeArgsInvocation(-1);
        // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
        // use the inferred types `String s = foo();`
        if (explicitTypes.isEmpty()) { // inference
            var buffer = new ListBuffer<Type>();
            if (paramToArgument != null) {
                sym.type.getTypeArguments().forEach(t -> {
                    var tsym = t.tsym;
                    Type res = null;
                    for (var pair : paramToArgument) { // we try to find the type in the map
                        if (pair.fst.tsym == tsym) {
                            res = pair.snd;
                            break;
                        }
                    }
                    if (res == null) { // if no type is found, it means that the type is an unbound wildcard
                        var upperBound = tsym.type.getUpperBound();
                        res = new Type.WildcardType(
                            upperBound,
                            upperBound == syms.objectType ? BoundKind.UNBOUND : BoundKind.EXTENDS,
                            null
                        );
                    }
                    buffer.prepend(res);
                });
            } else { // special case when rawtypes are involved (seems to be the reason for the mapping being null so far)
                sym.type.getTypeArguments().forEach(t -> buffer.prepend(t.getUpperBound()));
            }
            var list = buffer.toList();
            call.args = argTypeParam(list);
        } else { // provided type arguments
            var buffer = new ListBuffer<JCTree.JCExpression>();
            explicitTypes.forEach(t -> buffer.add(generateArgs(null, t.type)));
            call.args = buffer.toList();
        }
        return call;
    }

    public JCTree.JCMethodInvocation basicMethodArgConstruction(
        Symbol sym,
        List<Pair<Type, Type>> paramToArgument,
        Env<AttrContext> env,
        TreeMaker make,
        List<ScopeTypeParameter> scope,
        List<Map<Symbol, Type>> typeMappingScope
    ) {
        Objects.requireNonNull(sym);
        Objects.requireNonNull(paramToArgument);
        Objects.requireNonNull(env);
        Objects.requireNonNull(make);
        Objects.requireNonNull(scope);
        Objects.requireNonNull(typeMappingScope);
        var oldEnv = this.env;
        var oldMake = this.make;
        var oldScope = this.inScopeBaseTypeParams;
        var oldTypeMappingScope = this.typeMappingScope;
        try {
            this.env = env;
            this.make = make;
            this.inScopeBaseTypeParams = scope;
            this.typeMappingScope = typeMappingScope;
            return basicMethodArgConstruction(sym, List.nil(), paramToArgument);
        } finally {
            this.env = oldEnv;
            this.make = oldMake;
            this.inScopeBaseTypeParams = oldScope;
            this.typeMappingScope = oldTypeMappingScope;
        }
    }

    //region type arg factories calls factories
    public JCTree.JCMethodInvocation enumGenericMethodTypeArg(
        Symbol.ClassSymbol classSymbol,
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
            var isAccessible = isAccessible(classSymbol);
            var classTypeCall = classTypeOfInvocation(-1, isAccessible);
            classTypeCall.args = List.of(classArgParam(classSymbol, isAccessible));
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
        var methodCall = make.Apply(List.nil(), methFactory.apply(make, sym), List.nil()).setType(returnType);
        if (sym.isVarArgs()) {
            methodCall.varargsElement = ((Type.ArrayType) sym.params.last().type).elemtype;
        }
        return methodCall;
    }

    private JCTree.JCMethodInvocation mainStartReached(int pos) {
        return externalMethodInvocation(
            pos,
            "mainStartReached",
            syms.typeOperationsType,
            syms.voidType,
            List.of(syms.stringType)
        );
    }

    private JCTree.JCMethodInvocation mainEndReached(int pos) {
        return externalMethodInvocation(
            pos,
            "mainEndReached",
            syms.typeOperationsType,
            syms.voidType,
            List.of(syms.stringType)
        );
    }


    private JCTree.JCMethodInvocation rawTypeOfInvocation(int pos) {
        return rawTypeOfInvocation(pos, true);
    }

    private JCTree.JCMethodInvocation rawTypeOfInvocation(int pos, boolean accessible) {
        var arg = accessible ? syms.classType : syms.stringType;
        return externalMethodInvocation(
            pos,
            "of",
            syms.rawTypeTypeArgs,
            syms.rawTypeTypeArgs,
            List.of(arg)
        );
    }

    private JCTree.JCMethodInvocation parameterizedTypeOfInvocation(int pos) {
        return parameterizedTypeOfInvocation(pos, true);
    }

    private JCTree.JCMethodInvocation parameterizedTypeOfInvocation(int pos, boolean accessible) {
        var arg = accessible ? syms.classType : syms.stringType;
        return externalMethodInvocation(
            pos,
            "of",
            syms.parameterizedTypeTypeArgs,
            syms.parameterizedTypeTypeArgs,
            List.of(arg, types.makeArrayType(syms.argBaseType))
        );
    }

    private JCTree.JCMethodInvocation classTypeOfInvocation(int pos, boolean accessible) {
        var arg = accessible ? syms.classType : syms.stringType;
        return externalMethodInvocation(pos, "of", syms.classTypeArgs, syms.classTypeArgs, List.of(arg));
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
            List.of(syms.objectType, syms.argBaseType, syms.stringType, syms.stringType, syms.stringType)
        );
    }
    //endregion

    //region type arg name computation
    private Name computeArgName(Symbol.ClassSymbol owner) {
        var synChar = target.syntheticNameChar();
        var pkg = owner.packge().fullname.toString().replace('.', synChar);
        var builder = new StringBuilder();
        builder.append('0')
            .append(synChar)
            .append("typeArgs")
            .append(synChar)
            .append(pkg)
            .append(synChar);
        recursivelyAppendEnclosingClasses(owner, builder, indices.iterator());
        return names.fromString(builder.toString());
    }

    private void recursivelyAppendEnclosingClasses(
        Symbol.ClassSymbol current,
        StringBuilder builder,
        Iterator<Integer> localClassIndexIterator
    ) {
        var encl = enclClass(current);
        if (encl != null) {
            recursivelyAppendEnclosingClasses(encl, builder, localClassIndexIterator);
        }
        builder.append(target.syntheticNameChar());
        if (localClassIndexIterator.hasNext() && current.owner.kind.matches(Kinds.KindSelector.VAL_MTH)) {
            builder.append(localClassIndexIterator.next());
        }
        builder.append(current.getSimpleName().toString());
    }

    private Symbol.ClassSymbol enclClass(Symbol.ClassSymbol current) {
        Symbol c = current;
        while (c != null) {
            var encl = c.owner;
            if (encl instanceof Symbol.ClassSymbol cl) {
                return cl;
            }
            c = encl;
        }
        return null;
    }

    public Name computeArgParamName(Symbol owner, Type kind) {
        Objects.requireNonNull(owner);
        Objects.requireNonNull(kind);
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

    private List<JCTree.JCExpression> argTypeParam(List<Type> types) {
        var buffer = new ListBuffer<JCTree.JCExpression>();
        types.forEach(t -> buffer.prepend(generateArgs(null, t)));
        return buffer.toList();
    }

    public JCTree.JCExpression generateArgs(
        Symbol.ClassSymbol currentClass,
        Type type,
        Env<AttrContext> env,
        TreeMaker make,
        List<ScopeTypeParameter> scope,
        List<Map<Symbol, Type>> typeMappingScope
    ) {
        Objects.requireNonNull(currentClass);
        Objects.requireNonNull(type);
        Objects.requireNonNull(env);
        Objects.requireNonNull(make);
        Objects.requireNonNull(scope);
        Objects.requireNonNull(typeMappingScope);
        var oldCurrentClass = this.currentClass;
        var oldEnv = this.env;
        var oldMake = this.make;
        var oldScope = this.inScopeBaseTypeParams;
        var oldTypeMappingScope = this.typeMappingScope;
        try {
            this.currentClass = currentClass;
            this.env = env;
            this.make = make;
            this.inScopeBaseTypeParams = scope;
            this.typeMappingScope = typeMappingScope;
            return generateArgs(null, type);
        } finally {
            this.currentClass = oldCurrentClass;
            this.env = oldEnv;
            this.make = oldMake;
            this.inScopeBaseTypeParams = oldScope;
            this.typeMappingScope = oldTypeMappingScope;
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
                var c = classTypeOfInvocation(-1, true);
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
                var tsym = current.tsym;
                var isAccessible = isAccessible(tsym);
                var classFieldAcc = classArgParam((Symbol.ClassSymbol) tsym, isAccessible);
                if (current.isRaw()) { // Foo(raw)
                    var rawTypeCall = rawTypeOfInvocation(-1, isAccessible);
                    rawTypeCall.args = List.of(classFieldAcc);
                    yield rawTypeCall;
                } else if (current.getTypeArguments().nonEmpty()) { // Foo<E> (E can be a wildcard)
                    var call = parameterizedTypeOfInvocation(-1, isAccessible);
                    var buffer = new ListBuffer<JCTree.JCExpression>();
                    buffer.add(classFieldAcc);
                    var ctype = (Type.ClassType) current;
                    ctype.typarams_field.forEach(param -> buffer.add(generateArgs(current, param)));
                    call.args = buffer.toList();
                    yield call;
                } else { // Foo
                    var call = classTypeOfInvocation(-1, isAccessible);
                    call.args = List.of(classFieldAcc);
                    yield call;
                }
            }
            case Type.CapturedType capturedType -> {
                var bound = capturedType.getUpperBound();
                if (capturedType.wildcard.isUnbound()) {
                    var call = wildcardTypeOfInvocation(false, -1);
                    var c = classTypeOfInvocation(-1, true);
                    c.args = List.of(make.ClassLiteral(syms.objectType));
                    call.args = List.of(c);
                    yield call;
                }
                if (bound.equals(previous)) { // to avoid infinite recursion on recursive types
                    var call = classTypeOfInvocation(-1, true);
                    call.args = List.of(make.ClassLiteral(syms.objectType));
                    yield call;
                }
                yield generateArgs(previous, bound);
            }
            case Type.TypeVar typeVar -> {
                var actual = findType(typeVar);
                if (actual != typeVar) { // if a mapping is provided, try to find the actual type var
                    yield generateArgs(previous, actual);
                }
                var owner = typeVar.tsym.owner;
                // if the owner of this type does not have it in its declared type parameters, it is a wildcard
                var index = owner.type.getTypeArguments().indexOf(typeVar);
                if (index == -1) { // wildcard
                    var call = wildcardTypeOfInvocation(false, -1);
                    var classCall = classTypeOfInvocation(-1, true);
                    classCall.args = List.of(make.ClassLiteral(syms.objectType));
                    call.args = List.of(classCall);
                    yield call;
                }
                yield typeVarResolution(typeVar.tsym);
            }
            case Type.JCPrimitiveType primitiveType -> {
                var classFieldAcc = make.ClassLiteral(primitiveType);
                var call = classTypeOfInvocation(-1, true);
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

        res.type = syms.argBaseType;
        return res;
    }

    private JCTree.JCTypeApply enumTypeApply() {
        return make.TypeApply(make.Ident(syms.enumSym), List.of(make.Ident(currentClass)));
    }

    /**
     * If the baseType parameter does not contain the expectedArg at the index position, we copy the baseType and insert
     * the expectedArg at the index position. Otherwise, we return the baseType.
     */
    public Type copyTypeAndInsertTypeArgIfNeeded(Type baseType, Types.ArgPosition positions) {
        Objects.requireNonNull(baseType);
        Objects.requireNonNull(positions);
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

    private Type copyTypeAndRemoveTypeArgIfNeeded(Type baseType, Types.ArgPosition positions) {
        if (positions.hasNoArg()) return baseType;
        var copy = copyMethodType(baseType);
        var copyMtype = copy.asMethodType();
        copyMtype.argtypes = removeAtIndex(copyMtype.argtypes, (i, current) -> {
            var atPos = positions.atPos(i, syms);
            // only remove if atPos exists and is equal to the current type
            return atPos != null && types.isSameType(current, atPos);
        });
        return copy;
    }

    public Type copyTypeAndInsertTypeArgIfNeeded(Symbol symbol, Type baseType) {
        Objects.requireNonNull(symbol);
        Objects.requireNonNull(baseType);
        return copyTypeAndInsertTypeArgIfNeeded(baseType, types.typeArgPositions(symbol));
    }

    //region type variable resolution
    private JCTree.JCExpression typeVarResolution(Symbol typeVar) {
        ScopeTypeParameter foundScope = null;
        int foundIndex = -1;
        for (var current = inScopeBaseTypeParams; current.nonEmpty(); current = current.tail) {
            var scope = current.head;
            var index = scope.indexOf(typeVar);
            if (index != -1) {
                foundScope = scope;
                foundIndex = index;
                break;
            }
        }

        if (foundScope != null) {
            return foundScope.argAccessFactory().apply(foundIndex);
        }

        if (externalFieldOwner == null) {
            throw new AssertionError("Could not find type var " + typeVar + " in class " + currentClass + " with the following scope " + inScopeBaseTypeParams);
        }

        var argAccessingCall = externalMethodInvocation(
            -1,
            "getArg",
            syms.typeArgUtils,
            syms.argBaseType,
            List.of(syms.objectType, syms.classType)
        );
        argAccessingCall.args = List.of(
            externalSymbolAccess,
            make.ClassLiteral((Symbol.ClassSymbol) externalFieldOwner)
        );

        var index = typeVar.owner.type.getTypeArguments().stream().map(t -> t.tsym).toList().indexOf(typeVar);
        // then a second getArg on the retrieved Arg to get the actual type parameter value
        var getInnerArgCall = getGetArgInvocation(-1);
        getInnerArgCall.args = List.of(argAccessingCall, make.Literal(index));
        return getInnerArgCall;
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

    private IntFunction<JCTree.JCMethodInvocation> rawTypeArgHandle(Symbol classSymbol) {
        var types = classSymbol.type.getTypeArguments();
        return index -> {
            var t = (Type.TypeVar) types.get(index).tsym.type;
            var upperBound = t.getUpperBound();
            var isAccessible = isAccessible(upperBound.tsym);
            var call = upperBound.tsym.type.getTypeArguments().nonEmpty()
                ? rawTypeOfInvocation(-1, isAccessible)
                : classTypeOfInvocation(-1, isAccessible);
            call.args = List.of(classArgParam(upperBound, isAccessible));
            return call;
        };
    }

    /**
     * This class is used to represent the scope of type parameters. Anytime a type parameter is referenced, we need to
     * know its actual value. To do so each time a type parameter is declared (either in a method or a class), we create
     * a new ScopeTypeParameter instance that will store all the names of the type parameters declared in the scope, and
     * a factory that generates the method call to get the actual value of the type parameter.
     */
    public static final class ScopeTypeParameter {

        // we could use a HashMap<Symbol, Integer> to avoid the indexOf calls but as most classes have few type parameters,
        // it does not change much
        private final java.util.List<? extends Symbol> variableParams;
        private final IntFunction<? extends JCTree.JCExpression> argAccessFactory;

        private ScopeTypeParameter(
            java.util.List<? extends Symbol> variableParams,
            IntFunction<? extends JCTree.JCExpression> argAccessFactory
        ) {
            this.variableParams = java.util.List.copyOf(variableParams);
            this.argAccessFactory = argAccessFactory;
        }

        public int indexOf(Symbol name) {
            return variableParams.indexOf(name);
        }

        private IntFunction<? extends JCTree.JCExpression> argAccessFactory() {
            return argAccessFactory;
        }

        @Override
        public String toString() {
            return variableParams.toString();
        }
    }
    //endregion

    //region ignored class inheritance problem
    private java.util.List<Symbol.MethodSymbol> openMethods() {
        if (shouldProcessCurrentClass) return java.util.List.of();

        var methods = new ArrayList<Symbol.MethodSymbol>();

        var superClass = types.supertype(currentClass.type);
        while (superClass.hasTag(TypeTag.CLASS)) {
            addOpenMethods(methods, superClass);
            addInterfaceOpenMethods(methods, superClass);
            superClass = types.supertype(superClass);
        }

        types.interfaces(currentClass.type).forEach(i -> addInterfaceOpenMethods(methods, i));

        return methods;
    }

    private void addInterfaceOpenMethods(ArrayList<Symbol.MethodSymbol> list, Type interfce) {
        addOpenMethods(list, interfce);
        types.interfaces(interfce).forEach(i -> addInterfaceOpenMethods(list, i));
    }

    private void addOpenMethods(ArrayList<Symbol.MethodSymbol> list, Type clazz) {
        clazz.tsym.members()
            .getSymbols(
                s ->
                    s.kind == Kinds.Kind.MTH
                        && s.owner.hasNewGenerics()
                        && !s.isFinal()
                        && !s.isPrivate()
                        && s.type.getTypeArguments().nonEmpty(),
                Scope.LookupKind.NON_RECURSIVE
            )
            .forEach(s -> list.add((Symbol.MethodSymbol) s));
    }
    //endregion

    //region utils
    private JCTree.JCExpression classArgParam(Symbol.ClassSymbol sym, boolean isAccessible) {
        return isAccessible ? make.ClassLiteral(sym) : make.Literal(sym.flatname.toString());
    }

    private JCTree.JCExpression classArgParam(Type type, boolean isAccessible) {
        return isAccessible ? make.ClassLiteral(type) : make.Literal(((Symbol.ClassSymbol) type.tsym).flatname.toString());
    }

    private boolean isAccessible(Symbol.TypeSymbol sym) {
        return rs.isAccessible(env, sym);
    }

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
        symbolCopy.flags_field |= Flags.SYNTHETIC;// | Flags.NEW_GENERICS;
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

        // we must remove record flag because Lower will init fields in the first encountered constructor
        // that has the record flag
        treeCopy.mods.flags = (treeCopy.mods.flags & ~(Flags.RECORD | Flags.GENERATEDCONSTR));
        symbolCopy.flags_field = (symbolCopy.flags_field & ~(Flags.RECORD | Flags.GENERATEDCONSTR));

        if (baseMethod.sym.isAbstract()) {
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

    private interface IntBiPredicate<T> {
        boolean test(int i, T t);
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

    private static <T> List<T> removeAtIndex(List<T> list, IntBiPredicate<T> positionMapper) {
        var buffer = new ListBuffer<T>();
        var i = 0;
        for (var t : list) {
            if (!positionMapper.test(i++, t)) {
                buffer.add(t);
            }
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

    private boolean isNative(Symbol sym) {
        return (sym.flags_field & Flags.NATIVE) != 0;
    }

    private boolean isIntrinsic(Symbol sym) {
        for (var annotation : sym.getAnnotationMirrors()) {
            if (names.intrinsicCandidate.equals(annotation.type.tsym.name)) {
                return true;
            }
        }
        return false;
    }

    private static class LocalClassIndex {
        private int currentDepth = -1; // -1 = toplevel class
        private ArrayList<Integer> indexes = new ArrayList<>();

        public void visitLocalClass() {
            indexes.set(currentDepth, indexes.get(currentDepth) + 1);
        }

        public void pushIndex() {
            indexes.add(0);
            currentDepth++;
        }

        public void popIndex() {
            indexes.removeLast();
            currentDepth--;
        }

        public Iterator<Integer> iterator() {
            return new Iterator<>() {
                private int i = 0;

                @Override
                public boolean hasNext() {
                    return i < currentDepth && indexes.get(i) > 0;
                }

                @Override
                public Integer next() {
                    return indexes.get(i++);
                }
            };
        }

    }
    //endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree cdef, TreeMaker make) {
        try {
            this.make = make;
            this.env = env;
            this.treeCopier = new JCTreeCopier(make);
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
