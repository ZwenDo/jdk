package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.*;
import com.sun.tools.javac.jvm.ByteCodes;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.*;
import com.sun.tools.javac.util.List;

import javax.lang.model.element.ElementKind;
import javax.lang.model.type.TypeKind;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.sun.tools.javac.code.Flags.*;

public final class TransParameterizedTypes {

    //region fields
    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private final boolean enabled;

    private final InstructionVisitor parameterizedMethodCallVisitor;
    private final ArgLiteralGenerator argLiteralGenerator;
    private ConstantHolder constantHolder;

    private final Log log;
    private final Symtab syms;
    private final Names names;
    private final Resolve resolve;
    private final Translator translator;
    private final Types types;
    private final Operators operators;


    private TreeMaker make;
    private Env<AttrContext> env;
    private Symbol.ClassSymbol currentClass;
    private Symbol.VarSymbol currentClassArgField;
    private JCTree.JCClassDecl currentClassTree;

    /**
     * Stack containing the different type args declared at any point in the code. It allows when a generic type is
     * required, to find where it comes from and generate access to it.
     */
    private ParameterizedScope typeParameterScopes;
    /**
     * A snapshot used for each method, to ensure correct declaration and usage of local variables.
     */
    private ParameterizedScope.GroupState typeParameterScopeGroupState;

    /**
     * Stack containing the mappings between a symbol and its type. It is used to map type variables from transitive
     * super types to their actual type.
     * <p>
     * Here is an example of a mapping:
     * <p>
     * A&lt;T&gt; implements B&lt;E&gt; and B&lt;E&gt; implements C&lt;F&gt;
     * <p>
     * this map provides the following mappings:
     * <ul>
     *     <li>T -> T (self referencing)</li>
     *     <li>E -> T</li>
     *     <li>F -> T (flattened transitive relation F -> E -> T)</li>
     * </ul>
     * This is later used for retrieve the actual argument from the current scope
     */
    private List<Map<Symbol, Type>> typeMappingScope;

    /**
     * Field containing all the fields and blocks that are declared in the current class. These declarations must be
     * moved to the constructor to be able to access the method parameters.
     */
    private ArrayList<JCTree.JCStatement> inlineAndBlockDecls;

    /**
     * If we are visiting a class, this field contains all the supertypes for which we need to generate an arg in the
     * constructor.
     */
    private java.util.List<Type> superTypes;

    /// Simple value appended to generated local variable names to easily identify them (e.g. args$34). We assume that
    /// there will never be more than 99.
    private int nameOffsetIndex;

    private final class ConstantHolder {

        public final Name methodTypeArgumentsLocalVarName = names.fromString("methodArguments");

        public final Name objectTypeArgumentsLocalVarName = names.fromString("objectArguments");

        public final Name objectTypeArgumentsFieldName = names.fromString("$typeArguments");

        public final Name rawTypeClassInstance = names.fromString("$RAW_TYPE_INSTANCE");

        public final Name constructorTypeArgumentsLocalVarName = names.fromString("constructorArgument");

        public final Symbol.MethodSymbol methodTypeArgumentsAccessMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("methodTypeArguments"),
                new Type.MethodType(List.of(syms.classType), syms.specializedMethodTypeArgumentsType, List.nil(), syms.methodClass),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol constructorTypeArgumentsAccessMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("constructorTypeArguments"),
                new Type.MethodType(List.nil(), syms.specializedTypeType, List.nil(), syms.methodClass),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol extractTypeArgumentMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC | VARARGS,
                names.fromString("extract"),
                new Type.MethodType(
                        List.of(syms.specializedTypeContainerType, types.makeArrayType(syms.intType)),
                        syms.specializedTypeType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypeUtilsType.tsym
        );

        public final Symbol.MethodSymbol extractAsSuperMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("extractAsSuper"),
                new Type.MethodType(
                        List.of(syms.objectsType, syms.classType),
                        syms.specializedTypeType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypeUtilsType.tsym
        );

        public final Symbol.MethodSymbol arrayTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC,
                names.init,
                new Type.MethodType(List.of(syms.specializedTypeType), syms.voidType, List.nil(), syms.methodClass),
                syms.specializedArrayType.tsym
        );

        public final Symbol.MethodSymbol classTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC,
                names.init,
                new Type.MethodType(List.of(syms.classType), syms.voidType, List.nil(), syms.methodClass),
                syms.classTypeType.tsym
        );

        public final Symbol.MethodSymbol innerClassTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC,
                names.init,
                new Type.MethodType(List.of(syms.specializedTypeType, syms.specializedTypeType), syms.voidType, List.nil(), syms.methodClass),
                syms.innerClassType.tsym
        );

        public final Symbol.MethodSymbol wildcardTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(syms.booleanType, types.makeArrayType(syms.specializedTypeType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.wildcardType.tsym
        );

        public final Symbol.MethodSymbol parameterizedTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(syms.classType, types.makeArrayType(syms.specializedTypeType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.parameterizedTypeType.tsym
        );

        public final Symbol.MethodSymbol intersectionTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC,
                names.init,
                new Type.MethodType(
                        List.of(types.makeArrayType(syms.specializedTypeType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.intersectionType.tsym
        );

        public final Symbol.MethodSymbol classTypeStringConstructor = new Symbol.MethodSymbol(
                PUBLIC,
                names.init,
                new Type.MethodType(List.of(syms.stringType), syms.voidType, List.nil(), syms.methodClass),
                syms.classType.tsym
        );

        public final Symbol.MethodSymbol methodTypeArgsConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(types.makeArrayType(syms.specializedTypeType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedMethodTypeArgumentsType.tsym
        );

        public final Symbol.MethodSymbol parameterizedTypeStringConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(syms.stringType, types.makeArrayType(syms.specializedTypeType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.parameterizedTypeType.tsym
        );

        public final Symbol.MethodSymbol erasedTypeInstanceMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("instance"),
                new Type.MethodType(
                        List.nil(),
                        syms.erasedTypeType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.erasedTypeType.tsym
        );

        public final Symbol.MethodSymbol argStackWalkerMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("walker"),
                new Type.MethodType(List.nil(), syms.stackWalkerType, List.nil(), syms.methodClass),
                syms.specializedTypeUtilsType.tsym
        );

        public final Symbol.MethodSymbol stackWalkerGetCallerClassMethod = new Symbol.MethodSymbol(
                PUBLIC,
                names.fromString("getCallerClass"),
                new Type.MethodType(List.nil(), syms.classType, List.nil(), syms.methodClass),
                syms.stackWalkerType.tsym
        );

        public final Symbol.MethodSymbol preMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("pre"),
                new Type.MethodType(
                        List.of(syms.voidObjectType, syms.voidObjectType, syms.objectType),
                        syms.objectType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol postMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("post"),
                new Type.MethodType(
                        List.of(syms.objectType, syms.voidObjectType, syms.voidObjectType),
                        syms.objectType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol pushMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("pushMethod"),
                new Type.MethodType(
                        List.of(syms.specializedMethodTypeArgumentsType, syms.classType),
                        syms.voidObjectType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol pushConstructor = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("pushConstructor"),
                new Type.MethodType(
                        List.of(syms.specializedTypeType),
                        syms.voidObjectType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.OperatorSymbol objectEqOperator = operators
                .lookupBinaryOp(o -> o.opcode == ByteCodes.if_acmpeq);

    }

    private ConstantHolder constantHolder() {
        if (constantHolder == null) constantHolder = new ConstantHolder();
        return constantHolder;
    }

    @SuppressWarnings("this-escape")
    private TransParameterizedTypes(Context context) {
        context.put(typeReifierKey, this);
        make = TreeMaker.instance(context);
        parameterizedMethodCallVisitor = new InstructionVisitor();
        translator = new Translator();
        log = Log.instance(context);
        syms = Symtab.instance(context);
        names = Names.instance(context);
        resolve = Resolve.instance(context);
        types = Types.instance(context);
        operators = Operators.instance(context);
        typeParameterScopes = new ParameterizedScope();
        typeMappingScope = List.nil();
        argLiteralGenerator = new ArgLiteralGenerator();
        var options = Options.instance(context);
        enabled = options.isSet("enableSpecialization");
    }

    /**
     * Get the instance for this context.
     */
    public static TransParameterizedTypes instance(Context context) {
        var instance = context.get(typeReifierKey);
        if (instance == null) instance = new TransParameterizedTypes(context);
        return instance;
    }
    //endregion

    public static boolean newGenericsExcluded(Symbol.TypeSymbol clazz) {
        Objects.requireNonNull(clazz);
        var module = clazz.packge().modle;
        var moduleName = module.getQualifiedName().toString();
        return moduleName.startsWith("jdk")
                || moduleName.startsWith("java")
                || moduleName.startsWith("utils");

//        var packageName = clazz.packge().getQualifiedName().toString();
//        var fullName = clazz.fullname.toString();
//        return packageName.startsWith("java.lang")
//                || packageName.startsWith("java.util.ptype")
//                || packageName.startsWith("java.util.concurrent")
//                || packageName.startsWith("jdk.internal")
//                || packageName.startsWith("java.security")
//                || packageName.startsWith("build.tools.classlist")
//                || packageName.startsWith("sun.reflect.generics")
//                || "java.util.WeakHashMap".equals(fullName)
//                || (clazz.owner.getKind().isClass() && newGenericsExcluded((Symbol.ClassSymbol) clazz.owner));
    }

    //region rewriting (class)
    private final class Translator extends TreeTranslator {

        /**
         * this method is only called for the top-level class
         */
        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) {
            result = tree;

            if (!enabled || tree.sym.isNewGenericsExcluded()) return;
            try {
                rewriteClass(tree);
            } catch (Exception | AssertionError t) {
                debug("error in class: " + tree.sym.fullname);
                throw t;
            }
        }

    }

    private void rewriteClass(JCTree.JCClassDecl tree) {
        var oldCurrentClassTree = currentClassTree;
        var oldInlineAndBlockDecls = inlineAndBlockDecls;
        var oldTypeParameterScopes = typeParameterScopes;
        var oldTypeMappingScope = typeMappingScope;
        var oldSuperTypes = superTypes;
        var oldCurrentClassArgField = currentClassArgField;

        try {
            currentClass = tree.sym;
            currentClassTree = tree;
            inlineAndBlockDecls = new ArrayList<>();
            // static classes cannot access the outer class type parameters, so we remove them
            if (tree.sym.isStatic()) {
                typeParameterScopes = new ParameterizedScope();
                typeMappingScope = List.nil();
            }

            if (isParameterized(tree.sym)) {
                // do not reorder these instructions.
                currentClassArgField = createArgFieldSymbol();
                if (tree.sym.isInterface()) {
                    typeParameterScopes.pushInterfaceGroup(getTypeArguments(tree.sym));
                } else {
                    typeParameterScopes.pushClassGroup(getTypeArguments(tree.sym));
                }
                // for classes, we prepare the list of supertypes to init in the constructor
                if (!tree.sym.isInterface()) {
                    superTypes = prepareScopes(tree);
                }
            }

            rewriteDefs(tree);

            // last thing is to add the field if needed
            if (isParameterized(tree.sym) && !tree.sym.isInterface()) {
                tree.defs = tree.defs.prepend(make.VarDef(currentClassArgField, null));
                currentClass.members().enterIfAbsent(currentClassArgField);
            }
        } finally {
            if (isParameterized(tree.sym)) {
                typeParameterScopes.pop();
            }
            currentClass = oldCurrentClassTree != null ? oldCurrentClassTree.sym : null;
            currentClassTree = oldCurrentClassTree;
            inlineAndBlockDecls = oldInlineAndBlockDecls;
            typeParameterScopes = oldTypeParameterScopes;
            typeMappingScope = oldTypeMappingScope;
            superTypes = oldSuperTypes;
            currentClassArgField = oldCurrentClassArgField;
        }
    }

    private Symbol.VarSymbol createArgFieldSymbol() {
        return new Symbol.VarSymbol(
                PRIVATE | FINAL,
                constantHolder().objectTypeArgumentsFieldName,
                syms.specializedTypeType,
                currentClass
        );
    }

    /**
     * This method generates a scope representing the type parameters introduced by a class declaration. This
     * declaration includes the type parameters directly declared by the class, as well as those inherited.
     * <p>
     * We need to include inherited type parameters because we need them in constructors to fill the arg map.
     *
     * @param tree the current class
     */
    private java.util.List<Type> prepareScopes(JCTree.JCClassDecl tree) {
        // This is a new scope for the class type parameters
        var paramToConcrete = new HashMap<Symbol, Type>();

        // We need to push it directly, because when we do lookups to find concrete in the while loop below, we call
        // the general resolution method that uses the typeMappingScope method. We need this map to be inside, in case
        // the current class is the one that declares the type we are searching for.
        //
        // example:
        // interface A<T>
        // class Foo<E> {
        //     class Bar implements A<E> {}
        // }
        //
        // This example shows why we need to use the general lookup method. When we will call this method for Bar, the
        // while loop below will try to find the actual E from the implemented A. However, E is declared in Foo, which
        // have already been placed in the typeMappingScope.
        typeMappingScope = typeMappingScope.prepend(paramToConcrete);

//        var context = new HashMap<Symbol, Type>();

        // Set containing all already visited classes/interface, in case they are implemented several times like below:
        // interface A
        // class Abstract implements A
        // class Concrete extends Abstract implements A
        var visitedClasses = new HashSet<Symbol>();


        var queue = new ArrayDeque<Type>();
        queue.addLast(tree.type);
        currentClass.type.getTypeArguments().forEach(t -> paramToConcrete.put(t.tsym, t));

        var superTypesList = new ArrayList<Type>();

        while (!queue.isEmpty()) {
            var current = queue.removeFirst();
            var currentSymbol = (Symbol.ClassSymbol) current.tsym;

            // We fill the queue with the next interfaces and classes.
            // We only add types once, and only the parameterized ones.

            // TODO maybe remove this
            // For classes, if the superclass is not excluded, we can skip it, as the code will be generated in the
            // class too. Like this, we avoid a more complex algorithm and redundancy (as the superclass will overwrite
            // the content in the map).
            var superType = currentSymbol.getSuperclass();
            var superTypeSymbol = superType.tsym;

            if (superTypeSymbol != null) {
                queue.push(superType);
                // If the class is also parameterized, we add it to the final list
                if (isParameterized(superTypeSymbol) && !superTypeSymbol.isNewGenericsExcluded()) {
                    superTypesList.add(superType);
                }
            }

            currentSymbol.getInterfaces().forEach(i -> {
                var tsym = i.tsym;

                if (!visitedClasses.add(tsym)) return;
                queue.add(i);

                // To be added to the final list, the interface also need to be parameterized.
                if (isParameterized(tsym) && !tsym.isNewGenericsExcluded()) {
                    superTypesList.add(i);
                }
            });

            // Skip the class if it is not parameterized
            if (!isParameterized(currentSymbol)) continue;

            // if current is not parameterized, it means we inherited a parameterized type without using them in the
            // inheritance, meaning we are facing a case of raw inheritance.
            // class A<E>
            // class B extends A
            //
            // NOTE:
            // because we also put the current class in the queue, we could fear entering in this `if`, even
            // though it is not possible as if we entered this method in the first place was because the type
            // declaration contained type parameters.
            // tl;dr: we cannot enter this `if` while processing the base class.
            if (!isParameterized(current)) {
                // TODO rawtype inheritance
                throw new AssertionError("Rawtype inheritance not currently handled");
            }

            // now, we must add all type parameters with a mapping to their actual value.
            var declaredParams = currentSymbol.type.getTypeArguments().iterator();
            var actualValues = current.getTypeArguments().iterator();

            // The compiler only allows either rawtype inheritance (catched above) or inheritance where all
            // type parameters value is provided. It means that we know for sure that both lists have the same size.
            while (declaredParams.hasNext()) {
                var declared = declaredParams.next();
                var actual = actualValues.next();

                // if the actual is not a type parameter, we just add the type to the mapping and proceed to the next.
                // example:
                // interface A<E>
                // class Foo implements A<String>
                if (actual.tsym.getKind() != ElementKind.TYPE_PARAMETER) {
                    paramToConcrete.put(declared.tsym, actual);
                    continue;
                }

                // Otherwise, we are facing a case where the actual type we used is also a parameterized type. We need
                var concrete = findConcreteType(actual);
                paramToConcrete.put(declared.tsym, concrete);
                paramToConcrete.put(actual.tsym, concrete);
            }
        }

        return superTypesList;
    }

    private void rewriteDefs(JCTree.JCClassDecl tree) {
        var filteredDefs = filteredDefinitions(tree);
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

                // fields init have been moved to constructors EXCEPT if we are in an ignored class
                case VARDEF -> parameterizedMethodCallVisitor.visitField((JCTree.JCVariableDecl) member);
                // blocks here can only be static blocks
                case BLOCK -> parameterizedMethodCallVisitor.visitClassBlock((JCTree.JCBlock) member);

                default -> throw new AssertionError("Unexpected member type: " + member.getTag());
            }
        });
    }

    /**
     * Filters the definitions of a class to remove instance fields initializers and instance blocks. This is done
     * because the code they contain might need generic information, that is why we move them to the constructor.
     *
     * <pre>
     * class Foo&lt;T&gt; {
     *     List&lt;T&gt; list = new ArrayList&lt;&gt;(); // here we need the T info
     * }
     * </pre>
     *
     * @param tree the class to process
     * @return the filtered definitions
     */
    private List<JCTree> filteredDefinitions(JCTree.JCClassDecl tree) {
        if (!isParameterized(tree.sym)) return tree.defs;
        var buffer = new ListBuffer<JCTree>();
        tree.defs.forEach(member -> {
            switch (member.getTag()) {
                case VARDEF -> {
                    var field = (JCTree.JCVariableDecl) member;
                    if (!field.sym.isStatic() && field.init != null) { // we gather all instance fields with inits
                        inlineAndBlockDecls.add(make.Assignment(field.sym, field.init));
                        field.init = null;
                    }
                    buffer.add(field);
                }
                case BLOCK -> {
                    var block = (JCTree.JCBlock) member;
                    if ((block.flags & Flags.STATIC) != 0) { // static blocks are ok
                        buffer.add(block);
                    } else { // we totally remove the instance blocks
                        inlineAndBlockDecls.add(block);
                    }
                }
                default -> buffer.add(member);
            }
        });
        return buffer.toList();
    }
    //endregion

    //region rewriting (method)
    private void rewriteBasicMethod(JCTree.JCMethodDecl method) {
        if (isNative(method.sym) || method.sym.isAbstract()) return;

        var oldTypeParameterScopes = typeParameterScopes;
        var oldState = typeParameterScopeGroupState;

        if (method.sym.isStatic()) { // static method cannot access the class type parameters, so we remove them
            typeParameterScopes = new ParameterizedScope();
        }

        try {
            typeParameterScopes.enterMethod();
            if (isParameterized(method.sym)) {
                typeParameterScopes.pushMethodGroup(getTypeArguments(method.sym), method.sym);
            }
            typeParameterScopeGroupState = typeParameterScopes.newState(method.sym);

            parameterizedMethodCallVisitor.visitRegularMethod(method);

            adjustRegularMethodBody(method);

            if (isParameterized(method.sym)) {
                typeParameterScopes.pop();
            }
        } finally {
            typeParameterScopes.exitMethod();
            typeParameterScopes = oldTypeParameterScopes;
            typeParameterScopeGroupState = oldState;
        }
    }

    private void rewriteConstructor(JCTree.JCMethodDecl method) {
        var oldTypeParameterScopeGroupState = typeParameterScopeGroupState;

        try {
            typeParameterScopes.enterMethod();
            var doesCallOverload = TreeInfo.hasConstructorCall(method, names._this);
            // insert the init of the fields that we move to the constructor
            if (!doesCallOverload && !inlineAndBlockDecls.isEmpty()) {
                insertInlineFieldsAndBlocks(method);
            }

            Symbol.VarSymbol argsVariable = null;
            var shouldInitializeArgField = false;

            if (isParameterized(currentClass)) {
                typeParameterScopes.pushConstructorGroup(method.sym);
                argsVariable = typeParameterScopes.top().variable(method.sym);
                shouldInitializeArgField = !doesCallOverload;
            }

            if (isParameterized(method.sym)) {
                typeParameterScopes.pushMethodGroup(getTypeArguments(method.sym), method.sym);
            }
            typeParameterScopeGroupState = typeParameterScopes.newState(method.sym);

            parameterizedMethodCallVisitor.visitConstructor(method, argsVariable);

            adjustConstructorBody(method, argsVariable, shouldInitializeArgField);

            if (isParameterized(method.sym)) {
                typeParameterScopes.pop();
            }
            if (isParameterized(currentClass)) {
                typeParameterScopes.pop();
            }
        } finally {
            typeParameterScopes.exitMethod();
            typeParameterScopeGroupState = oldTypeParameterScopeGroupState;
        }
    }

    /**
     * Inserts the removed code of field initializers and instance blocks in the constructor body.
     *
     * @param method the constructor in which to insert the code
     */
    private void insertInlineFieldsAndBlocks(JCTree.JCMethodDecl method) {
        var superCall = TreeInfo.findConstructorCall(method);
        var buffer = new ListBuffer<JCTree.JCStatement>();
        if (superCall != null) { // if there is a superCall, we must insert the inits right after it
            var bodyIterator = method.body.stats.iterator();
            while (bodyIterator.hasNext()) {
                var next = bodyIterator.next();
                buffer.add(next);
                if (TreeInfo.isSuperCall(next)) {
                    break;
                }
            }
            buffer.addAll(inlineAndBlockDecls);
            bodyIterator.forEachRemaining(buffer::add);
        } else { // otherwise we can just prepend the instructions
            buffer.addAll(inlineAndBlockDecls);
            buffer.addAll(method.body.stats);
        }
        method.body.stats = buffer.toList();
    }

    /**
     * This method is called during a constructor rewriting process. It Generates code filling the global arg map with
     * the type information of the object being created.
     *
     * @param argsVariable the variable that contains the type arguments of the constructor
     */
    private void fillArgMap(
            ListBuffer<JCTree.JCStatement> instructions,
            Symbol.VarSymbol argsVariable,
            Symbol.VarSymbol constructorArgsVariable
    ) {
        // first, we put the current class type parameters in the map
        var call = instanceMethodInvocation(constantHolder().argStackWalkerMethod, constructorArgsVariable);
        call.args = List.of(
                make.Ident(argsVariable),
                make.ClassLiteral(currentClass.type)
        );
        instructions.append(make.Exec(call));

        // then, we put all the supertypes we detected
        superTypes.forEach(superType -> {
            // this is the constructorArgs.add(...) call;
            var c = instanceMethodInvocation(constantHolder().argStackWalkerMethod, constructorArgsVariable);

            // TODO we might need to check if the type is parameterized to handle raw inheritance
            // this is the ParameterizedType.of(...) call;
            var isAccessible = isAccessible(superType.tsym);
            var argFactory = parameterizedTypeConstructorSelector(isAccessible);

            argFactory.args = superType.getTypeArguments().map(argLiteralGenerator::generateSuperArgs);
            argFactory.args = argFactory.args.prepend(classArgParam((Symbol.ClassSymbol) superType.tsym, isAccessible));

            c.args = List.of(argFactory, make.ClassLiteral(superType));

            instructions.append(make.Exec(c));
        });
    }

    private void adjustConstructorBody(
            JCTree.JCMethodDecl method,
            Symbol.VarSymbol argsVariable,
            boolean shouldInitializeArgField
    ) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        typeParameterScopeGroupState.generateVariables(newInstructions::append);

        if (shouldInitializeArgField) {
            var fieldInit = make.Exec(make.Assign(make.Ident(currentClassArgField), make.Ident(argsVariable)));
            newInstructions.append(fieldInit);
        }

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private void adjustRegularMethodBody(JCTree.JCMethodDecl method) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        typeParameterScopeGroupState.generateVariables(newInstructions::append);

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private JCTree.JCStatement argsFallback(Symbol.VarSymbol argsVariable, JCTree.JCExpression call) {
        var binary = make.Binary(JCTree.Tag.EQ, make.Ident(argsVariable), nullLiteral());
        binary.operator = constantHolder().objectEqOperator;
        binary.type = syms.booleanType;
        return make.If(
                binary,
                make.Exec(make.Assign(make.Ident(argsVariable), call)),
                null
        );
    }
    //endregion

    private final class InstructionVisitor extends TreeTranslator {

        private Symbol.VarSymbol constructorArgsVariable;

        @Override
        public void visitExec(JCTree.JCExpressionStatement tree) {
            var res = this.<JCTree>translate(tree.expr);
            if (res.hasTag(JCTree.Tag.BLOCK)) {
                result = res;
            } else {
                tree.expr = (JCTree.JCExpression) res;
                result = tree;
            }
        }

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            super.visitApply(tree);

            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) throw new AssertionError("No symbol for " + tree.meth);

            var isParameterizedMethod = isParameterized(sym);
            var isConstructorFromParameterizedClass = sym.isConstructor() && isParameterized(sym.owner);
            if (sym.owner.type.tsym.isNewGenericsExcluded() || !(isParameterizedMethod || isConstructorFromParameterizedClass)) return;

            var pushMethod = isParameterizedMethod ? pushMethodCode(tree, sym) : nullLiteral();
            var pushConstructor = isConstructorFromParameterizedClass
                    ? pushConstructorCode(tree, sym)
                    : nullLiteral();
            if (tree.args.isEmpty()) {
                // If the method has a void type, we can just add instructions that push the information right
                // before the call.
                // To do so, we change the call in a block with the calls.
                if (sym.getReturnType().getKind() == TypeKind.VOID) {
                    var instr = List.<JCTree.JCStatement>of(make.Exec(tree));
                    if (isConstructorFromParameterizedClass) {
                        instr = instr.prepend(make.Exec(pushConstructor));
                    }
                    if (isParameterizedMethod) {
                        instr = instr.prepend(make.Exec(pushMethod));
                    }
                    result = make.Block(0L, instr);
                } else {
                    // otherwise, we are calling a method that returns a value. We can use the 'pre' method
                    result = preCall(pushMethod, pushConstructor, tree);
                }
            } else {
                // if the method has arguments, we need to push the information after the evaluation of the last arg.
                // to do so, we use the 'post' method
                debug("here ?");
                tree.args = insertPostCall(pushMethod, pushConstructor, tree.args);
                debug("there ?");
            }
        }

        private JCTree.JCExpression pushMethodCode(JCTree.JCMethodInvocation tree, Symbol.MethodSymbol sym) {
            var generatedArgs = basicMethodArgConstruction(
                    sym,
                    tree.typeargs,
                    tree.meth.type.asMethodType().inferenceMapping
            );
            var actualArgs = generatedArgs.orElseGet(() -> {
                throw new UnsupportedOperationException("Rawtypes not handled");
            });

            var push = staticMethodInvocation(constantHolder().pushMethod);
            var caller = needsCallerInfo(sym) ? make.ClassLiteral(currentClass) : nullLiteral();
            push.args = List.of(actualArgs, caller);
            return push;
        }

        private JCTree.JCExpression pushConstructorCode(JCTree.JCMethodInvocation tree, Symbol.MethodSymbol called) {
            var push = staticMethodInvocation(constantHolder().pushConstructor);

            var methodName = Objects.requireNonNull(TreeInfo.name(tree.meth));

            // easiest case, this, we just repush the args without any modification
            if (methodName == methodName.table.names._this) {
                if (constructorArgsVariable == null) {
                    throw new AssertionError("No constructor args variable in context");
                }
                push.args = List.of(make.Ident(constructorArgsVariable));
                return push;
            }

            // otherwise, we need to create a new args object for the super constructor
            var superType = called.owner.type;
            var isAccessible = isAccessible(superType.tsym);
            var supertypeInformation = parameterizedTypeConstructorSelector(isAccessible);

            supertypeInformation.args = superType.getTypeArguments().map(argLiteralGenerator::generateSuperArgs);
            supertypeInformation.args = supertypeInformation.args.prepend(classArgParam((Symbol.ClassSymbol) superType.tsym, isAccessible));

            push.args = List.of(supertypeInformation);
            return push;
        }

        public void visitRegularMethod(JCTree.JCMethodDecl method) {
            method.accept(this);
        }

        public void visitConstructor(JCTree.JCMethodDecl method, Symbol.VarSymbol constructorArgsVariable) {
            var oldConstructorArgsVariable = this.constructorArgsVariable;
            try {
                this.constructorArgsVariable = constructorArgsVariable;
                method.accept(this);
            } finally {
                this.constructorArgsVariable = oldConstructorArgsVariable;
            }
        }

        public void visitField(JCTree.JCVariableDecl field) {

        }

        public void visitClassBlock(JCTree.JCBlock block) {

        }

        private Optional<JCTree.JCExpression> basicMethodArgConstruction(
                Symbol sym,
                List<JCTree.JCExpression> explicitTypes,
                List<Pair<Type, Type>> inferredTypes
        ) {
            var call = constructorInvocation(constantHolder().methodTypeArgsConstructor);
            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
            // use the inferred types `String s = foo();`
            if (!explicitTypes.isEmpty()) { // provided type arguments
                call.args = explicitTypes.map(t -> argLiteralGenerator.generateArgs(t.type));
                return Optional.of(call);
            }

            if (inferredTypes == null) { // special case for rawtype call, we do nothing
                return Optional.empty(); // TODO do something
            }

            call.args = sym.type
                    .getTypeArguments()
                    .map(t -> argLiteralGenerator.generateArgs(computeTypeFromInference(inferredTypes, t)));
            return Optional.of(call);
        }

        private Type computeTypeFromInference(List<Pair<Type, Type>> inferredTypes, Type t) {
            var tsym = t.tsym;
            for (var pair : inferredTypes) { // we try to find the type in the map
                if (pair.fst.tsym == tsym) {
                    return pair.snd;
                }
            }

            // if we end up here, it means that we are facing a '?'
            var upperBound = tsym.type.getUpperBound();
            return new Type.WildcardType(
                    upperBound,
                    upperBound == syms.objectType ? BoundKind.UNBOUND : BoundKind.EXTENDS,
                    null
            );
        }

        private JCTree.JCExpression preCall(
                JCTree.JCExpression e1,
                JCTree.JCExpression e2,
                JCTree.JCExpression actualExpression
        ) {
            Objects.requireNonNull(e1);
            Objects.requireNonNull(e2);
            var call = staticMethodInvocation(constantHolder().preMethod);
            call.args = List.of(e1, e2, actualExpression);
            return call;
        }

        private List<JCTree.JCExpression> insertPostCall(
                JCTree.JCExpression e1,
                JCTree.JCExpression e2,
                List<JCTree.JCExpression> arguments
        ) {
            Objects.requireNonNull(e1);
            Objects.requireNonNull(e2);

            var newInstructions = new ListBuffer<JCTree.JCExpression>();
            var iterator = arguments.iterator();
            while (iterator.hasNext()) {
                var next = iterator.next();
                if (iterator.hasNext()) {
                    newInstructions.append(next);
                    continue;
                }
                var postCall = staticMethodInvocation(constantHolder().postMethod);
                postCall.args = List.of(next, e1, e2);
                newInstructions.append(postCall);
            }

            return newInstructions.toList();
        }

    }

    //region arg generation
    private final class ArgLiteralGenerator {

        /// This field is only used to know if we need to find a concrete type in type var resolution.
        private boolean inSuper;

        public JCTree.JCExpression generateArgs(Type current) {
            inSuper = false;
            return actualGenerateArgs(current);
        }

        public JCTree.JCExpression generateSuperArgs(Type current) {
            inSuper = true;
            return actualGenerateArgs(current);
        }

        private JCTree.JCExpression actualGenerateArgs(Type current) {
            var res = switch (current.getKind()) {
                case ARRAY -> generateArrayKind((Type.ArrayType) current);
                case WILDCARD -> generateWildcardKind((Type.WildcardType) current);
                case INTERSECTION -> generateIntersectionKind((Type.IntersectionClassType) current);
                case DECLARED -> generateClassKind((Type.ClassType) current);
                case TYPEVAR -> generateTypeVarKind((Type.TypeVar) current);
                case BOOLEAN, BYTE, SHORT, INT, LONG, CHAR, FLOAT, DOUBLE ->
                        generatePrimitiveType((Type.JCPrimitiveType) current);
                case EXECUTABLE, PACKAGE, VOID, NONE, NULL, ERROR, UNION, MODULE, OTHER ->
                        throw new AssertionError(current);
            };

            res = handleInnerClass(current, res);
//            res.setType(syms.specializedTypeType); // set the type of the expression to argBaseType (upper bound of all args)
            return res;
        }

        private JCTree.JCExpression generateArrayKind(Type.ArrayType type) {
            var call = constructorInvocation(constantHolder().arrayTypeConstructor);
            call.args = List.of(actualGenerateArgs(type.elemtype));
            return call;
        }

        private JCTree.JCExpression generateWildcardKind(Type.WildcardType type) {
            return switch (type.kind) {
                case UNBOUND -> generateWcExtendsObject();
                case EXTENDS -> {
                    var call = constructorInvocation(constantHolder().wildcardTypeConstructor);
                    call.args = List.of(make.Literal(false), actualGenerateArgs(type.getExtendsBound()));
                    yield call;
                }
                case SUPER -> {
                    var call = constructorInvocation(constantHolder().wildcardTypeConstructor);
                    call.args = List.of(make.Literal(false), actualGenerateArgs(type.getSuperBound()));
                    yield call;
                }
            };
        }

        private JCTree.JCExpression generateIntersectionKind(Type.IntersectionClassType type) {
            var call = constructorInvocation(constantHolder().intersectionTypeConstructor);
            var buffer = new ListBuffer<JCTree.JCExpression>();
            type.getComponents().forEach(c -> buffer.add(actualGenerateArgs(c)));
            call.args = buffer.toList();
            return call;
        }

        private JCTree.JCExpression generateClassKind(Type.ClassType type) {
            var tsym = type.tsym;
            var isAccessible = isAccessible(type.tsym);
            var classFieldAcc = classArgParam((Symbol.ClassSymbol) tsym, isAccessible);

            if (type.isRaw()) { // Foo (raw)
                return rawTypeCreation();
            }

            if (type.getTypeArguments().nonEmpty()) { // Foo<E> (E can be a wildcard)
                var call = parameterizedTypeConstructorSelector(isAccessible);
                var buffer = new ListBuffer<JCTree.JCExpression>();
                buffer.add(classFieldAcc);
                type.typarams_field.forEach(param -> buffer.add(actualGenerateArgs(param)));
                call.args = buffer.toList();
                return call;
            }

            // Foo (basic class)
            var call = classTypeConstructorFactory(isAccessible);
            call.args = List.of(classFieldAcc);
            return call;
        }

        private JCTree.JCExpression generateTypeVarKind(Type.TypeVar type) {
            if (inSuper) {
                var actual = findConcreteType(type);
                if (actual != type) { // if a mapping is provided, try to find the actual type var
                    return actualGenerateArgs(actual);
                }
            }

            var owner = type.tsym.owner;
            var index = owner.type.getTypeArguments().indexOf(type);
            // if the owner of this type does not have it in its declared type parameters, it is a wildcard
            if (index == -1) { // wildcard
                return generateWcExtendsObject();
            }

            return typeVarResolution(type.tsym);
        }

        private JCTree.JCExpression generatePrimitiveType(Type.JCPrimitiveType type) {
            var classFieldAcc = make.ClassLiteral(type);
            var call = constructorInvocation(constantHolder().classTypeConstructor);
            call.args = List.of(classFieldAcc);
            return call;
        }

        private JCTree.JCExpression handleInnerClass(Type current, JCTree.JCExpression res) {
            var enclosingType = current.getEnclosingType();
            // if there is no enclosing type, we have nothing to do
            if (enclosingType == null || Type.noType.equals(enclosingType)) return res;

            // if we are in an inner class, we also need to generate the args for the enclosing type
            var outerRes = actualGenerateArgs(enclosingType);
            var params = List.of(outerRes, res);
            var innerCall = constructorInvocation(constantHolder().innerClassTypeConstructor);
            innerCall.args = params;
            return innerCall;
        }

        private JCTree.JCExpression generateWcExtendsObject() {
            var call = constructorInvocation(constantHolder().wildcardTypeConstructor);
            var c = constructorInvocation(constantHolder().classTypeConstructor);
            c.args = List.of(make.ClassLiteral(syms.objectType));
            call.args = List.of(make.Literal(false), c);
            return call;
        }

    }
    //endregion

    //region type resolution

    /// This method is exclusively used in the context of supertypes arg construction. When in a class constructor, we
    /// need to put the arg representing its supertypes, we use this method to map the typeVar of the supertypes to
    /// their actual values.
    ///
    /// It can return a concrete type, e.g., `class Foo implements Consumer<String>` where when filling the arg for
    /// `Consumer`, `String` will be inserted.
    ///
    /// It can also be a type variable, e.g., `class Foo<T> implements Consumer<T>` where this time it will return `T`.
    ///
    /// @param typeVar the type variable to resolve
    /// @return the actual type of the variable
    private Type findConcreteType(Type typeVar) {
        // because in interfaces typeMappingScope is empty OR TODO see if the type does not come from a class
        if (currentClass.isInterface()/* || typeVar.tsym.owner.kind != Kinds.Kind.TYP*/) return typeVar;

        for (var mapping : typeMappingScope) {
            var type = mapping.get(typeVar.tsym);
            if (type != null) {
                return type;
            }
        }

        // TODO, same as above
        // If we are trying to find a type in the context of an external field assign
//        if (externalFieldOwner != null) {
//            return typeVar;
//        }

        throw new AssertionError("Type not found in mapping: " + typeVar + " in " + typeMappingScope);
    }

    /// Resolves the usage of a type variable. This method generates the code that fetch the information of a type
    /// parameter at runtime.
    ///
    /// @param typeVar the symbol of the type variable that we are looking for
    private JCTree.JCExpression typeVarResolution(Symbol typeVar) {
        ParameterizedScope.Group foundGroup = null;
        var foundIndex = -1;
        var groupDepth = -1;

        for (var scope : typeParameterScopes) {
            var index = scope.index(typeVar);
            groupDepth++;
            if (index != -1) {
                foundGroup = scope;
                foundIndex = index;
                break;
            }
        }

        if (foundGroup != null) {
            var variable = typeParameterScopeGroupState.variable(groupDepth);
            var call = staticMethodInvocation(constantHolder().extractTypeArgumentMethod);
            var id = make.Ident(variable);
            call.args = List.of(id, make.Literal(foundIndex));

            return call;
        }

        // this part is used for external field assign.

//        if (externalFieldOwner == null) {
        throw new AssertionError("Could not find type var " + typeVar + " in class " + currentClass + " with the following scope " + typeParameterScopes);
//        }
//
//        var argAccessingCall = externalMethodInvocation(
//            -1,
//            "getArg",
//            syms.typeArgUtils,
//            syms.argBaseType,
//            List.of(syms.objectType, syms.classType)
//        );
//        argAccessingCall.args = List.of(
//            externalSymbolAccess,
//            make.ClassLiteral((Symbol.ClassSymbol) externalFieldOwner)
//        );
//
//        var index = typeVar.owner.type.getTypeArguments().stream().map(t -> t.tsym).toList().indexOf(typeVar);
//        // then a second getArg on the retrieved Arg to get the actual type parameter value
//        var getInnerArgCall = getGetArgInvocation(-1);
//        getInnerArgCall.args = List.of(argAccessingCall, make.Literal(index));
//        return getInnerArgCall;
    }
    //endregion

    private final class ParameterizedScope implements Iterable<ParameterizedScope.Group> {
        private final ArrayList<Group> groups = new ArrayList<>();
        private int depth;

        public void pushClassGroup(java.util.List<? extends Symbol> variableParams) {
            groups.add(new ClassGroup(variableParams, depth));
        }

        public void pushInterfaceGroup(java.util.List<? extends Symbol> variableParams) {
            groups.add(new InterfaceGroup(variableParams, currentClass, depth));
        }

        public void enterMethod() {
            depth++;
        }

        public void exitMethod() {
            depth--;
        }

        public void pushMethodGroup(java.util.List<? extends Symbol> variableParams, Symbol.MethodSymbol owner) {
            groups.add(new MethodGroup(variableParams, depth, owner));
        }

        public void pushConstructorGroup(Symbol.MethodSymbol constructor) {
            // if we are in a constructor (and the current class is parameterized), we know that the topmost group is a
            // class group.
            var origin = groups.getLast();
            groups.add(new ConstructorGroup(origin.variableParams(), depth, constructor));
        }

        public void pop() {
            groups.removeLast();
        }

        public GroupState newState(Symbol.MethodSymbol currentMethod) {
            return new GroupState(groups, currentMethod, depth);
        }

        @Override
        public Iterator<Group> iterator() {
            return groups.reversed().iterator();
        }

        public Group top() {
            return groups.getLast();
        }

        @Override
        public String toString() {
            return groups.toString();
        }

        public static final class GroupState {
            private final java.util.List<Slot> state;
            private final int depth;

            private GroupState(ArrayList<Group> groups, Symbol.MethodSymbol currentMethod, int depth) {
                state = groups.stream()
                        .map(g -> new Slot(g, g.variable(currentMethod)))
                        .toList();
                this.depth = depth;
            }

            public Symbol.VarSymbol variable(int depth) {
                var index = state.size() - 1 - depth;
                Objects.checkIndex(index, state.size());

                // we also mark the group as used
                var slot = state.get(index);
                slot.used = slot.group.markAsUsed();

                return state.get(index).variable;
            }

            public void generateVariables(Consumer<JCTree.JCStatement> statementConsumer) {
                state.forEach(slot -> {
                    if (!slot.group.shouldGenerate(slot.used, depth)) {
                        return;
                    }
                    slot.group.declaration(slot.variable, statementConsumer);
                });
            }

            private static final class Slot {
                private final Group group;
                private final Symbol.VarSymbol variable;
                private boolean used;

                private Slot(Group group, Symbol.VarSymbol variable) {
                    this.group = group;
                    this.variable = variable;
                }

            }
        }

        sealed interface Group {

            int index(Symbol typeVar);

            void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            );

            Symbol.VarSymbol variable(Symbol.MethodSymbol currentMethod);

            /// Notify the group that it has been used and returns whether the local state should also keep track of
            /// this usage.
            boolean markAsUsed();

            boolean shouldGenerate(boolean usedInState, int currentDepth);

            java.util.List<? extends Symbol> variableParams();

        }

        private static abstract sealed class Base implements Group {
            private final java.util.List<? extends Symbol> variableParams;
            protected final int depth;
            private boolean used;

            protected Base(java.util.List<? extends Symbol> variableParams, int depth) {
                if (depth < 0) {
                    throw new IllegalArgumentException("depth < 0 (" + depth + ")");
                }
                this.variableParams = java.util.List.copyOf(variableParams);
                this.depth = depth;
            }

            @Override
            public final int index(Symbol typeVar) {
                return variableParams.indexOf(typeVar);
            }

            @Override
            public final java.util.List<? extends Symbol> variableParams() {
                return variableParams;
            }

            @Override
            public final boolean markAsUsed() {
                used = true;
                return shouldSaveInLocal();
            }

            protected final boolean used() {
                return used;
            }

            @Override
            public final String toString() {
                return getClass().getSimpleName() + ": [" + variableParams.toString() + "]";
            }

            protected abstract boolean shouldSaveInLocal();

        }

        private final class MethodGroup extends Base {
            // Method group only has one args variable declared.
            private final Symbol.MethodSymbol method;
            private final Symbol.VarSymbol variable;

            private MethodGroup(
                    java.util.List<? extends Symbol> variableParams,
                    int depth,
                    Symbol.MethodSymbol method
            ) {
                super(variableParams, depth);
                this.method = method;
                this.variable = createVariable(
                        constantHolder().methodTypeArgumentsLocalVarName,
                        syms.specializedTypeType,
                        method
                );
            }

            @Override
            public Symbol.VarSymbol variable(Symbol.MethodSymbol currentMethod) {
                return variable;
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                // MethodTypeArgs methodTypeArgs = null;
                statementConsumer.accept(make.VarDef(declarationVariable, nullLiteral()));

                // For methods that can be overriden outside the prototype, we need to add a check using the caller class.
                if (!needsCallerInfo(method)) {
                    var init = staticMethodInvocation(constantHolder().methodTypeArgumentsAccessMethod);
                    init.args = List.of(nullLiteral());
                    statementConsumer.accept(make.Exec(make.Assign(make.Ident(declarationVariable), init)));
                    return;
                }

                // methodTypeArgs = MethodArgStack.methodTypeArgs(callerClass()); or null if there is no virtual call
                var getInstance = staticMethodInvocation(constantHolder().argStackWalkerMethod);
                var callerClass = instanceMethodInvocation(constantHolder().stackWalkerGetCallerClassMethod, getInstance);
                var init = staticMethodInvocation(constantHolder().methodTypeArgumentsAccessMethod);
                init.args = List.of(callerClass);
                var assign = make.Assign(make.Ident(declarationVariable), init);
                statementConsumer.accept(make.Exec(assign));

                // if (methodTypeArgs == null) methodTypeArgs = *raw type arguments;
                var mArgsFallback = constructorInvocation(constantHolder().methodTypeArgsConstructor);
                mArgsFallback.args = rawMethodTypeArguments(method);
                statementConsumer.accept(
                        argsFallback(
                                declarationVariable,
                                mArgsFallback
                        )
                );
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, int currentDepth) {
                return currentDepth == depth && used();
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }
        }

        private final class ClassGroup extends Base {
            private final Symbol.VarSymbol variable;

            private ClassGroup(
                    java.util.List<? extends Symbol> variableParams,
                    int depth
            ) {
                super(variableParams, depth);
                this.variable = currentClassArgField;
            }

            @Override
            public Symbol.VarSymbol variable(Symbol.MethodSymbol currentMethod) {
                return variable;
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, int currentDepth) {
                return false;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }

        }

        private final class InterfaceGroup extends Base {
            // Method group only has one args variable declared.
            private final Symbol.ClassSymbol type;

            private InterfaceGroup(
                    java.util.List<? extends Symbol> variableParams,
                    Symbol.ClassSymbol type,
                    int depth
            ) {
                super(variableParams, depth);
                this.type = type;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }

            @Override
            public void declaration(Symbol.VarSymbol declarationVariable, Consumer<JCTree.JCStatement> statementConsumer) {
                // SpecializedType args = ...;
                var init = staticMethodInvocation(constantHolder().extractAsSuperMethod);
                init.args = List.of(make.This(type.type), make.ClassLiteral(type));
                var assign = make.Assign(make.Ident(declarationVariable), init);
                statementConsumer.accept(make.Exec(assign));
            }

            @Override
            public Symbol.VarSymbol variable(Symbol.MethodSymbol currentMethod) {
                return createVariable(
                        constantHolder().objectTypeArgumentsLocalVarName,
                        syms.specializedTypeType,
                        currentMethod
                );
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, int currentDepth) {
                return currentDepth == depth && used();
            }
        }

        private final class ConstructorGroup extends Base {
            private final Symbol.VarSymbol variable;

            private ConstructorGroup(
                    java.util.List<? extends Symbol> variableParams,
                    int depth,
                    Symbol.MethodSymbol constructor
            ) {
                super(
                        variableParams,
                        depth
                );
                this.variable = createVariable(constantHolder().constructorTypeArgumentsLocalVarName, syms.specializedTypeType, constructor);
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                // SpecializedType constructorArguments = MethodArgStack.constructorTypeArguments();
                var call = staticMethodInvocation(constantHolder().constructorTypeArgumentsAccessMethod);
                statementConsumer.accept(make.VarDef(declarationVariable, call));

                // if (args == null) args = *create raw type*;
                statementConsumer.accept(
                        argsFallback(
                                declarationVariable,
                                rawTypeCreation()
                        )
                );
            }

            @Override
            public Symbol.VarSymbol variable(Symbol.MethodSymbol currentMethod) {
                return variable;
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, int currentDepth) {
                // We always need to generate it in its constructor as it will at lease be used to setup the
                // constructorTypeArgs.
                return currentDepth == depth;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }
        }

    }

    //region method factories
    private JCTree.JCMethodInvocation externalMethodInvocation(
            Symbol.MethodSymbol method,
            Type returnType,
            JCTree.JCExpression accessor
    ) {
        var methodCall = make.Apply(
                List.nil(),
                accessor,
                List.nil()
        ).setType(returnType);
        if (method.isVarArgs()) {
            methodCall.varargsElement = ((Type.ArrayType) method.type.asMethodType().argtypes.last()).elemtype;
        }
        return methodCall;
    }

    private JCTree.JCNewClass constructorInvocation(Symbol.MethodSymbol constructor) {
        var cl = (Symbol.ClassSymbol) constructor.owner;
        var res = make.NewClass(null, List.nil(), make.QualIdent(cl.type.tsym), List.nil(), null);
        res.setType(cl.type);
        res.constructor = constructor;
        if (constructor.isVarArgs()) {
            res.varargsElement = ((Type.ArrayType) constructor.type.asMethodType().argtypes.last()).elemtype;
        }
        return res;
    }

    private JCTree.JCMethodInvocation staticMethodInvocation(Symbol.MethodSymbol method) {
        return externalMethodInvocation(method, method.type.asMethodType().getReturnType(), make.Ident(method));
    }

    private JCTree.JCMethodInvocation instanceMethodInvocation(Symbol.MethodSymbol method, Symbol target) {
        return instanceMethodInvocation(method, make.Ident(target));
    }

    private JCTree.JCMethodInvocation instanceMethodInvocation(Symbol.MethodSymbol method, JCTree.JCExpression target) {
        return externalMethodInvocation(
                method,
                method.type.asMethodType().getReturnType(),
                make.Select(target, method)
        );
    }

    private JCTree.JCFieldAccess fieldAccess(Symbol.VarSymbol field, JCTree.JCExpression owner) {
        return make.Select(owner, field);
    }

    private JCTree.JCNewClass parameterizedTypeConstructorSelector(boolean isAccessible) {
        var sym = isAccessible ?
                constantHolder().parameterizedTypeConstructor :
                constantHolder().parameterizedTypeStringConstructor;
        return constructorInvocation(sym);
    }

    private JCTree.JCNewClass classTypeConstructorFactory(boolean isAccessible) {
        var sym = isAccessible ?
                constantHolder().classTypeConstructor :
                constantHolder().classTypeStringConstructor;
        return constructorInvocation(sym);
    }

    private JCTree.JCExpression rawTypeCreation() {
        var constructorCall = constructorInvocation(constantHolder().parameterizedTypeConstructor);
        var typeParametersCount = currentClass.type.getTypeArguments().length();
        var constructorArguments = List.<JCTree.JCExpression>nil();
        for (int i = 0; i < typeParametersCount; i++) {
            var arg = staticMethodInvocation(constantHolder().erasedTypeInstanceMethod);
            constructorArguments = constructorArguments.prepend(arg);
        }
        constructorArguments = constructorArguments.prepend(make.ClassLiteral(currentClass));
        constructorCall.args = constructorArguments;
        return constructorCall;
    }

    private List<JCTree.JCExpression> rawMethodTypeArguments(Symbol.MethodSymbol sym) {
        var typeParametersCount = sym.type.getTypeArguments().length();
        var arguments = List.<JCTree.JCExpression>nil();
        for (int i = 0; i < typeParametersCount; i++) {
            var arg = staticMethodInvocation(constantHolder().erasedTypeInstanceMethod);
            arguments = arguments.prepend(arg);
        }
        return arguments;
    }
    //endregion

    //region utils
    private boolean needsCallerInfo(Symbol.MethodSymbol method) {
        return !(method.isStatic() || method.isPrivate() || method.isConstructor());
    }

    private JCTree.JCExpression nullLiteral() {
        return make.Literal(TypeTag.BOT, null).setType(syms.botType);
    }

    /**
     * Creates a tree node representing a class access in the context of an arg factory call. If the given symbol is
     * not accessible, it returns a string literal representing the class fully qualified name.
     *
     * @param sym          the symbol of the class to access
     * @param isAccessible true if the symbol is accessible, false otherwise
     * @return a tree node representing the class access
     */
    private JCTree.JCExpression classArgParam(Symbol.ClassSymbol sym, boolean isAccessible) {
        return isAccessible ? make.ClassLiteral(sym) : make.Literal(sym.flatname.toString());
    }

    /**
     * Checks whether the symbol is accessible from the current environment.
     *
     * @param sym the symbol to check
     * @return true if the symbol is accessible, false otherwise
     */
    private boolean isAccessible(Symbol.TypeSymbol sym) {
        return resolve.isAccessible(env, sym);
    }

    /**
     * Checks if the symbol is native. The implementation only checks if the symbol has the NATIVE flag.
     *
     * @param sym the symbol to check
     * @return true if the symbol is native, false otherwise
     */
    private static boolean isNative(Symbol sym) {
        return (sym.flags_field & Flags.NATIVE) != 0;
    }

    /**
     * Checks if the symbol is parameterized. The implementation only checks if the symbol has type arguments.
     *
     * @param sym the symbol to check
     * @return true if the symbol is parameterized, false otherwise
     */
    private static boolean isParameterized(Symbol sym) {
        return isParameterized(sym.type);
    }

    /**
     * Checks if the type is parameterized. The implementation only checks if the type has type arguments.
     *
     * @param type the type to check
     * @return true if the type is parameterized, false otherwise
     */
    private static boolean isParameterized(Type type) {
        return !type.getTypeArguments().isEmpty();
    }

    /**
     * Creates a new variable symbol with the given name, type and owner. The variable is marked as final.
     *
     * @param name  the name of the variable
     * @param type  the type of the variable
     * @param owner the owner of the variable
     * @return the new variable symbol
     */
    private Symbol.VarSymbol createVariable(Name name, Type type, Symbol owner) {
        var offset = nameOffsetIndex;
        nameOffsetIndex = (byte) (nameOffsetIndex < 99 ? nameOffsetIndex + 1 : 0);
        return new Symbol.VarSymbol(
                0, name.append('$', names.fromString(String.valueOf(offset))),
                type,
                owner
        );
    }

    /**
     * Extracts the type arguments from a symbol as a list of {@link Symbol.TypeSymbol}.
     *
     * @param sym the symbol to process
     * @return the list of type arguments
     */
    private static java.util.List<Symbol.TypeSymbol> getTypeArguments(Symbol sym) {
        var list = new ArrayList<Symbol.TypeSymbol>();
        sym.type.getTypeArguments().forEach(t -> list.add(t.tsym));
        return list;
    }
    //endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree classDef, TreeMaker make) {
        Objects.requireNonNull(env);
        Objects.requireNonNull(classDef);
        Objects.requireNonNull(make);
        if (!enabled) return classDef;
        try {
            this.make = make;
            this.env = env;
            return translator.translate(classDef);
        } finally {
            this.make = null;
            this.env = null;
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
