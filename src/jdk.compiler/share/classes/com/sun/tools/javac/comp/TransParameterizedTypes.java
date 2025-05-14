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
import java.util.ptype.ClassType;
import java.util.ptype.RawType;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.sun.tools.javac.code.Flags.*;

public final class TransParameterizedTypes {

    //region fields
    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private final InstructionVisitor parameterizedMethodCallVisitor;
    private final ArgLiteralGenerator argLiteralGenerator;
    private final ConstantHolder constantHolder;

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
    private JCTree.JCClassDecl currentClassTree;

    /**
     * Stack containing the different type args declared at any point in the code. It allows when a generic type is
     * required, to find where it comes from and generate access to it.
     */
    private List<TypeParameterGroup> typeParameterScopes;

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

    private final class ConstantHolder {

        public final Name argsParamName = names.fromString("args");

        public final Name constructorTypeArgsParamName = names.fromString("constructorTypeArgs");

        public final Name methodTypeArgsParamName = names.fromString("methodTypeArgs");

        private final Name ofName = names.fromString("of");

        private final Name isEnabledName = names.fromString("isEnabled");

        public final Symbol.MethodSymbol isEnabledMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("enabled"),
            new Type.MethodType(List.nil(), syms.booleanType, List.nil(), syms.methodClass),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol enableMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("enable"),
            new Type.MethodType(List.nil(), syms.voidType, List.nil(), syms.methodClass),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol disableMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("disable"),
            new Type.MethodType(List.nil(), syms.voidType, List.nil(), syms.methodClass),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol methodTypeArgsAccessMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("methodTypeArgs"),
            new Type.MethodType(List.of(syms.classType), syms.methodTypeArgs, List.nil(), syms.methodClass),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol argsAccessMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("typeArgs"),
            new Type.MethodType(List.nil(), syms.argBaseType, List.nil(), syms.methodClass),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol constructorTypeArgsAccessMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("constructorTypeArgs"),
            new Type.MethodType(
                List.nil(),
                syms.constructorTypeArgsType,
                List.nil(),
                syms.methodClass
            ),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol constructorTypeArgsAddMethod = new Symbol.MethodSymbol(
            PUBLIC,
            names.fromString("add"),
            new Type.MethodType(
                List.of(syms.argBaseType, syms.classType),
                syms.voidType,
                List.nil(),
                syms.methodClass
            ),
            syms.constructorTypeArgsType.tsym
        );

        public final Symbol.MethodSymbol constructorTypeArgsFlushMethod = new Symbol.MethodSymbol(
            PUBLIC,
            names.fromString("flush"),
            new Type.MethodType(
                List.of(syms.objectType),
                syms.voidType,
                List.nil(),
                syms.methodClass
            ),
            syms.constructorTypeArgsType.tsym
        );

        public final Symbol.MethodSymbol argMapGetMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("get"),
            new Type.MethodType(
                List.of(syms.objectType, syms.classType),
                syms.argBaseType,
                List.nil(),
                syms.methodClass
            ),
            syms.argMapType.tsym
        );

        public final Symbol.MethodSymbol typeArgsGetArgMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC | VARARGS,
            names.fromString("getArg"),
            new Type.MethodType(
                List.of(syms.argBaseType, types.makeArrayType(syms.intType)),
                syms.argBaseType,
                List.nil(),
                syms.methodClass
            ),
            syms.typeArgUtils.tsym
        );

        public final Symbol.MethodSymbol methodTypeArgsGetMethod = new Symbol.MethodSymbol(
            PUBLIC,
            names.fromString("get"),
            new Type.MethodType(
                List.of(syms.intType),
                syms.argBaseType,
                List.nil(),
                syms.methodClass
            ),
            syms.methodTypeArgs.tsym
        );

        public final Symbol.MethodSymbol arrayTypeOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(List.of(syms.argBaseType), syms.arrayTypeArgs, List.nil(), syms.methodClass),
            syms.arrayTypeArgs.tsym
        );

        public final Symbol.MethodSymbol classTypeOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(List.of(syms.classType), syms.classTypeArgs, List.nil(), syms.methodClass),
            syms.classTypeArgs.tsym
        );

        public final Symbol.MethodSymbol innerClassTypeOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(List.of(syms.argBaseType, syms.argBaseType), syms.innerClassTypeArgs, List.nil(), syms.methodClass),
            syms.innerClassTypeArgs.tsym
        );

        public final Symbol.MethodSymbol parameterizedTypeOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC | VARARGS,
            ofName,
            new Type.MethodType(
                List.of(syms.classType, types.makeArrayType(syms.argBaseType)),
                syms.parameterizedTypeTypeArgs,
                List.nil(),
                syms.methodClass
            ),
            syms.parameterizedTypeTypeArgs.tsym
        );

        public final Symbol.MethodSymbol intersectionTypeOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(
                List.of(types.makeArrayType(syms.argBaseType)),
                syms.intersectionTypeArgs,
                List.nil(),
                syms.methodClass
            ),
            syms.intersectionTypeArgs.tsym
        );

        public final Symbol.MethodSymbol wildcardTypeOfUpperMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC | VARARGS,
            names.fromString("ofUpper"),
            new Type.MethodType(
                List.of(types.makeArrayType(syms.argBaseType)),
                syms.wildcardTypeArgs,
                List.nil(),
                syms.methodClass
            ),
            syms.wildcardTypeArgs.tsym
        );

        public final Symbol.MethodSymbol wildcardTypeOfLowerMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC | VARARGS,
            names.fromString("ofLower"),
            new Type.MethodType(
                List.of(types.makeArrayType(syms.argBaseType)),
                syms.wildcardTypeArgs,
                List.nil(),
                syms.methodClass
            ),
            syms.wildcardTypeArgs.tsym
        );

        public final Symbol.MethodSymbol rawTypeOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(List.of(syms.classType), syms.rawTypeTypeArgs, List.nil(), syms.methodClass),
            syms.rawTypeTypeArgs.tsym
        );

        public final Symbol.MethodSymbol classTypeOfStringMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(List.of(syms.stringType), syms.classType, List.nil(), syms.methodClass),
            syms.classType.tsym
        );

        public final Symbol.MethodSymbol methodTypeArgsOfMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC | VARARGS,
            ofName,
            new Type.MethodType(
                List.of(types.makeArrayType(syms.argBaseType)),
                syms.methodTypeArgs,
                List.nil(),
                syms.methodClass
            ),
            syms.methodTypeArgs.tsym
        );

        public final Symbol.MethodSymbol parameterizedTypeOfStringMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC | VARARGS,
            ofName,
            new Type.MethodType(
                List.of(syms.stringType, types.makeArrayType(syms.argBaseType)),
                syms.parameterizedTypeTypeArgs,
                List.nil(),
                syms.methodClass
            ),
            syms.parameterizedTypeTypeArgs.tsym
        );

        public final Symbol.MethodSymbol rawTypeOfStringMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            ofName,
            new Type.MethodType(List.of(syms.stringType), syms.rawTypeTypeArgs, List.nil(), syms.methodClass),
            syms.rawTypeTypeArgs.tsym
        );

        public final Symbol.MethodSymbol argStackWalkerMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("walker"),
            new Type.MethodType(List.nil(), syms.stackWalkerType, List.nil(), syms.methodClass),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol stackWalkerGetCallerClassMethod = new Symbol.MethodSymbol(
            PUBLIC,
            names.fromString("getCallerClass"),
            new Type.MethodType(List.nil(), syms.classType, List.nil(), syms.methodClass),
            syms.stackWalkerType.tsym
        );

        public final Symbol.MethodSymbol constructorTypeArgsConstructor = new Symbol.MethodSymbol(
            PUBLIC,
            names.init,
            new Type.MethodType(
                List.nil(),
                syms.voidType,
                List.nil(),
                syms.methodClass
            ),
            syms.constructorTypeArgsType.tsym
        );

        public final Symbol.OperatorSymbol objectEqOperator = operators
            .lookupBinaryOp(o -> o.opcode == ByteCodes.if_acmpeq);

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
        typeParameterScopes = List.nil();
        typeMappingScope = List.nil();
        argLiteralGenerator = new ArgLiteralGenerator();
        constantHolder = new ConstantHolder();
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

    public static boolean newGenericsExcluded(Symbol.ClassSymbol clazz) {
        Objects.requireNonNull(clazz);
//        if (true) return true;
        var packageName = clazz.packge().getQualifiedName().toString();
        var fullName = clazz.fullname.toString();
        return packageName.startsWith("java.lang")
//            || packageName.startsWith("java.util")
            || packageName.startsWith("java.util.ptype")
            || "java.util.Objects".equals(fullName)
            || packageName.startsWith("java.util.concurrent")
            || packageName.startsWith("jdk.internal")
            || packageName.startsWith("java.security")
            || packageName.startsWith("build.tools.classlist")
            || packageName.startsWith("sun.reflect.generics")
            || "java.util.WeakHashMap".equals(fullName)
            || (clazz.owner.getKind().isClass() && newGenericsExcluded((Symbol.ClassSymbol) clazz.owner));
    }

    //region rewriting (class)
    private final class Translator extends TreeTranslator {

        /**
         * this method is only called for the top-level class
         */
        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) {
            result = tree;

            if (tree.sym.newGenericsExcluded()) return;
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

        try {
            currentClass = tree.sym;
            currentClassTree = tree;
            inlineAndBlockDecls = new ArrayList<>();
            // static classes cannot access the outer class type parameters, so we remove them
            if (tree.sym.isStatic()) {
                typeParameterScopes = List.nil();
                typeMappingScope = List.nil();
            }

            if (isParameterized(tree.sym)) {
                pushGroup(TypeParameterGroup.clazz(getTypeArguments(tree.sym)));
                // for classes, we prepare the list of supertypes to init in the constructor
                if (!tree.sym.isInterface()) {
                    superTypes = prepareScopes(tree);
                }
            }

            rewriteDefs(tree);
        } finally {
            currentClass = oldCurrentClassTree != null ? oldCurrentClassTree.sym : null;
            currentClassTree = oldCurrentClassTree;
            inlineAndBlockDecls = oldInlineAndBlockDecls;
            typeParameterScopes = oldTypeParameterScopes;
            typeMappingScope = oldTypeMappingScope;
            superTypes = oldSuperTypes;
        }
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
        // We push a new scope for the class type parameters
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
        // have already be placed in the typeMappingScope.
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

        var finalList = new ArrayList<Type>();

        while (!queue.isEmpty()) {
            var current = queue.removeFirst();
            var currentSymbol = (Symbol.ClassSymbol) current.tsym;

            // We fill the queue with the next interfaces and classes.
            // We only add types once, and only the parameterized ones.

            // For classes, if the superclass is not excluded, we can skip it, as the code will be generated in the
            // class too. Like this, we avoid a more complex algorithm and redundancy (as the superclass will overwrite
            // the content in the map).
            var superType = currentSymbol.getSuperclass();
            var superTypeSymbol = superType.tsym;

            if (superTypeSymbol != null && superTypeSymbol.newGenericsExcluded()) {
                queue.push(superType);
                // If the class is also parameterized, we add it to the final list
                if (isParameterized(superTypeSymbol)) {
                    finalList.add(superType);
                }
            }

            currentSymbol.getInterfaces().forEach(i -> {
                var tsym = i.tsym;

                if (!visitedClasses.add(tsym)) return;
                queue.add(i);

                // To be added to the final list, the interface also need to be parameterized.
                if (isParameterized(tsym)) {
                    finalList.add(i);
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
            }
        }

        return finalList;
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
        if (!currentClass.newGenericsExcluded() || !isParameterized(tree.sym)) return tree.defs;
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
        if (method.sym.isStatic()) { // static method cannot access the class type parameters, so we remove them
            typeParameterScopes = List.nil();
        }

        try {
            // Do not reorder this method call. It should be the first thing to do, as it updates a field
            Symbol.VarSymbol argsVariable = null;
            if (isParameterized(method.sym)) {
                argsVariable = createVariable(constantHolder.methodTypeArgsParamName, syms.methodTypeArgs, method.sym);
                pushGroup(TypeParameterGroup.method(getTypeArguments(method.sym), argsVariable));
            }

            parameterizedMethodCallVisitor.visitMethod(method);
            adjustBody(method, argsVariable, null, null);
        } finally {
            typeParameterScopes = oldTypeParameterScopes;
        }
    }

    private void rewriteConstructor(JCTree.JCMethodDecl method) {
        var oldTypeParameterScopes = typeParameterScopes;

        try {
            Symbol.VarSymbol argsVariable = null;
            Symbol.VarSymbol constructorArgsVariable = null;

            var doesCallOverload = TreeInfo.hasConstructorCall(method, names._this);
            // insert the init of the fields that we move to the constructor
            if (!doesCallOverload && !inlineAndBlockDecls.isEmpty()) {
                insertInlineFieldsAndBlocks(method);
            }

            if (!doesCallOverload && isParameterized(currentClass)) {
                argsVariable = createVariable(constantHolder.argsParamName, syms.argBaseType, method.sym);
                constructorArgsVariable = createVariable(
                    constantHolder.constructorTypeArgsParamName,
                    syms.constructorTypeArgsType,
                    method.sym
                );
            }

            Symbol.VarSymbol methodArgsVariable = null;
            if (isParameterized(method.sym)) {
                methodArgsVariable = createVariable(constantHolder.methodTypeArgsParamName, syms.methodTypeArgs, method.sym);
                pushGroup(TypeParameterGroup.method(getTypeArguments(method.sym), methodArgsVariable));
            }

            parameterizedMethodCallVisitor.visitMethod(method);

            adjustBody(method, methodArgsVariable, argsVariable, constructorArgsVariable);
        } finally {
            typeParameterScopes = oldTypeParameterScopes;
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
        var call = instanceMethodInvocation(constantHolder.constructorTypeArgsAddMethod, constructorArgsVariable);
        call.args = List.of(
            make.Ident(argsVariable),
            make.ClassLiteral(currentClass.type)
        );
        instructions.append(make.Exec(call));

        // then, we put all the supertypes we detected
        superTypes.forEach(superType -> {
            // this is the constructorArgs.add(...) call;
            var c = instanceMethodInvocation(constantHolder.constructorTypeArgsAddMethod, constructorArgsVariable);

            // TODO we might need to check if the type is parameterized to handle raw inheritance
            // this is the ParameterizedType.of(...) call;
            var isAccessible = isAccessible(superType.tsym);
            var argFactory = parameterizedTypeOfFactory(isAccessible);

            argFactory.args = superType.getTypeArguments()
                .map(argument -> argLiteralGenerator.generateSuperArgs(superType, argument, argsVariable));
            argFactory.args = argFactory.args.prepend(classArgParam((Symbol.ClassSymbol) superType.tsym, isAccessible));

            c.args = List.of(argFactory, make.ClassLiteral(superType));

            instructions.append(make.Exec(c));
        });
    }

    /// Transform the body of the method to add the declaration of the mArgs and/or args variables.
    ///
    /// This method would convert:
    ///
    /// ```
    /// <T> void foo(){
    ///     // ...
    ///}
    ///```
    /// to
    /// ```
    /// <T> void foo(){
    ///     var args = MethodArgStack.typeArgs(...);
    ///     // ...
    ///}
    ///```
    ///
    /// @param method             the original method
    /// @param methodArgsVariable the variable that contains the type arguments or null if the method is not
    ///                                                                               parameterized
    /// @param argsVariable       the variable that contains the arguments or null if the method is not
    private void adjustBody(
        JCTree.JCMethodDecl method,
        Symbol.VarSymbol methodArgsVariable,
        Symbol.VarSymbol argsVariable,
        Symbol.VarSymbol constructorArgsVariable
    ) {
        if (argsVariable == null && methodArgsVariable == null) return;

        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        var isEnabled = createVariable(constantHolder.isEnabledName, syms.booleanType, method.sym);
        newInstructions.append(
            make.VarDef(isEnabled, staticMethodInvocation(constantHolder.isEnabledMethod))
        );

        // if the method is a constructor and its class is parameterized, we declare the args variable that might be
        // used in the body
        if (argsVariable != null) {
            // Arg args = null;
            newInstructions.append(make.VarDef(argsVariable, nullLiteral()));
            // ConstructorTypeArgs constructorTypeArgs = null;
            newInstructions.append(make.VarDef(constructorArgsVariable, nullLiteral()));

            // if (isEnabled) {
            //     args = MethodArgStack.typeArgs();
            //     if (args == null) args = args = RawType.of(currentClass);
            //     constructorTypeArgs = MethodArgStack.constructorTypeArgs();
            //     if (constructorTypeArgs == null) constructorTypeArgs = new ConstructorTypeArgs();
            //     *all parameter initializations*
            // } else {
            //     args = RawType.of(currentClass);
            // }

            var conditional = new ListBuffer<JCTree.JCStatement>();

            // args = MethodArgStack.typeArgs();
            conditional.append(
                make.Exec(
                    make.Assign(
                        make.Ident(argsVariable),
                        staticMethodInvocation(constantHolder.argsAccessMethod)
                    )
                )
            );

            // if (args == null) args = RawType.of(currentClass);
            var argsFallback = staticMethodInvocation(constantHolder.rawTypeOfMethod);
            argsFallback.args = List.of(make.ClassLiteral(currentClass.type));
            conditional.append(
                argsFallback(
                    argsVariable,
                    argsFallback
                )
            );

            // constructorTypeArgs = MethodArgStack.constructorTypeArgs();
            var ctorArgsAssign = make.Assign(
                make.Ident(constructorArgsVariable),
                staticMethodInvocation(constantHolder.constructorTypeArgsAccessMethod)
            );
            conditional.append(make.Exec(ctorArgsAssign));

            // if (constructorTypeArgs == null) constructorTypeArgs = new ConstructorTypeArgs();
            conditional.append(
                argsFallback(
                    constructorArgsVariable,
                    newConstructorTypeArgs()
                )
            );

            // all parameter initializations
            fillArgMap(conditional, argsVariable, constructorArgsVariable);


            // } else { args = RawType.of(currentClass); }
            var fallback2 = staticMethodInvocation(constantHolder.rawTypeOfMethod)
                .setType(syms.argBaseType);
            fallback2.args = List.of(make.ClassLiteral(currentClass.type));
            var elseBlock = make.Assign(make.Ident(argsVariable), fallback2);

            var ifBlock = make.If(
                make.Ident(isEnabled),
                make.Block(0L, conditional.toList()),
                make.Block(0L, List.of(make.Exec(elseBlock)))
            );

            newInstructions.append(ifBlock);
        }


        // if the method has type parameters, we declare the methodArgs variable that might be used in the body
        if (methodArgsVariable != null) {
            // MethodTypeArgs methodTypeArgs = null;
            newInstructions.append(make.VarDef(methodArgsVariable, nullLiteral()));


            // if (MethodArgStack.enabled()) {
            //     methodTypeArgs = MethodArgStack.methodTypeArgs(callerClass());
            //     MethodArgStack.enable();
            // }
            var getInstance = staticMethodInvocation(constantHolder.argStackWalkerMethod);
            var callerClass = instanceMethodInvocation(constantHolder.stackWalkerGetCallerClassMethod, getInstance);
            var init = staticMethodInvocation(constantHolder.methodTypeArgsAccessMethod);
            init.args = List.of(callerClass);
            var assign = make.Assign(make.Ident(methodArgsVariable), init);
            var ifBlock = make.If(
                make.Ident(isEnabled),
                make.Block(
                    0L,
                    List.of(
                        make.Exec(staticMethodInvocation(constantHolder.disableMethod)),
                        make.Exec(assign),
                        make.Exec(staticMethodInvocation(constantHolder.enableMethod))
                    )
                ),
                null
            );
            newInstructions.append(ifBlock);

            // if (methodTypeArgs == null) methodTypeArgs = MethodTypeArgs.of(default);
            var mArgsFallback = staticMethodInvocation(constantHolder.methodTypeArgsOfMethod);
            mArgsFallback.args = generateDefaultArgs(method.sym.getTypeParameters());
            newInstructions.append(
                argsFallback(
                    methodArgsVariable,
                    mArgsFallback
                )
            );

        }

        if (constructorArgsVariable != null) {
            for (JCTree.JCStatement instr : method.body.stats) {
                newInstructions.add(instr);
                if (TreeInfo.isSuperCall(instr)) {
                    var flush = instanceMethodInvocation(
                        constantHolder.constructorTypeArgsFlushMethod,
                        constructorArgsVariable
                    );
                    flush.args = List.of(make.This(currentClass.type));
                    var ifBlock = make.If(
                        make.Ident(isEnabled),
                        make.Exec(flush),
                        null
                    );
                    newInstructions.add(ifBlock);
                }
            }
        } else {
            newInstructions.addAll(method.body.stats);
        }

        method.body.stats = newInstructions.toList();
    }

    private JCTree.JCStatement argsFallback(Symbol.VarSymbol argsVariable, JCTree.JCExpression call) {
        var binary = make.Binary(JCTree.Tag.EQ, make.Ident(argsVariable), nullLiteral());
        binary.operator = constantHolder.objectEqOperator;
        binary.type = syms.booleanType;
        return make.If(
            binary,
            make.Exec(make.Assign(make.Ident(argsVariable), call)),
            null
        );
    }
    //endregion

    private static final class InstructionVisitor extends TreeTranslator {

        public void visitMethod(JCTree.JCMethodDecl method) {
            if (method.body == null) { // abstract method
                return;
            }
        }

        public void visitField(JCTree.JCVariableDecl field) {

        }

        public void visitClassBlock(JCTree.JCBlock block) {

        }

    }

    //region arg generation
    private final class ArgLiteralGenerator {

        private Symbol.VarSymbol classArgs;
        /// This field is only used to know if we need to find a concrete type in type var resolution.
        private boolean inSuper;

        public JCTree.JCExpression generateArgs(Type previous, Type current, Symbol.VarSymbol classArgs) {
            this.classArgs = classArgs;
            inSuper = false;
            try {
                return generateArgs(previous, current);
            } finally {
                this.classArgs = null;
            }
        }

        public JCTree.JCExpression generateSuperArgs(Type previous, Type current, Symbol.VarSymbol classArgs) {
            this.classArgs = classArgs;
            inSuper = true;
            try {
                return generateArgs(previous, current);
            } finally {
                this.classArgs = null;
            }
        }

        private JCTree.JCExpression generateArgs(Type previous, Type current) {
            var res = switch (current.getKind()) {
                case ARRAY -> generateArrayKind((Type.ArrayType) current);
                case WILDCARD -> generateWildcardKind((Type.WildcardType) current);
                case INTERSECTION -> generateIntersectionKind((Type.IntersectionClassType) current);
                case DECLARED -> generateClassKind((Type.ClassType) current);
                case TYPEVAR -> generateTypeVarKind((Type.TypeVar) current, Objects.requireNonNull(previous));
                case BOOLEAN, BYTE, SHORT, INT, LONG, CHAR, FLOAT, DOUBLE ->
                    generatePrimitiveType((Type.JCPrimitiveType) current);
                case EXECUTABLE, PACKAGE, VOID, NONE, NULL, ERROR, UNION, MODULE, OTHER ->
                    throw new AssertionError(current);
            };

            res = handleInnerClass(current, res);
            res.setType(syms.argBaseType); // set the type of the expression to argBaseType (upper bound of all args)
            return res;
        }

        private JCTree.JCExpression generateArrayKind(Type.ArrayType type) {
            var call = staticMethodInvocation(constantHolder.arrayTypeOfMethod);
            call.args = List.of(generateArgs(type, type.elemtype));
            return call;
        }

        private JCTree.JCExpression generateWildcardKind(Type.WildcardType type) {
            return switch (type.kind) {
                case UNBOUND -> generateWcExtendsObject();
                case EXTENDS -> {
                    var call = staticMethodInvocation(constantHolder.wildcardTypeOfUpperMethod);
                    call.args = List.of(generateArgs(type, type.getExtendsBound()));
                    yield call;
                }
                case SUPER -> {
                    var call = staticMethodInvocation(constantHolder.wildcardTypeOfLowerMethod);
                    call.args = List.of(generateArgs(type, type.getSuperBound()));
                    yield call;
                }
            };
        }

        private JCTree.JCExpression generateIntersectionKind(Type.IntersectionClassType type) {
            var call = staticMethodInvocation(constantHolder.intersectionTypeOfMethod);
            var buffer = new ListBuffer<JCTree.JCExpression>();
            type.getComponents().forEach(c -> buffer.add(generateArgs(type, c)));
            call.args = buffer.toList();
            return call;
        }

        private JCTree.JCExpression generateClassKind(Type.ClassType type) {
            var tsym = type.tsym;
            var isAccessible = isAccessible(type.tsym);
            var classFieldAcc = classArgParam((Symbol.ClassSymbol) tsym, isAccessible);

            if (type.isRaw()) { // Foo (raw)
                var call = rawTypeOfFactory(isAccessible);
                call.args = List.of(classFieldAcc);
                return call;
            }

            if (type.getTypeArguments().nonEmpty()) { // Foo<E> (E can be a wildcard)
                var call = parameterizedTypeOfFactory(isAccessible);
                var buffer = new ListBuffer<JCTree.JCExpression>();
                buffer.add(classFieldAcc);
                type.typarams_field.forEach(param -> buffer.add(generateArgs(type, param)));
                call.args = buffer.toList();
                return call;
            }

            // Foo (basic class)
            var call = classTypeOfFactory(isAccessible);
            call.args = List.of(classFieldAcc);
            return call;
        }

        private JCTree.JCExpression generateTypeVarKind(Type.TypeVar type, Type enclosing) {
            if (inSuper) {
                var actual = findConcreteType(type);
                if (actual != type) { // if a mapping is provided, try to find the actual type var
                    return generateArgs(enclosing, actual);
                }
            }

            var owner = type.tsym.owner;
            var index = owner.type.getTypeArguments().indexOf(type);
            // if the owner of this type does not have it in its declared type parameters, it is a wildcard
            if (index == -1) { // wildcard
                return generateWcExtendsObject();
            }

            return typeVarResolution(type.tsym, classArgs);
        }

        private JCTree.JCExpression generatePrimitiveType(Type.JCPrimitiveType type) {
            var classFieldAcc = make.ClassLiteral(type);
            var call = staticMethodInvocation(constantHolder.classTypeOfMethod);
            call.args = List.of(classFieldAcc);
            return call;
        }

        private JCTree.JCExpression handleInnerClass(Type current, JCTree.JCExpression res) {
            var enclosingType = current.getEnclosingType();
            // if there is no enclosing type, we have nothing to do
            if (enclosingType == null || Type.noType.equals(enclosingType)) return res;

            // if we are in an inner class, we also need to generate the args for the enclosing type
            var outerRes = generateArgs(current, enclosingType);
            var params = List.of(outerRes, res);
            var innerCall = staticMethodInvocation(constantHolder.innerClassTypeOfMethod);
            innerCall.args = params;
            return innerCall;
        }

        private JCTree.JCExpression generateWcExtendsObject() {
            var call = staticMethodInvocation(constantHolder.wildcardTypeOfUpperMethod);
            var c = staticMethodInvocation(constantHolder.classTypeOfMethod);
            c.args = List.of(make.ClassLiteral(syms.objectType));
            call.args = List.of(c);
            return call;
        }

    }

    /**
     * Generate a list of erased types representing the type arguments of a method.
     */
    private List<JCTree.JCExpression> generateDefaultArgs(List<Symbol.TypeVariableSymbol> params) {
        var buffer = new ListBuffer<JCTree.JCExpression>();
        params.forEach(parameter -> {
            var bounds = parameter.getBounds();
            types.erasure(bounds).forEach(t -> {
                var tsym = (Symbol.ClassSymbol) t.tsym;
                var isAccessible = isAccessible(tsym);

                var c = classTypeOfFactory(isAccessible);

                c.args = List.of(classArgParam(tsym, isAccessible));
                buffer.append(c);
            });
        });
        return buffer.toList();
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
//        // If we are trying to find a type in the context of an external field assign
//        if (externalFieldOwner != null) {
//            return typeVar;
//        }

        throw new AssertionError("Type not found in mapping: " + typeVar + " in " + typeMappingScope);
    }

    /// Resolves the usage of a type variable. This method generates the code that fetch the information of a type
    /// parameter at runtime.
    ///
    /// @param typeVar   the symbol of the type variable that we are looking for
    /// @param classArgs the object containing type information for the current class (can be null).
    private JCTree.JCExpression typeVarResolution(Symbol typeVar, Symbol.VarSymbol classArgs) {
        TypeParameterGroup foundScope = null;
        var foundIndex = -1;

        for (var scope : typeParameterScopes) {
            var index = scope.index(typeVar);
            if (index != -1) {
                foundScope = scope;
                foundIndex = index;
                break;
            }
        }

        if (foundScope != null) {
            return foundScope.access(foundIndex, classArgs, this);
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

    //region scope type parameter

    /// This class is used to represent a group of type parameters coming from the same declaration. Anytime a type
    /// parameter is referenced in the code, we need to know its actual value to achieve reification.
    ///
    /// To do so, each time a type parameter is declared (either in a method or a class), we create a new
    /// [TypeParameterGroup] instance. This instance will store all the type parameters belonging to this declaration
    /// and also the type of the declaration (whether they are declared by a method or a class).
    private static final class TypeParameterGroup {
        private final java.util.List<? extends Symbol> variableParams;
        private final boolean isClass;
        private final Symbol.VarSymbol constantHandle;

        private TypeParameterGroup(
            java.util.List<? extends Symbol> variableParams,
            boolean isClass,
            Symbol.VarSymbol constantHandle
        ) {
            Objects.requireNonNull(variableParams);
            this.variableParams = variableParams;
            this.isClass = isClass;
            this.constantHandle = constantHandle;
        }

        public int index(Symbol typeVar) {
            return variableParams.indexOf(typeVar);
        }

        public JCTree.JCExpression access(int index, Symbol.VarSymbol classArgs, TransParameterizedTypes handler) {
            if (isClass) {
                Objects.requireNonNull(classArgs);
                return classAccess(index, classArgs, handler);
            }
            return methodAccess(index, constantHandle, handler);
        }

        /**
         * This scope gets the information from the enclosing method arguments.
         *
         * <pre>
         * static &lt;T&gt; void foo() {
         *     var l = List.&lt;T&gt;of();
         * }
         * </pre>
         * It uses the arg local variable generated in all parameterized methods.
         *
         * @param arguments          the list of type arguments declared in the method
         * @param methodArgsVariable the local variable containing the type information for this method
         */
        public static TypeParameterGroup method(java.util.List<? extends Symbol> arguments, Symbol.VarSymbol methodArgsVariable) {
            Objects.requireNonNull(methodArgsVariable);
            return new TypeParameterGroup(arguments, false, methodArgsVariable);
        }

        /**
         * This scope gets the information from args stored in the object.
         *
         * <pre>
         * interface Foo&lt;T&gt; {
         *     void foo() {
         *         var l = List.&lt;T&gt;of();
         *     }
         * }
         * </pre>
         *
         * @param arguments the list of type arguments declared in the class
         */
        public static TypeParameterGroup clazz(java.util.List<? extends Symbol> arguments) {
            return new TypeParameterGroup(arguments, true, null);
        }

        private static JCTree.JCExpression methodAccess(
            int index,
            Symbol.VarSymbol variable,
            TransParameterizedTypes handler
        ) {
            var call = handler.instanceMethodInvocation(handler.constantHolder.methodTypeArgsGetMethod, variable);
            call.args = List.of(handler.make.Literal(index));
            return call;
        }

        private static JCTree.JCExpression classAccess(
            int index,
            Symbol.VarSymbol variable,
            TransParameterizedTypes handler
        ) {
            var call = handler.staticMethodInvocation(handler.constantHolder.typeArgsGetArgMethod);
            var id = handler.make.Ident(variable);
            call.args = List.of(id, handler.make.Literal(index));
            return call;
        }

    }

    //endregion

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

    /// Utility method to generate the correct [ParameterizedType.of][java.util.ptype.ParameterizedType#of] depending on
    /// whether the class param is accessible.
    ///
    /// @param isAccessible whether the class is accessible
    /// @return the method invocation
    private JCTree.JCMethodInvocation parameterizedTypeOfFactory(boolean isAccessible) {
        var sym = isAccessible ?
            constantHolder.parameterizedTypeOfMethod :
            constantHolder.parameterizedTypeOfStringMethod;
        return staticMethodInvocation(sym);
    }

    /// Utility method to generate the correct [ClassType.of][java.util.ptype.ClassType#of] depending on whether the
    /// class param is accessible.
    ///
    /// @param isAccessible whether the class is accessible
    /// @return the method invocation
    private JCTree.JCMethodInvocation classTypeOfFactory(boolean isAccessible) {
        var sym = isAccessible ?
            constantHolder.classTypeOfMethod :
            constantHolder.classTypeOfStringMethod;
        return staticMethodInvocation(sym);
    }

    /// Utility method to generate the correct [RawType.of][java.util.ptype.RawType#of] depending on whether the class
    /// param is accessible.
    ///
    /// @param isAccessible whether the class is accessible
    /// @return the method invocation
    private JCTree.JCMethodInvocation rawTypeOfFactory(boolean isAccessible) {
        var sym = isAccessible ?
            constantHolder.rawTypeOfMethod :
            constantHolder.rawTypeOfStringMethod;
        return staticMethodInvocation(sym);
    }
    //endregion

    //region utils
    private JCTree.JCExpression nullLiteral() {
        return make.Literal(TypeTag.BOT, null).setType(syms.botType);
    }

    private void pushGroup(TypeParameterGroup group) {
        typeParameterScopes = typeParameterScopes.prepend(group);
    }

    private JCTree.JCNewClass newConstructorTypeArgs() {
         var res = make.NewClass(null, List.nil(), make.QualIdent(syms.constructorTypeArgsType.tsym), List.nil(), null);
         res.setType(syms.constructorTypeArgsType);
         res.constructor = constantHolder.constructorTypeArgsConstructor;
         return res;
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
    private static Symbol.VarSymbol createVariable(Name name, Type type, Symbol owner) {
        return new Symbol.VarSymbol(0, name, type, owner);
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
