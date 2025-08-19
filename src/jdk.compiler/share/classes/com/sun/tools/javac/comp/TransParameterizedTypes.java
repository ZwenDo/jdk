package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.BoundKind;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.TypeTag;
import com.sun.tools.javac.code.Types;
import com.sun.tools.javac.jvm.ByteCodes;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;
import com.sun.tools.javac.util.Pair;

import javax.lang.model.element.ElementKind;
import javax.lang.model.type.TypeKind;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
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
    private ParameterizedScope typeParameterScopes;

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

        public final Symbol.MethodSymbol pushMethodArgMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("push"),
            new Type.MethodType(
                List.of(syms.methodTypeArgs, syms.classType),
                syms.voidObjectType,
                List.nil(),
                syms.methodClass
            ),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol pusArgMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("pushArg"),
            new Type.MethodType(
                List.of(syms.argBaseType),
                syms.voidObjectType,
                List.nil(),
                syms.methodClass
            ),
            syms.argStackType.tsym
        );

        public final Symbol.MethodSymbol pusArgAndConstructorArgMethod = new Symbol.MethodSymbol(
            PUBLIC | STATIC,
            names.fromString("pushArg"),
            new Type.MethodType(
                List.of(syms.argBaseType, syms.constructorTypeArgsType),
                syms.voidObjectType,
                List.nil(),
                syms.methodClass
            ),
            syms.argStackType.tsym
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
        typeParameterScopes = new ParameterizedScope();
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
                typeParameterScopes = new ParameterizedScope();
                typeMappingScope = List.nil();
            }
            typeParameterScopes.prepareClass();

            if (isParameterized(tree.sym)) {
                typeParameterScopes.pushClassGroup(tree.sym, getTypeArguments(tree.sym));
                // for classes, we prepare the list of supertypes to init in the constructor
                if (!tree.sym.isInterface()) {
                    superTypes = prepareScopes(tree);
                }
            }

            rewriteDefs(tree);
        } finally {
            typeParameterScopes.clearClass();
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

            if (superTypeSymbol != null && !superTypeSymbol.newGenericsExcluded()) {
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
                paramToConcrete.put(actual.tsym, concrete);
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
        if (method.sym.isStatic()) { // static method cannot access the class type parameters, so we remove them
            typeParameterScopes = new ParameterizedScope();
        }

        try {
            typeParameterScopes.prepareMethod(method.sym, this);

            if (isParameterized(method.sym)) {
                typeParameterScopes.pushMethodGroup(method.sym, getTypeArguments(method.sym), this);
            }

            parameterizedMethodCallVisitor.visitRegularMethod(method);

            if (typeParameterScopes.hasUsedGroup()) {
                adjustRegularMethodBody(method);
            }
        } finally {
            typeParameterScopes.clearMethod();
            typeParameterScopes = oldTypeParameterScopes;
        }
    }

    private void rewriteConstructor(JCTree.JCMethodDecl method) {
        var oldTypeParameterScopes = typeParameterScopes;

        try {
            typeParameterScopes.prepareMethod(method.sym, this);

            Symbol.VarSymbol argsVariable = null;
            Symbol.VarSymbol constructorArgsVariable = null;

            var doesCallOverload = TreeInfo.hasConstructorCall(method, names._this);
            // insert the init of the fields that we move to the constructor
            if (!doesCallOverload && !inlineAndBlockDecls.isEmpty()) {
                insertInlineFieldsAndBlocks(method);
            }

            if (isParameterized(currentClass)) {
                // As we are in a constructor, we know that the last group is the current class group.
                argsVariable = typeParameterScopes.currentClassArgs().handle();
                if (!doesCallOverload) {
                    constructorArgsVariable = createVariable(
                        constantHolder.constructorTypeArgsParamName,
                        syms.constructorTypeArgsType,
                        method.sym
                    );
                }
            }

            if (isParameterized(method.sym)) {
                typeParameterScopes.pushMethodGroup(method.sym, getTypeArguments(method.sym), this);
            }

            parameterizedMethodCallVisitor.visitConstructor(method);

            if (isParameterized(currentClass) || typeParameterScopes.hasUsedGroup()) {
                adjustConstructorBody(method, argsVariable, constructorArgsVariable);
            }
        } finally {
            typeParameterScopes.clearMethod();
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
                .map(argument -> argLiteralGenerator.generateSuperArgs(superType, argument));
            argFactory.args = argFactory.args.prepend(classArgParam((Symbol.ClassSymbol) superType.tsym, isAccessible));

            c.args = List.of(argFactory, make.ClassLiteral(superType));

            instructions.append(make.Exec(c));
        });
    }

    /// Adjust the body of a constructor. This method adds the declarations of the mArgs and/or args variables:
    ///
    /// ```
    /// var isEnabled = MethodArgStack.enabled();
    ///
    /// MethodTypeArgs methodTypeArgs = null;
    /// if (isEnabled){
    ///     MethodArgStack.disable();
    ///     MethodArgStack.methodTypeArgs(callerClass());
    ///     MethodArgStack.enable();
    ///}
    /// if (methodTypeArgs == null) methodTypeArgs = MethodTypeArgs.of(defaultValue);
    ///
    /// Arg args = null;
    /// ConstructorTypeArgs constructorTypeArgs = null;
    /// if (isEnabled){
    ///     args = MethodArgStack.typeArgs();
    ///     if (args == null) args = RawType.of(currentClass);
    ///     constructorTypeArgs = MethodArgStack.constructorTypeArgs();
    ///     if (constructorTypeArgs == null) constructorTypeArgs = ConstructorTypeArgs.of();
    ///     *all parameter initializations*
    ///} else {
    ///     args = RawType.of(currentClass);
    ///}
    /// super(...);
    /// if (isEnabled){
    ///     constructorTypeArgs.flush(this);
    ///}
    ///```
    private void adjustConstructorBody(
        JCTree.JCMethodDecl method,
        Symbol.VarSymbol argsVariable,
        Symbol.VarSymbol constructorArgsVariable
    ) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        var isEnabled = addIsEnabled(newInstructions, method);

        typeParameterScopes.forEachUsedGroup(group -> {
            if (group.isClass()) {
                if (group.handle() == argsVariable) return;
                addArgsForRegularMethod(newInstructions, isEnabled, group.handle(), group.owner());
            } else {
                addMethodArgs(newInstructions, method, isEnabled, group.handle());
            }
        });

        addArgsForConstructor(newInstructions, isEnabled, argsVariable, constructorArgsVariable);

        addConstructorArgsFlush(newInstructions, method, isEnabled, constructorArgsVariable);

        method.body.stats = newInstructions.toList();
    }

    /// Adjust the body of a regular method. This method adds the declarations of the mArgs and/or args variables:
    ///
    /// ```
    /// var isEnabled = MethodArgStack.enabled();
    ///
    /// MethodTypeArgs methodTypeArgs = null;
    /// if (isEnabled){
    ///     MethodArgStack.disable();
    ///     MethodArgStack.methodTypeArgs(callerClass());
    ///     MethodArgStack.enable();
    ///}
    /// if (methodTypeArgs == null) methodTypeArgs = MethodTypeArgs.of(defaultValue);
    ///
    /// Arg args = null;
    /// if (isEnabled) args = ArgMap.get(this, currentClass);
    /// if (args == null) args = RawType.of(currentClass);
    ///```
    private void adjustRegularMethodBody(JCTree.JCMethodDecl method) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        var isEnabled = addIsEnabled(newInstructions, method);

        typeParameterScopes.forEachUsedGroup(group -> {
            if (group.isClass()) {
                addArgsForRegularMethod(newInstructions, isEnabled, group.handle(), group.owner());
            } else {
                addMethodArgs(newInstructions, method, isEnabled, group.handle());
            }
        });
    }

    private Symbol.VarSymbol addIsEnabled(ListBuffer<JCTree.JCStatement> newInstructions, JCTree.JCMethodDecl method) {
        var isEnabled = createVariable(constantHolder.isEnabledName, syms.booleanType, method.sym);
        newInstructions.append(
            make.VarDef(isEnabled, staticMethodInvocation(constantHolder.isEnabledMethod))
        );
        return isEnabled;
    }

    /// Adds the method args declaration for a method:
    ///
    /// ```
    /// MethodTypeArgs methodTypeArgs = null;
    /// if (isEnabled){
    ///     MethodArgStack.disable();
    ///     methodTypeArgs = MethodArgStack.methodTypeArgs(callerClass()); // or null if there is no virtual call
    ///     MethodArgStack.enable();
    ///}
    /// if (methodTypeArgs == null) methodTypeArgs = MethodTypeArgs.of(defaultValue);
    ///```
    private void addMethodArgs(
        ListBuffer<JCTree.JCStatement> newInstructions,
        JCTree.JCMethodDecl method,
        Symbol.VarSymbol isEnabled,
        Symbol.VarSymbol methodArgsVariable
    ) {
        if (methodArgsVariable == null) return;

        // MethodTypeArgs methodTypeArgs = null;
        newInstructions.append(make.VarDef(methodArgsVariable, nullLiteral()));

        // For methods that can be overriden outside the prototype, we need to add a check using the caller class.
        if (!needsCallerInfo(method.sym)) {
            var init = staticMethodInvocation(constantHolder.methodTypeArgsAccessMethod);
            init.args = List.of(nullLiteral());
            newInstructions.append(make.Exec(make.Assign(make.Ident(methodArgsVariable), init)));
            return;
        }

        // if (isEnabled) {
        //     MethodArgStack.disable();
        //     methodTypeArgs = MethodArgStack.methodTypeArgs(callerClass()); or null if there is no virtual call
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

        // if (methodTypeArgs == null) methodTypeArgs = MethodTypeArgs.of(defaultValue);
        var mArgsFallback = staticMethodInvocation(constantHolder.methodTypeArgsOfMethod);
        mArgsFallback.args = generateDefaultArgs(method.sym.getTypeParameters());
        newInstructions.append(
            argsFallback(
                methodArgsVariable,
                mArgsFallback
            )
        );
    }

    /// Adds the arg declaration for a regular method:
    ///
    /// ```
    /// Arg args = null;
    /// if (isEnabled) args = ArgMap.get(this, currentClass);
    /// if (args == null) args = RawType.of(currentClass);
    ///```
    private void addArgsForRegularMethod(
        ListBuffer<JCTree.JCStatement> newInstructions,
        Symbol.VarSymbol isEnabled,
        Symbol.VarSymbol argsVariable,
        Symbol owner
    ) {
        // Arg args = null;
        newInstructions.append(make.VarDef(argsVariable, nullLiteral()));

        // if (isEnabled) args = ArgMap.get(this, currentClass);
        var init = staticMethodInvocation(constantHolder.argMapGetMethod);
        init.args = List.of(make.QualThis(owner.type), make.ClassLiteral(currentClass));
        var initStatement = make.Exec(make.Assign(make.Ident(argsVariable), init));
        var ifBlock = make.If(make.Ident(isEnabled), initStatement, null);
        newInstructions.append(ifBlock);

        // if (args == null) args = RawType.of(currentClass);
        var argsFallback = staticMethodInvocation(constantHolder.rawTypeOfMethod);
        argsFallback.args = List.of(make.ClassLiteral(currentClass.type));
        newInstructions.append(
            argsFallback(
                argsVariable,
                argsFallback
            )
        );
    }

    /// Adds the arg and constructor args declaration for a constructor:
    ///
    /// ```
    /// Arg args = null;
    /// ConstructorTypeArgs constructorTypeArgs = null;
    /// if (isEnabled){
    ///    args = MethodArgStack.typeArgs();
    ///    if (args == null) args = args = RawType.of(currentClass);
    ///    constructorTypeArgs = MethodArgStack.constructorTypeArgs();
    ///    if (constructorTypeArgs == null) constructorTypeArgs = new ConstructorTypeArgs();
    ///    *all parameter initializations*
    ///} else {
    ///    args = RawType.of(currentClass);
    ///}
    ///```
    private void addArgsForConstructor(
        ListBuffer<JCTree.JCStatement> newInstructions,
        Symbol.VarSymbol isEnabled,
        Symbol.VarSymbol argsVariable,
        Symbol.VarSymbol constructorArgsVariable
    ) {
        if (argsVariable == null) return;

        // Arg args = null;
        newInstructions.append(make.VarDef(argsVariable, nullLiteral()));
        if (constructorArgsVariable != null) {
            // ConstructorTypeArgs constructorTypeArgs = null;
            newInstructions.append(make.VarDef(constructorArgsVariable, nullLiteral()));
        }

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

        if (constructorArgsVariable != null) {
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
        }


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

    /// Adds the constructor args flush call for a constructor (after the super call):
    ///
    /// ```
    /// super();
    /// if (isEnabled){
    ///    constructorArgs.flush(this);
    ///}
    ///```
    private void addConstructorArgsFlush(
        ListBuffer<JCTree.JCStatement> newInstructions,
        JCTree.JCMethodDecl method,
        Symbol.VarSymbol isEnabled,
        Symbol.VarSymbol constructorArgsVariable
    ) {
        if (constructorArgsVariable == null) {
            newInstructions.addAll(method.body.stats);
            return;
        }
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

    private final class InstructionVisitor extends TreeTranslator {

        private boolean isInExpression = false;

        private Symbol.VarSymbol methodArgsVariable;

        private Symbol.VarSymbol argsVariable;

        @Override
        public void visitApply(JCTree.JCMethodInvocation tree) {
            var oldIsInExpression = isInExpression;
            isInExpression = true;
            try {
                tree.meth = translate(tree.meth);
            } finally {
                isInExpression = oldIsInExpression;
            }

            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) throw new AssertionError("No symbol for " + tree.meth);

            // TODO `this` calls might need a repush of args
            // If the symbol is not parameterized, we have nothing to do.
            if (!isParameterized(sym)) return;

            var generatedArgs = basicMethodArgConstruction(
                sym,
                tree.typeargs,
                tree.meth.type.asMethodType().inferenceMapping
            );

            if (tree.args.isEmpty()) {
                // If the method has a void type, we can just add an instruction that pushes the information right
                // before the call.
                // To do so, we change the call in a block with two calls.
                if (sym.getReturnType().getKind() == TypeKind.VOID) {
                    var push = staticMethodInvocation(constantHolder.pushMethodArgMethod);
                    var caller = needsCallerInfo(sym) ? make.ClassLiteral(currentClass) : nullLiteral();
                    push.args = List.of(caller);
                    result = make.Block(
                        0L,
                        List.of(
                            make.Exec(push),
                            make.Exec(tree)
                        )
                    );
                } else {
                    // otherwise, we are calling a method that returns a value. We can use the 'e' method

                }
            }
        }

        public void visitRegularMethod(JCTree.JCMethodDecl method) {
//            method.accept(this);
        }

        public void visitConstructor(JCTree.JCMethodDecl method) {
//            method.accept(this);
        }

        public void visitField(JCTree.JCVariableDecl field) {

        }

        public void visitClassBlock(JCTree.JCBlock block) {

        }

        private Optional<JCTree.JCMethodInvocation> basicMethodArgConstruction(
            Symbol sym,
            List<JCTree.JCExpression> explicitTypes,
            List<Pair<Type, Type>> inferredTypes
        ) {
            var call = staticMethodInvocation(constantHolder.methodTypeArgsOfMethod);
            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
            // use the inferred types `String s = foo();`
            if (!explicitTypes.isEmpty()) { // provided type arguments
                call.args = explicitTypes.map(t -> argLiteralGenerator.generateArgs(null, t.type));
                return Optional.of(call);
            }

            if (inferredTypes == null) { // special case for rawtype call, we do nothing
                return Optional.empty();
            }

            var args = List.<JCTree.JCExpression>nil();
            for (var t : sym.type.getTypeArguments()) {
                var tsym = t.tsym;
                Type res = null;
                for (var pair : inferredTypes) { // we try to find the type in the map
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
                args = args.prepend(argLiteralGenerator.generateArgs(null, res));
            }

            call.args = args;
            return Optional.of(call);
        }

    }

    //region arg generation
    private final class ArgLiteralGenerator {

        /// This field is only used to know if we need to find a concrete type in type var resolution.
        private boolean inSuper;

        public JCTree.JCExpression generateArgs(Type previous, Type current) {
            inSuper = false;
            return actualGenerateArgs(previous, current);
        }

        public JCTree.JCExpression generateSuperArgs(Type previous, Type current) {
            inSuper = true;
            return actualGenerateArgs(previous, current);
        }

        private JCTree.JCExpression actualGenerateArgs(Type previous, Type current) {
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
            call.args = List.of(actualGenerateArgs(type, type.elemtype));
            return call;
        }

        private JCTree.JCExpression generateWildcardKind(Type.WildcardType type) {
            return switch (type.kind) {
                case UNBOUND -> generateWcExtendsObject();
                case EXTENDS -> {
                    var call = staticMethodInvocation(constantHolder.wildcardTypeOfUpperMethod);
                    call.args = List.of(actualGenerateArgs(type, type.getExtendsBound()));
                    yield call;
                }
                case SUPER -> {
                    var call = staticMethodInvocation(constantHolder.wildcardTypeOfLowerMethod);
                    call.args = List.of(actualGenerateArgs(type, type.getSuperBound()));
                    yield call;
                }
            };
        }

        private JCTree.JCExpression generateIntersectionKind(Type.IntersectionClassType type) {
            var call = staticMethodInvocation(constantHolder.intersectionTypeOfMethod);
            var buffer = new ListBuffer<JCTree.JCExpression>();
            type.getComponents().forEach(c -> buffer.add(actualGenerateArgs(type, c)));
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
                type.typarams_field.forEach(param -> buffer.add(actualGenerateArgs(type, param)));
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
                    return actualGenerateArgs(enclosing, actual);
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
            var call = staticMethodInvocation(constantHolder.classTypeOfMethod);
            call.args = List.of(classFieldAcc);
            return call;
        }

        private JCTree.JCExpression handleInnerClass(Type current, JCTree.JCExpression res) {
            var enclosingType = current.getEnclosingType();
            // if there is no enclosing type, we have nothing to do
            if (enclosingType == null || Type.noType.equals(enclosingType)) return res;

            // if we are in an inner class, we also need to generate the args for the enclosing type
            var outerRes = actualGenerateArgs(current, enclosingType);
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
    private JCTree.JCExpression typeVarResolution(Symbol typeVar) {
        ParameterizedScope.Group foundGroup = null;
        var foundIndex = -1;

        for (var scope : typeParameterScopes) {
            var index = scope.index(typeVar);
            if (index != -1) {
                foundGroup = scope;
                foundIndex = index;
                break;
            }
        }

        if (foundGroup != null) {
            return foundGroup.access(foundIndex, this);
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
    private static final class ParameterizedScope implements Iterable<ParameterizedScope.Group> {
        private final ArrayDeque<ClassGroup> classes = new ArrayDeque<>();
        private final ArrayDeque<MethodGroup> methods = new ArrayDeque<>();
        private int methodIndex;
        private int classIndex;

        /// This method should be called when visiting a method or a constructor. It increases the batch index and init
        /// all handles.
        public void prepareMethod(Symbol.MethodSymbol currentMethod, TransParameterizedTypes handler) {
            methodIndex++;
            var it = classes.descendingIterator();
            while (it.hasNext()) {
                var group = it.next();
                if (group.handle() != null) break; // already initialized
                group.setForMethod(
                    createVariable(
                        handler.constantHolder.argsParamName,
                        handler.syms.argBaseType,
                        currentMethod
                    ),
                    methodIndex
               );
            }
        }

        public void prepareClass() {
            classIndex++;
        }

        public void pushClassGroup(
            Symbol.ClassSymbol owner,
            java.util.List<? extends Symbol> variableParams
        ) {
            // index for classes is always -1, as it will be initialized in methods when calling prepare
            classes.add(new ClassGroup(variableParams, classIndex, owner));
        }

        public void pushMethodGroup(
            Symbol.MethodSymbol owner,
            java.util.List<? extends Symbol> variableParams,
            TransParameterizedTypes handler
        ) {
            var handle = createVariable(handler.constantHolder.methodTypeArgsParamName, handler.syms.methodTypeArgs, owner);
            methods.add(new MethodGroup(handle, variableParams, methodIndex));
        }

        public void clearMethod() {
            var it = classes.descendingIterator();
            while (it.hasNext()) {
                var group = it.next();
                if (group.methodIndex != methodIndex) break;
                group.reset();
            }
            var it2 = methods.descendingIterator();
            while (it2.hasNext()) {
                var group = it2.next();
                if (group.methodIndex != methodIndex) break;
                it2.remove();
            }

            methodIndex--;
        }

        public void clearClass() {
            var it = classes.descendingIterator();
            while (it.hasNext()) {
                var group = it.next();
                if (group.classIndex != classIndex) break; // we only want the groups from the current batch
                it.remove();
            }
            classIndex--;
        }

        public void forEachUsedGroup(Consumer<? super ParameterizedScope.Group> action) {
            var it = classes.descendingIterator();
            while (it.hasNext()) {
                var group = it.next();
                if (group.methodIndex != methodIndex) break; // we only want the groups from the current batch
                if (group.used()) action.accept(group); // found a used group
            }
            var it2 = methods.descendingIterator();
            while (it2.hasNext()) {
                var group = it2.next();
                if (group.methodIndex != methodIndex) break; // we only want the groups from the current batch
                if (group.used()) action.accept(group); // found a used group
            }
        }

        public boolean hasUsedGroup() {
            var it = classes.descendingIterator();
            while (it.hasNext()) {
                var group = it.next();
                if (group.methodIndex != methodIndex) break;
                if (group.used()) return true; // found a used group
            }
            var it2 = methods.descendingIterator();
            while (it2.hasNext()) {
                var group = it2.next();
                if (group.methodIndex != methodIndex) break;
                if (group.used()) return true; // found a used group
            }
            return false;
        }

        @Override
        public Iterator<Group> iterator() {
            return new Iterator<Group>() {
                private final Iterator<ClassGroup> it = classes.descendingIterator();
                private final Iterator<MethodGroup> it2 = methods.descendingIterator();

                @Override
                public boolean hasNext() {
                    return it.hasNext() || it2.hasNext();
                }

                @Override
                public Group next() {
                    if (it.hasNext()) {
                        return it.next();
                    }
                    if (it2.hasNext()) {
                        return it2.next();
                    }
                    throw new NoSuchElementException("No more groups in the scope");
                }
            };
        }

        public Group currentClassArgs() {
            return classes.getLast();
        }

        @Override
        public String toString() {
            return "(methods: " + methods + ", classes: " + classes + ")";
        }

        private boolean isFromCurrentBatch(int index) {
            return methodIndex == index || -methodIndex == index;
        }

        interface Group {
            boolean used();

            boolean isClass();

            Symbol.VarSymbol handle();

            int index(Symbol typeVar);

            default Symbol owner() {
                throw new UnsupportedOperationException();
            }

            JCTree.JCExpression access(int index, TransParameterizedTypes handler);
        }

        private static abstract class Base implements Group {
            private final java.util.List<? extends Symbol> variableParams;
            private boolean used;

            protected Base(java.util.List<? extends Symbol> variableParams) {
                this.variableParams = variableParams;
                this.used = false;
            }

            @Override
            public final boolean used() {
                return used;
            }

            @Override
            public final int index(Symbol typeVar) {
                return variableParams.indexOf(typeVar);
            }

            @Override
            public String toString() {
                return variableParams.toString();
            }

        }

        private static final class MethodGroup extends Base {
            private final Symbol.VarSymbol handle;
            private final int methodIndex;

            MethodGroup(Symbol.VarSymbol handle, java.util.List<? extends Symbol> variableParams, int methodIndex) {
                super(variableParams);
                this.handle = handle;
                this.methodIndex = methodIndex;
            }

            @Override
            public boolean isClass() {
                return false;
            }

            @Override
            public Symbol.VarSymbol handle() {
                return handle;
            }

            @Override
            public JCTree.JCExpression access(int index, TransParameterizedTypes handler) {
                var call = handler.instanceMethodInvocation(handler.constantHolder.methodTypeArgsGetMethod, handle);
                call.args = List.of(handler.make.Literal(index));
                return call;
            }

        }

        private static final class ClassGroup extends Base {
            private Symbol.VarSymbol handle;
            private final Symbol owner;
            private final int classIndex;
            private int methodIndex;

            ClassGroup(
                java.util.List<? extends Symbol> variableParams,
                int classIndex,
                Symbol owner
            ) {
                super(variableParams);
                this.owner = owner;
                this.classIndex = classIndex;
            }

            @Override
            public boolean isClass() {
                return true;
            }

            @Override
            public Symbol.VarSymbol handle() {
                return handle;
            }

            @Override
            public Symbol owner() {
                return owner;
            }

            public void setForMethod(Symbol.VarSymbol handle, int methodIndex) {
                this.handle = handle;
                this.methodIndex = methodIndex;
            }

            public void reset() {
                this.handle = null;
                this.methodIndex = -1;
            }

            @Override
            public JCTree.JCExpression access(int index, TransParameterizedTypes handler) {
                var call = handler.staticMethodInvocation(handler.constantHolder.typeArgsGetArgMethod);
                var id = handler.make.Ident(handle);
                call.args = List.of(id, handler.make.Literal(index));
                return call;
            }

        }

    }

//    /// This class is used to represent a group of type parameters coming from the same declaration. Anytime a type
//    /// parameter is referenced in the code, we need to know its actual value to achieve reification.
//    ///
//    /// To do so, each time a type parameter is declared (either in a method or a class), we create a new
//    /// [TypeParameterGroup] instance. This instance will store all the type parameters belonging to this declaration
//    /// and also the type of the declaration (whether they are declared by a method or a class).
//    private static final class TypeParameterGroup {
//        private final java.util.List<? extends Symbol> variableParams;
//        private final boolean isClass;
//        private final Symbol.VarSymbol methodArgs;
//
//        private TypeParameterGroup(
//            java.util.List<? extends Symbol> variableParams,
//            boolean isClass,
//            Symbol.VarSymbol methodArgs
//        ) {
//            Objects.requireNonNull(variableParams);
//            this.variableParams = variableParams;
//            this.isClass = isClass;
//            this.methodArgs = methodArgs;
//        }
//
//        public int index(Symbol typeVar) {
//            return variableParams.indexOf(typeVar);
//        }
//
//        public JCTree.JCExpression access(int index, Symbol.VarSymbol classArgs, TransParameterizedTypes handler) {
//            if (isClass) {
//                Objects.requireNonNull(classArgs);
//                return classAccess(index, classArgs, handler);
//            }
//            return methodAccess(index, methodArgs, handler);
//        }
//
//        /**
//         * This scope gets the information from the enclosing method arguments.
//         *
//         * <pre>
//         * static &lt;T&gt; void foo() {
//         *     var l = List.&lt;T&gt;of();
//         * }
//         * </pre>
//         * It uses the arg local variable generated in all parameterized methods.
//         *
//         * @param arguments          the list of type arguments declared in the method
//         * @param methodArgsVariable the local variable containing the type information for this method
//         */
//        public static TypeParameterGroup method(java.util.List<? extends Symbol> arguments, Symbol.VarSymbol methodArgsVariable) {
//            Objects.requireNonNull(methodArgsVariable);
//            return new TypeParameterGroup(arguments, false, methodArgsVariable);
//        }
//
//        /**
//         * This scope gets the information from args stored in the object.
//         *
//         * <pre>
//         * interface Foo&lt;T&gt; {
//         *     void foo() {
//         *         var l = List.&lt;T&gt;of();
//         *     }
//         * }
//         * </pre>
//         *
//         * @param arguments the list of type arguments declared in the class
//         */
//        public static TypeParameterGroup clazz(java.util.List<? extends Symbol> arguments) {
//            return new TypeParameterGroup(arguments, true, null);
//        }
//
//        private static JCTree.JCExpression methodAccess(
//            int index,
//            Symbol.VarSymbol variable,
//            TransParameterizedTypes handler
//        ) {
//            var call = handler.instanceMethodInvocation(handler.constantHolder.methodTypeArgsGetMethod, variable);
//            call.args = List.of(handler.make.Literal(index));
//            return call;
//        }
//
//        private static JCTree.JCExpression classAccess(
//            int index,
//            Symbol.VarSymbol variable,
//            TransParameterizedTypes handler
//        ) {
//            var call = handler.staticMethodInvocation(handler.constantHolder.typeArgsGetArgMethod);
//            var id = handler.make.Ident(variable);
//            call.args = List.of(id, handler.make.Literal(index));
//            return call;
//        }
//
//    }

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
    private boolean needsCallerInfo(Symbol.MethodSymbol method) {
        return !(method.isStatic() || method.isPrivate());
    }

    private JCTree.JCExpression nullLiteral() {
        return make.Literal(TypeTag.BOT, null).setType(syms.botType);
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
