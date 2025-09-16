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


    private final boolean enableSynthetic;
    private final boolean enabled;

    private final InstructionVisitor instructionVisitor;
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

    private ClassContext classContext;

    private final ParameterizedScope typeParameterScopes;

    /// Simple value appended to generated local variable names to easily identify them (e.g. args$34). We assume that
    /// there will never be more than 99.
    private int nameOffsetIndex;


    private static final class ClassContext {
        private final Symbol.ClassSymbol classSymbol;
        private final Symbol.VarSymbol descriptorField;
        private final boolean isHighestClassInGenericHierarchy;

        private final Symbol.MethodSymbol fieldAccessorStaticMethod;
        /// Field containing all the fields and blocks that are declared in the current class. These declarations must
        /// be moved to the constructor to be able to access the method parameters.
        private final java.util.List<JCTree.JCStatement> inlineAndBlockDeclarations = new ArrayList<>();

        private ClassContext(
                Symbol.ClassSymbol classSymbol,
                Symbol.VarSymbol descriptorField,
                Symbol.MethodSymbol fieldAccessorStaticMethod,
                boolean isHighestClassInGenericHierarchy
        ) {
            this.classSymbol = classSymbol;
            this.descriptorField = descriptorField;
            this.fieldAccessorStaticMethod = fieldAccessorStaticMethod;
            this.isHighestClassInGenericHierarchy = isHighestClassInGenericHierarchy;
        }

    }

    private final class ConstantHolder {

        public final Name methodTypeArgumentsLocalVarName = names.fromString("methodArguments");

        public final Name objectTypeArgumentsLocalVarName = names.fromString("objectArguments");

        public final Name objectTypeArgumentsFieldName = names.fromString("$typeArguments");

        public final Name computeSuperMethodName = names.fromString("$computeSuper");

        public final Name fieldAccessorStaticMethodName = names.fromString("$getDescriptorStatic");

        public final Name constructorTypeArgumentsLocalVarName = names.fromString("constructorArgument");

        public final Name letExprLocalVarName = names.fromString("result");

        public final Symbol.MethodSymbol methodTypeArgumentsAccessMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("methodTypeArguments"),
                new Type.MethodType(List.of(syms.classType), syms.methodDescriptorType, List.nil(), syms.methodClass),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol constructorTypeArgumentsAccessMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("constructorTypeArguments"),
                new Type.MethodType(List.nil(), syms.specializedTypeDescriptorType, List.nil(), syms.methodClass),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol extractFromClassMethod = new Symbol.MethodSymbol(
                PUBLIC,
                names.fromString("$typeArgument"),
                new Type.MethodType(
                        List.of(syms.intType),
                        syms.specializedTypeDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.classDescriptorType.tsym
        );

        public final Symbol.MethodSymbol outerMethod = new Symbol.MethodSymbol(
                PUBLIC,
                names.fromString("$outer"),
                new Type.MethodType(
                        List.nil(),
                        syms.classDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.classDescriptorType.tsym
        );

        public final Symbol.MethodSymbol extractFromMethodMethod = new Symbol.MethodSymbol(
                PUBLIC,
                names.fromString("$typeArgument"),
                new Type.MethodType(
                        List.of(syms.intType),
                        syms.specializedTypeDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.methodDescriptorType.tsym
        );

        public final Symbol.MethodSymbol asSuperMethod = new Symbol.MethodSymbol(
                PUBLIC,
                names.fromString("asSuper"),
                new Type.MethodType(
                        List.of(syms.classType),
                        syms.classDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.classType.tsym
        );

        public final Symbol.MethodSymbol arrayTypeConstructor = new Symbol.MethodSymbol(
                PUBLIC,
                names.init,
                new Type.MethodType(List.of(syms.specializedTypeDescriptorType), syms.voidType, List.nil(), syms.methodClass),
                syms.arrayDescriptorType.tsym
        );

        public final Symbol.MethodSymbol classDescriptorConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(syms.classDescriptorType, syms.classType, syms.booleanType, types.makeArrayType(syms.specializedTypeDescriptorType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.classDescriptorType.tsym
        );

        public final Symbol.MethodSymbol specializedMethodTypeArgsConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(types.makeArrayType(syms.specializedTypeDescriptorType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.methodDescriptorType.tsym
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
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol stackWalkerGetCallerClassMethod = new Symbol.MethodSymbol(
                PUBLIC,
                names.fromString("getCallerClass"),
                new Type.MethodType(List.nil(), syms.classType, List.nil(), syms.methodClass),
                syms.stackWalkerType.tsym
        );


        public final Symbol.MethodSymbol pushMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("pushMethod"),
                new Type.MethodType(
                        List.of(syms.methodDescriptorType, syms.classType),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol popMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("popMethodTypeArguments"),
                new Type.MethodType(
                        List.nil(),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol pushConstructor = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("pushConstructor"),
                new Type.MethodType(
                        List.of(syms.specializedTypeDescriptorType),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypePassingHandleType.tsym
        );

        public final Symbol.MethodSymbol hashMapConstructor = new Symbol.MethodSymbol(
                PUBLIC | VARARGS,
                names.init,
                new Type.MethodType(
                        List.of(types.makeArrayType(syms.objectType)),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypeHashMap.tsym
        );

        public final Symbol.MethodSymbol findClassByNameMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("findClassByName"),
                new Type.MethodType(
                        List.of(syms.stringType),
                        syms.classType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.specializedTypeUtilsClassType.tsym
        );

        public final Symbol.MethodSymbol rawMethodDescriptorMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("rawInstance"),
                new Type.MethodType(
                        List.nil(),
                        syms.methodDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.methodDescriptorType.tsym
        );

        public final Type computeSuperMapType = types.subst(
                syms.specializedTypeHashMap,
                syms.specializedTypeHashMap.getTypeArguments(),
                List.of(
                        types.subst(syms.classType, syms.classType.getTypeArguments(), List.of(new Type.WildcardType(syms.objectType, BoundKind.UNBOUND, syms.boundClass))),
                        syms.specializedTypeDescriptorType
                )
        );

        public final Symbol.MethodSymbol optionalEmptyMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("empty"),
                new Type.MethodType(
                        List.nil(),
                        syms.optionalType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.optionalType.tsym
        );

        public final Symbol.MethodSymbol optionalOfNullableMethod = new Symbol.MethodSymbol(
                PUBLIC | STATIC,
                names.fromString("ofNullable"),
                new Type.MethodType(
                        List.of(syms.objectType),
                        syms.optionalType,
                        List.nil(),
                        syms.methodClass
                ),
                syms.optionalType.tsym
        );

        public final Symbol.OperatorSymbol objectEqOperator = operators
                .lookupBinaryOp(o -> o.opcode == ByteCodes.if_acmpeq);

    }

    @SuppressWarnings("this-escape")
    private TransParameterizedTypes(Context context) {
        context.put(typeReifierKey, this);
        make = TreeMaker.instance(context);
        instructionVisitor = new InstructionVisitor();
        translator = new Translator();
        log = Log.instance(context);
        syms = Symtab.instance(context);
        names = Names.instance(context);
        resolve = Resolve.instance(context);
        types = Types.instance(context);
        operators = Operators.instance(context);
        typeParameterScopes = new ParameterizedScope();
        argLiteralGenerator = new ArgLiteralGenerator();
        var options = Options.instance(context);
        enabled = options.isSet("enableSpecialization");
        enableSynthetic = !options.isSet("showGeneratedCode");

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

    public static boolean hasNewGenerics(Symbol.TypeSymbol clazz) {
        Objects.requireNonNull(clazz);
        var module = clazz.packge().modle;
        var moduleName = module.getQualifiedName().toString();
        return !(moduleName.startsWith("java") || moduleName.startsWith("jdk") || moduleName.startsWith("sun"));

//        var packageName = clazz.packge().getQualifiedName().toString();
//        var fullName = clazz.getQualifiedName().toString();
//        return packageName.startsWith("java.lang")
//                || packageName.startsWith("java.util.ptype")
//                || packageName.startsWith("java.util.concurrent")
//                || packageName.startsWith("jdk.internal")
//                || packageName.startsWith("java.security")
//                || packageName.startsWith("build.tools.classlist")
//                || packageName.startsWith("sun.reflect.generics")
//                || "java.util.WeakHashMap".equals(fullName)
//                || (clazz.owner.getKind().isClass() && hasNewGenerics((Symbol.ClassSymbol) clazz.owner));
    }

    //region rewriting (class)
    private final class Translator extends TreeTranslator {

        /**
         * this method is only called for the top-level class
         */
        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) {
            result = tree;

            if (!enabled || !tree.sym.hasNewGenerics()) return;
            try {
                rewriteClass(tree);
            } catch (Exception | AssertionError t) {
                debug("error in class: " + tree.sym.fullname);
                throw t;
            }
        }

    }

    private void rewriteClass(JCTree.JCClassDecl tree) {
        var oldClassContext = classContext;

        var highestClassInGenericHierarchy = highestGenericSuperType(tree.sym);
        var isHighestClassInGenericHierarchy = tree.sym == highestClassInGenericHierarchy;

        Symbol.VarSymbol descriptorField = null;
        if (highestClassInGenericHierarchy != null) {
            if (isHighestClassInGenericHierarchy) {
                descriptorField = tree.sym.descriptorField = createDescriptorFieldSymbol(tree.sym);
            } else {
                descriptorField = Objects.requireNonNull(highestClassInGenericHierarchy.descriptorField);
            }
        }

        classContext = new ClassContext(
                tree.sym,
                descriptorField,
                tree.sym.type.isParameterized() ? fieldAccessorStaticMethodSymbol(tree.sym) : null,
                isHighestClassInGenericHierarchy
        );

        var walkSuperTypesResult = walkSuperTypes(tree);

        try (var _ = typeParameterScopes.pushClassGroup(walkSuperTypesResult.mapping(), descriptorField)) {
            rewriteDefs(tree);

            Symbol.MethodSymbol fieldAccessorInstanceMethod = null;
            // for parameterized interfaces or classes that are part of a generic hierarchy
            if (
                    (classContext.classSymbol.isInterface() && classContext.classSymbol.type.isParameterized())
                            || (!classContext.classSymbol.isInterface() && highestClassInGenericHierarchy != null)
            ) {
                fieldAccessorInstanceMethod = fieldAccessorInstanceMethodSymbol(classContext.classSymbol);
                var accessor = fieldAccessorInstanceMethod(fieldAccessorInstanceMethod);
                tree.defs = tree.defs.prepend(accessor);
                classContext.classSymbol.members().enterIfAbsent(accessor.sym);
            }

            // add static field accessor if parameterized
            if (classContext.fieldAccessorStaticMethod != null) {
                var accessor = fieldAccessorStaticMethod(descriptorField, fieldAccessorInstanceMethod);
                tree.defs = tree.defs.prepend(accessor);
                classContext.classSymbol.members().enterIfAbsent(accessor.sym);
            }

            // add the computeSuperMethod if there are generic super types
            if (!walkSuperTypesResult.genericSuperTypes().isEmpty()) {
                var computeSuper = computeSuperMethod(walkSuperTypesResult.genericSuperTypes());
                tree.defs = tree.defs.prepend(computeSuper);
                classContext.classSymbol.members().enterIfAbsent(computeSuper.sym);
            }

            // add the descriptor field if we are the highest
            if (isHighestClassInGenericHierarchy) {
                tree.defs = tree.defs.prepend(make.VarDef(descriptorField, null));
                classContext.classSymbol.members().enterIfAbsent(descriptorField);
            }
        } finally {
            classContext = oldClassContext;
        }
    }

    private Symbol.VarSymbol createDescriptorFieldSymbol(Symbol.ClassSymbol owner) {
        return new Symbol.VarSymbol(
                PROTECTED | FINAL | TRANSIENT | optionalSynthetic(),
                constantHolder.objectTypeArgumentsFieldName,
                syms.classDescriptorType,
                owner
        );
    }

    private JCTree.JCMethodDecl computeSuperMethod(List<Type> genericSuperTypes) {
        var symbol = new Symbol.MethodSymbol(
                PRIVATE | STATIC,
                constantHolder.computeSuperMethodName,
                new Type.MethodType(
                        List.of(syms.classDescriptorType),
                        constantHolder.computeSuperMapType,
                        List.nil(),
                        syms.methodClass
                ),
                classContext.classSymbol
        );

        var methodDef = make.MethodDef(
                symbol,
                make.Block(0L, List.nil())
        );

        try (var _ = typeParameterScopes.pushComputeSuperGroup(methodDef.params.head.sym, symbol)) {
            methodDef.body.stats = List.of(make.Return(computeMap(genericSuperTypes)));
        }

        return methodDef;
    }

    private JCTree.JCExpression computeMap(List<Type> genericSuperTypes) {
        var call = constructorInvocation(constantHolder.hashMapConstructor);
        var args = new ListBuffer<JCTree.JCExpression>();
        genericSuperTypes.forEach(superType -> {
            var arg = classArgParam((Symbol.ClassSymbol) superType.tsym, isAccessible(superType.tsym));
            args.append(arg);
            args.append(argLiteralGenerator.generateArgs(superType, ArgLiteralGenerator.Mode.IN_SUPER));
        });

        call.args = args.toList();

        return call;
    }

    private JCTree.JCMethodDecl fieldAccessorStaticMethod(Symbol.VarSymbol descriptorField, Symbol.MethodSymbol method) {
        var methodDef = make.MethodDef(classContext.fieldAccessorStaticMethod, null);

        var nullGuard = fallbackIfStatement(methodDef.params.head.sym, make.Return(nullLiteral()));

        if (!classContext.classSymbol.isInterface()) {
            var target = make.Ident(descriptorField);
            var asSuper = instanceMethodInvocation(constantHolder.asSuperMethod, target);
            asSuper.args = List.of(make.ClassLiteral(classContext.classSymbol));

            var instr = make.Return(asSuper);

            methodDef.body = make.Block(0L, List.of(nullGuard, instr));
            return methodDef;
        }

        var fieldAccessor = instanceMethodInvocation(method, methodDef.params.head.sym);
        var fieldLocalVar = make.VarDef(
                createVariable(
                        constantHolder.objectTypeArgumentsLocalVarName,
                        syms.classDescriptorType,
                        classContext.fieldAccessorStaticMethod
                ),
                fieldAccessor
        );


        var binary = make.Binary(JCTree.Tag.EQ, make.Ident(fieldLocalVar), nullLiteral());
        binary.operator = constantHolder.objectEqOperator;
        binary.type = syms.booleanType;
        var shortCircuit = make.If(
                binary,
                make.Return(nullLiteral()),
                null
        );

        var asSuper = instanceMethodInvocation(constantHolder.asSuperMethod, make.Ident(fieldLocalVar));
        asSuper.args = List.of(make.ClassLiteral(classContext.classSymbol));

        var instr = make.Return(asSuper);

        methodDef.body = make.Block(0L, List.of(nullGuard, fieldLocalVar, shortCircuit, instr));

        return methodDef;
    }

    private Symbol.MethodSymbol fieldAccessorStaticMethodSymbol(Symbol.ClassSymbol owner) {
        return new Symbol.MethodSymbol(
                PUBLIC | STATIC | optionalSynthetic(),
                constantHolder.fieldAccessorStaticMethodName,
                new Type.MethodType(
                        List.of(owner.type),
                        syms.classDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                owner
        );
    }

    private JCTree.JCMethodDecl fieldAccessorInstanceMethod(Symbol.MethodSymbol method) {
        JCTree.JCReturn ret;
        if (classContext.descriptorField != null) {
            ret = make.Return(make.Ident(classContext.descriptorField));
        } else {
            ret = make.Return(nullLiteral());
        }
        return make.MethodDef(
                method,
                make.Block(0L, List.of(ret))
        );
    }

    private Symbol.MethodSymbol fieldAccessorInstanceMethodSymbol(Symbol.ClassSymbol owner) {
        long flags = PUBLIC | optionalSynthetic();
        if (owner.isInterface()) {
            flags |= DEFAULT;
        }
        return new Symbol.MethodSymbol(
                flags,
                names.fromString("$getDescriptor"),
                new Type.MethodType(
                        List.nil(),
                        syms.classDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                owner
        );
    }

    private WalkSuperTypesResult walkSuperTypes(JCTree.JCClassDecl tree) {
        // This is a new mapping for the class type parameters
        var paramToConcrete = new HashMap<Symbol, Type>();

        // Set containing all already visited classes/interface, in case they are implemented several times like below:
        // interface A
        // class Abstract implements A
        // class Concrete extends Abstract implements A
        var visitedClasses = new HashSet<Symbol>();

        var queue = new ArrayDeque<Type>();
        queue.addLast(tree.type);
        tree.sym.type.allparams().forEach(t -> paramToConcrete.put(t.tsym, t));

        var genericSuperTypes = new ListBuffer<Type>();

        while (!queue.isEmpty()) {
            var current = queue.removeFirst();
            var currentSymbol = (Symbol.ClassSymbol) current.tsym;

            // We fill the queue with the next interfaces and classes.
            // We only add types once, and only the parameterized ones.

            var superType = currentSymbol.getSuperclass();
            var superTypeSymbol = superType.tsym;

            if (superTypeSymbol != null) {
                queue.push(superType);
                // If the class is also parameterized, we add it to the final list
                if (superTypeSymbol.type.isParameterized() && superTypeSymbol.hasNewGenerics()) {
                    genericSuperTypes.add(superType);
                }
            }

            currentSymbol.getInterfaces().forEach(i -> {
                var tsym = i.tsym;

                if (!visitedClasses.add(tsym)) return;
                queue.add(i);

                // To be added to the final list, the interface also need to be parameterized.
                if (tsym.type.isParameterized() && tsym.hasNewGenerics()) {
                    genericSuperTypes.add(i);
                }
            });

            // Skip the class if it is not parameterized
            if (!currentSymbol.type.isParameterized()) continue;

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
            if (!current.isParameterized()) {
                continue;
            }

            // now, we must add all type parameters with a mapping to their actual value.
            var declaredParams = currentSymbol.type.allparams().iterator();
            var actualValues = current.allparams().iterator();

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
                var concrete = findConcreteType(actual, paramToConcrete);
                paramToConcrete.put(declared.tsym, concrete);
                paramToConcrete.put(actual.tsym, concrete);
            }
        }

        return new WalkSuperTypesResult(paramToConcrete, genericSuperTypes.toList());
    }

    private record WalkSuperTypesResult(
            Map<Symbol, Type> mapping,
            List<Type> genericSuperTypes
    ) {
        private WalkSuperTypesResult(
                Map<Symbol, Type> mapping,
                List<Type> genericSuperTypes
        ) {
            this.mapping = Map.copyOf(mapping);
            this.genericSuperTypes = genericSuperTypes;
        }
    }

    /// Method used while building the scope, as the typeParameterScopes does not contain the mapping for the current
    /// class, we first fetch in this map, and fallback on the typeParameterScopes
    private Type findConcreteType(Type typeVar, Map<Symbol, Type> mapping) {
        var result = mapping.get(typeVar.tsym);
        if (result != null) return result;
        return findConcreteType(typeVar);
    }

    private void rewriteDefs(JCTree.JCClassDecl tree) {
        tree.defs = filteredDefinitions(tree);
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

                // fields init have been moved to constructors and static blocks, we have nothing to do
                case VARDEF -> {
                }

                case BLOCK -> instructionVisitor.visitStaticBlock((JCTree.JCBlock) member);

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
        if (!tree.sym.type.isParameterized()) return tree.defs;
        var buffer = new ListBuffer<JCTree>();
        var staticBlock = new ListBuffer<JCTree.JCStatement>();
        tree.defs.forEach(member -> {
            switch (member.getTag()) {
                case VARDEF -> {
                    var field = (JCTree.JCVariableDecl) member;
                    if (field.init != null) { // we gather all fields with inits
                        if (field.sym.isStatic()) {
                            staticBlock.add(make.Assignment(field.sym, field.init));
                        } else {
                            classContext.inlineAndBlockDeclarations.add(make.Assignment(field.sym, field.init));
                        }
                        field.init = null;
                    }
                    buffer.add(field);
                }
                case BLOCK -> {
                    var block = (JCTree.JCBlock) member;
                    if ((block.flags & Flags.STATIC) != 0) { // static blocks are ok
                        staticBlock.addAll(block.stats);
                    } else { // we totally remove the instance blocks
                        classContext.inlineAndBlockDeclarations.addAll(block.stats);
                    }
                }
                default -> buffer.add(member);
            }
        });
        if (staticBlock.nonEmpty()) {
            buffer.append(make.Block(STATIC, staticBlock.toList()));
        }
        return buffer.toList();
    }
    //endregion

    //region rewriting (method)
    private void rewriteBasicMethod(JCTree.JCMethodDecl method) {
        if (isNative(method.sym) || method.body == null) return;

        try (var _ = typeParameterScopes.pushMethodGroup(method.sym)) {
            instructionVisitor.visitRegularMethod(method);
            adjustRegularMethodBody(method);
        }
    }

    private void rewriteConstructor(JCTree.JCMethodDecl method) {
        try (var _ = typeParameterScopes.pushMethodGroup(method.sym)) {
            var doesCallOverload = TreeInfo.hasConstructorCall(method, names._this);
            // insert the init of the fields that we move to the constructor
            if (!doesCallOverload && !classContext.inlineAndBlockDeclarations.isEmpty()) {
                insertInlineFieldsAndBlocks(method);
            }

            instructionVisitor.visitConstructor(method, typeParameterScopes.constructorPassedDescriptor());
            adjustConstructorBody(method, doesCallOverload ? null : typeParameterScopes.constructorPassedDescriptor());
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
            buffer.addAll(classContext.inlineAndBlockDeclarations);
            bodyIterator.forEachRemaining(buffer::add);
        } else { // otherwise we can just prepend the instructions
            buffer.addAll(classContext.inlineAndBlockDeclarations);
            buffer.addAll(method.body.stats);
        }
        method.body.stats = buffer.toList();
    }

    private void adjustConstructorBody(JCTree.JCMethodDecl method, Symbol.VarSymbol argsVariable) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        typeParameterScopes.generateVariables(newInstructions::append);

        if (argsVariable != null && classContext.isHighestClassInGenericHierarchy) {
            var fieldInit = make.Exec(
                    make.Assign(
                            make.Ident(classContext.descriptorField),
                            make.TypeCast(syms.classDescriptorType, make.Ident(argsVariable))
                    )
            );
            newInstructions.append(fieldInit);
        }

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private void adjustRegularMethodBody(JCTree.JCMethodDecl method) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        typeParameterScopes.generateVariables(newInstructions::append);

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private JCTree.JCStatement fallbackIf(Symbol.VarSymbol variable, JCTree.JCExpression fallback) {
        return fallbackIfStatement(
                variable,
                make.Exec(make.Assign(make.Ident(variable), fallback))
        );
    }

    private JCTree.JCStatement fallbackIfStatement(Symbol.VarSymbol variable, JCTree.JCStatement statement) {
        var binary = make.Binary(JCTree.Tag.EQ, make.Ident(variable), nullLiteral());
        binary.operator = constantHolder.objectEqOperator;
        binary.type = syms.booleanType;
        return make.If(
                binary,
                statement,
                null
        );
    }
    //endregion

    private final class InstructionVisitor extends TreeTranslator {

        private Symbol.VarSymbol constructorArgsVariable;

        private Symbol.MethodSymbol extraVariablesOwner;

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
            if (sym.attribute(syms.compilerIntrinsicType.tsym) != null) {
                handleCompilerIntrinsic(tree, sym);
                return;
            }

            if (!sym.owner.type.tsym.hasNewGenerics()) return;

            var isParameterizedMethod = sym.type.getTypeArguments().nonEmpty();
            var isConstructorFromParameterizedClass = sym.isConstructor() && sym.owner.type.isParameterized();
            if (!isParameterizedMethod && !isConstructorFromParameterizedClass) return;

            List<JCTree.JCStatement> pushStatements = List.nil();

            if (isConstructorFromParameterizedClass) {
                var push = pushSuperOrThisCode(tree);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            if (isParameterizedMethod) {
                var push = pushMethodCode(tree.typeargs, tree.meth.type.asMethodType(), sym, tree.isRaw);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            // if there is no argument, we just convert this call to a let expr with push as declarations
            if (tree.args.isEmpty()) {
                if (tree.type.hasTag(TypeTag.VOID)) {
                    result = make.Block(
                            0L,
                            List.<JCTree.JCStatement>of(make.Exec(tree)).prependList(pushStatements)
                    );
                } else {
                    result = make.LetExpr(pushStatements, tree).setType(tree.type.baseType());
                }
            } else {
                // if the method has arguments, we need to push the information after the evaluation of the last arg.
                tree.args = insertPostCall(pushStatements, tree.args);
            }
        }

        @Override
        public void visitNewClass(JCTree.JCNewClass tree) {
            super.visitNewClass(tree);

            var sym = (Symbol.MethodSymbol) tree.constructor;
            if (!sym.owner.type.tsym.hasNewGenerics()) return;

            var isParameterizedMethod = sym.type.getTypeArguments().nonEmpty();
            var isConstructorFromParameterizedClass = sym.isConstructor() && sym.owner.type.isParameterized();
            if (!isParameterizedMethod && !isConstructorFromParameterizedClass) return;

            List<JCTree.JCStatement> pushStatements = List.nil();

            // should be used instead of 'tree' when it is used as a whole (e.g., foo(tree) -> foo(actualTree))
            // 'tree' fields can still be used to access information
            JCTree.LetExpr letExpr = null;

            Symbol.VarSymbol enclVariable = null;

            if (isConstructorFromParameterizedClass) {
                if (tree.encl != null) {
                    enclVariable = createVariable(constantHolder.letExprLocalVarName, tree.encl.type, extraVariablesOwner);
                    var varDecl = make.VarDef(enclVariable, tree.encl);
                    letExpr = make.LetExpr(varDecl, tree);
                    tree.encl = make.Ident(enclVariable);
                }
                var push = pushConstructorCode(tree.type.getTypeArguments(), sym, enclVariable);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            if (isParameterizedMethod) {
                var push = pushMethodCode(tree.typeargs, tree.constructorType.asMethodType(), sym, tree.isRaw);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            if (tree.args.isEmpty()) {
                if (letExpr != null) {
                    letExpr.expr = make.LetExpr(pushStatements, letExpr.expr).setType(tree.type);
                } else {
                    letExpr = make.LetExpr(pushStatements, tree);
                }
            } else {
                // if the method has arguments, we need to push the information after the evaluation of the last arg.
                tree.args = insertPostCall(pushStatements, tree.args);
            }

            result = letExpr != null ? letExpr.setType(tree.type.baseType()) : tree;
        }

        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) { // do not recurse on inner classes
            rewriteClass(tree);
            result = tree;
        }

        @Override
        public void visitBlock(JCTree.JCBlock tree) {
            super.visitBlock(tree);
            if (!tree.isStatic() || tree.stats.isEmpty()) return;

            // TODO move this code out of InstructionVisitor, to be consistent with method and constructor
            var methodPop = staticMethodInvocation(constantHolder.methodTypeArgumentsAccessMethod);
            methodPop.args = List.of(nullLiteral());
            var methodLocalVar = createVariable(constantHolder.methodTypeArgumentsLocalVarName, syms.methodDescriptorType, extraVariablesOwner);
            var methodDef = make.VarDef(methodLocalVar, methodPop);

            var constructorPop = staticMethodInvocation(constantHolder.constructorTypeArgumentsAccessMethod);
            var constructorLocalVar = createVariable(constantHolder.constructorTypeArgumentsLocalVarName, syms.specializedTypeDescriptorType, extraVariablesOwner);
            var constructorDef = make.VarDef(constructorLocalVar, constructorPop);

            var tryBlock = make.Block(0L, tree.stats);

            // because we know that the class is always loaded either by a static access (method or field) or a constructor
            // call, we can assume that no callerClass is needed, and therefore we can push null
            var methodPush = staticMethodInvocation(constantHolder.pushMethod);
            methodPush.args = List.of(make.Ident(methodLocalVar), nullLiteral());

            var constructorPush = staticMethodInvocation(constantHolder.pushConstructor);
            constructorPush.args = List.of(make.Ident(constructorLocalVar));

            var finallyBlock = make.Block(0L, List.of(make.Exec(methodPush), make.Exec(constructorPush)));
            var tryFinally = make.Try(tryBlock, List.nil(), finallyBlock);

            tree.stats = List.of(methodDef, constructorDef, tryFinally);
        }

        //        @Override
//        public void visitLambda(JCTree.JCLambda tree) {
//            var oldState = typeParameterScopeGroupState;
//            typeParameterScopes.pushLambdaGroup(
//                    new Symbol.VarSymbol(
//                            PRIVATE | FINAL,
//                            constantHolder().objectTypeArgumentsFieldName,
//                            syms.specializedTypeType,
//                            null // we will set the actual value
//                    )
//            );
//            typeParameterScopeGroupState = typeParameterScopes.newState();
//            try {
//                super.visitLambda(tree);
//            } finally {
//                typeParameterScopeGroupState = oldState;
//            }
//        }
        public void visitRegularMethod(JCTree.JCMethodDecl method) {
            var oldExtraVariablesOwner = extraVariablesOwner;
            try {
                extraVariablesOwner = method.sym;
                visitMethodDef(method);
            } finally {
                extraVariablesOwner = oldExtraVariablesOwner;
            }
        }

        public void visitConstructor(JCTree.JCMethodDecl method, Symbol.VarSymbol constructorArgsVariable) {
            var oldConstructorArgsVariable = this.constructorArgsVariable;
            var oldExtraVariablesOwner = extraVariablesOwner;
            try {
                extraVariablesOwner = method.sym;
                this.constructorArgsVariable = constructorArgsVariable;
                method.accept(this);
            } finally {
                this.constructorArgsVariable = oldConstructorArgsVariable;
                extraVariablesOwner = oldExtraVariablesOwner;
            }
        }

        public void visitStaticBlock(JCTree.JCBlock block) {
            var oldExtraVariablesOwner = extraVariablesOwner;
            try {
                extraVariablesOwner = new Symbol.MethodSymbol(
                        STATIC,
                        names.clinit,
                        new Type.MethodType(
                                List.nil(), syms.voidType,
                                List.nil(), syms.methodClass),
                        classContext.classSymbol
                );
                visitBlock(block);
            } finally {
                extraVariablesOwner = oldExtraVariablesOwner;
            }
        }


        private JCTree.JCExpression pushMethodCode(
                List<JCTree.JCExpression> explicitTypes,
                Type.MethodType method,
                Symbol.MethodSymbol sym,
                boolean isRaw
        ) {
            var generatedArgs = basicMethodArgConstruction(
                    sym,
                    method,
                    explicitTypes,
                    isRaw,
                    ArgLiteralGenerator.Mode.DEFAULT
            );
            var pushedExpression = generatedArgs.<JCTree.JCExpression>map(l -> {
                        var call = constructorInvocation(constantHolder.specializedMethodTypeArgsConstructor);
                        call.args = l;
                        return call;
                    })
                    .orElseGet(TransParameterizedTypes.this::nullLiteral);
            var push = staticMethodInvocation(constantHolder.pushMethod);
            var caller = needsCallerInfo(sym) ? make.ClassLiteral(classContext.classSymbol) : nullLiteral();
            push.args = List.of(pushedExpression, caller);
            return push;
        }

        private JCTree.JCExpression pushConstructorCode(List<Type> types, Symbol.MethodSymbol sym, Symbol.VarSymbol enclVariable) {
            var enclosingType = sym.owner.type.getEnclosingType();
            var enclosingDescriptor = nullLiteral();
            if (enclosingType != null && !Type.noType.equals(enclosingType)) {
                // if the enclosing is parameterized, it will have a field that we can extract
                if (enclosingType.isParameterized()) {
                    if (enclVariable != null || classContext.classSymbol.isInterface()) {
                        // TODO can slightly optimize the access if the enclVariable is 'this'
                        JCTree.JCMethodInvocation extraction;
                        if (!enclosingType.tsym.hasNewGenerics()) {
                            throw new AssertionError("Not handled");
                        } else {
                            var staticAccessor = fieldAccessorStaticMethodSymbol((Symbol.ClassSymbol) enclosingType.tsym);
                            extraction = staticMethodInvocation(staticAccessor);
                        }
                        extraction.args = List.of(make.Ident(enclVariable));
                        enclosingDescriptor = extraction;
                    } else {
                        enclosingDescriptor = make.Ident(classContext.descriptorField);
                    }
                    // otherwise, it is a regular class, we can just create a class descriptor from it
                } else {
                    var type = enclVariable != null ? enclVariable.type : classContext.classSymbol.type;
                    enclosingDescriptor = argLiteralGenerator.generateArgs(type, ArgLiteralGenerator.Mode.DEFAULT);
                }
            }

            var isRaw = types.isEmpty();
            List<JCTree.JCExpression> args = isRaw ? List.nil() : types.map(
                    current -> argLiteralGenerator.generateArgs(current, ArgLiteralGenerator.Mode.DEFAULT)
            );

            var call = constructorInvocation(constantHolder.classDescriptorConstructor);
            call.args = args
                    .prepend(make.Literal(isRaw))
                    .prepend(make.ClassLiteral(sym.owner.type))
                    .prepend(enclosingDescriptor);

            var push = staticMethodInvocation(constantHolder.pushConstructor);
            push.args = List.of(call);
            return push;
        }

        private JCTree.JCExpression pushSuperOrThisCode(JCTree.JCMethodInvocation tree) {
            var push = staticMethodInvocation(constantHolder.pushConstructor);

            var methodName = Objects.requireNonNull(TreeInfo.name(tree.meth));

            // easiest case, this, we just repush the args without any modification
            if (methodName == methodName.table.names._this) {
                if (constructorArgsVariable == null) {
                    throw new AssertionError("No constructor args variable in context");
                }
                push.args = List.of(make.Ident(constructorArgsVariable));
                return push;
            }

            // for super there are two cases:
            // 1. we are in a parameterized class -> just push the constructor argument
            // 2. we are in a regular class -> we need to compute the super

            if (constructorArgsVariable != null) {
                push.args = List.of(make.Ident(constructorArgsVariable));
                return push;
            }

            var descriptor = argLiteralGenerator.generateArgs(classContext.classSymbol.getSuperclass(), ArgLiteralGenerator.Mode.IN_SUPER);
            push.args = List.of(descriptor);

            return push;
        }


        private Optional<List<JCTree.JCExpression>> basicMethodArgConstruction(
                Symbol.MethodSymbol sym,
                Type.MethodType methodType,
                List<JCTree.JCExpression> explicitTypes,
                boolean isRaw,
                ArgLiteralGenerator.Mode mode
        ) {
            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
            // use the inferred types `String s = foo();`
            if (!explicitTypes.isEmpty()) { // provided type arguments
                var list = explicitTypes.map(t -> argLiteralGenerator.generateArgs(t.type, mode));
                return Optional.of(list);
            }

            var inferredTypes = methodType.inferenceMapping;
            if (isRaw || inferredTypes == null || inferredTypes.isEmpty()) {
                return Optional.empty();
            }

            var list = sym.type
                    .getTypeArguments()
                    .map(t -> argLiteralGenerator.generateArgs(computeTypeFromInference(inferredTypes, t), mode));
            return Optional.of(list);
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

        private List<JCTree.JCExpression> insertPostCall(
                List<JCTree.JCStatement> letStatements,
                List<JCTree.JCExpression> arguments
        ) {
            var newArguments = new ListBuffer<JCTree.JCExpression>();
            var iterator = arguments.iterator();
            while (iterator.hasNext()) {
                var next = iterator.next();
                if (iterator.hasNext()) {
                    newArguments.append(next);
                    continue;
                }

                var tempVariable = createVariable(constantHolder.letExprLocalVarName, next.type, extraVariablesOwner);
                var statements = letStatements.prepend(make.VarDef(tempVariable, next));
                var let = make.LetExpr(statements, make.Ident(tempVariable)).setType(tempVariable.type.baseType());
                newArguments.append(let);
            }

            return newArguments.toList();
        }

        private void handleCompilerIntrinsic(JCTree.JCMethodInvocation tree, Symbol.MethodSymbol sym) {
            if (!syms.specializedTypeDescriptorType.equals(sym.owner.type)) return;

            switch (sym.name.toString()) {
                case "of" -> {
                    var methodType = tree.meth.type.asMethodType();
                    if (tree.typeargs.isEmpty()) {
                        result = staticMethodInvocation(constantHolder.optionalEmptyMethod);
                        break;
                    }
                    var generatedArgs = basicMethodArgConstruction(
                            sym,
                            methodType,
                            tree.typeargs,
                            false,
                            ArgLiteralGenerator.Mode.DEFAULT
                    );
                    if (generatedArgs.isEmpty()) {
                        throw new AssertionError();
                    }
                    var args = generatedArgs.get().head;
                    var optWrap = staticMethodInvocation(constantHolder.optionalOfNullableMethod);
                    optWrap.args = List.of(args);
                    result = optWrap;
                }
                case "from" -> {
                    var argSym = (Symbol.ClassSymbol) tree.args.head.type.tsym;
                    if (!argSym.hasNewGenerics() || !argSym.type.isParameterized()) {
                        result = staticMethodInvocation(constantHolder.optionalEmptyMethod);
                        break;
                    }
                    var r = argLiteralGenerator.generateArgs(tree.args.head.type, ArgLiteralGenerator.Mode.DEFAULT);
                    var optWrap = staticMethodInvocation(constantHolder.optionalOfNullableMethod);
                    optWrap.args = List.of(r);
                    result = optWrap;
                }
            }
        }

    }

    private final class ArgLiteralGenerator {

        private enum Mode {
            DEFAULT,
            IN_SUPER,
            ;
        }

        private Mode mode;

        public JCTree.JCExpression generateArgs(Type current, Mode mode) {
            this.mode = mode;
            return actualGenerateArgs(current);
        }

        private JCTree.JCExpression actualGenerateArgs(Type current) {
            return switch (current.getKind()) {
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
        }

        private JCTree.JCExpression generateArrayKind(Type.ArrayType type) {
            var call = constructorInvocation(constantHolder.arrayTypeConstructor);
            call.args = List.of(actualGenerateArgs(type.elemtype));
            return call;
        }

        private JCTree.JCExpression generateWildcardKind(Type.WildcardType type) {
            return staticMethodInvocation(constantHolder.erasedTypeInstanceMethod);
        }

        private JCTree.JCExpression generateIntersectionKind(Type.IntersectionClassType type) {
            return staticMethodInvocation(constantHolder.erasedTypeInstanceMethod);
        }

        private JCTree.JCExpression generateClassKind(Type.ClassType type) {
            var outerConstruction = generateOuterClass(type);

            var tsym = (Symbol.ClassSymbol) type.tsym;
            var isAccessible = isAccessible(tsym);
            var classFieldAcc = classArgParam(tsym, isAccessible);

            var call = constructorInvocation(constantHolder.classDescriptorConstructor);

            List<JCTree.JCExpression> typeArguments = List.nil();

            if (type.getTypeArguments().nonEmpty()) { // Foo<E> (E can be a wildcard)
                typeArguments = type.getTypeArguments().map(this::actualGenerateArgs);
            }

            // for raw and non parameterized classes typeArguments keeps the empty list value
            call.args = typeArguments
                    .prepend(make.Literal(type.isRaw()))
                    .prepend(classFieldAcc)
                    .prepend(outerConstruction);
            return call;
        }

        private JCTree.JCExpression generateTypeVarKind(Type.TypeVar type) {
            if (mode == Mode.IN_SUPER) {
                var actual = findConcreteType(type);
                if (actual != type) { // if a mapping is provided, try to find the actual type var
                    return actualGenerateArgs(actual);
                }
            }

            var owner = type.tsym.owner;
            var index = owner.type.getTypeArguments().indexOf(type);
            if (index == -1) {
                index = owner.type.allparams().indexOf(type);
            }

            // if the owner of this type does not have it in its declared type parameters, it is a wildcard
            if (index == -1) { // wildcard
                return staticMethodInvocation(constantHolder.erasedTypeInstanceMethod);
            }

            return typeVarResolution(type.tsym);
        }

        private JCTree.JCExpression generatePrimitiveType(Type.JCPrimitiveType type) {
            var classFieldAcc = make.ClassLiteral(type);
            var call = constructorInvocation(constantHolder.classDescriptorConstructor);
            call.args = List.of(nullLiteral(), classFieldAcc, make.Literal(false));
            return call;
        }

        private JCTree.JCExpression generateOuterClass(Type current) {
            var enclosingType = current.getEnclosingType();
            // if there is no enclosing type, we have nothing to do
            if (enclosingType == null || Type.noType.equals(enclosingType)) return nullLiteral();

            // if we are in an inner class, we also need to generate the args for the enclosing type
            return actualGenerateArgs(enclosingType);
        }

        /// Resolves the usage of a type variable. This method generates the code that fetch the information of a type
        /// parameter at runtime.
        ///
        /// @param typeVar the symbol of the type variable that we are looking for
        private JCTree.JCExpression typeVarResolution(Symbol.TypeSymbol typeVar) {
            var res = typeParameterScopes.resolve(typeVar);
            if (res != null) return res;


            // this part is used for external field assign.

//        if (externalFieldOwner == null) {
            throw new AssertionError("Could not find type var " + typeVar + " in class " + classContext.classSymbol + " with the following scope " + typeParameterScopes);
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


    }

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
        if (classContext.classSymbol.isInterface()/* || typeVar.tsym.owner.kind != Kinds.Kind.TYP*/) return typeVar;
        return typeParameterScopes.mapToConcrete(typeVar);
    }
    //endregion

    private final class ParameterizedScope {
        // Invariants:
        // - Each pushClass also pushes a mapping. If the class actually have no mapping, it pushes an empty map
        // - Each pushMethod also pushes a new state.
        private List<Group> groups = List.nil();

        /// Stack containing the mappings between a symbol and its type. It is used to map type variables from transitive
        /// super types to their actual type.
        ///
        /// Here is an example of a mapping:
        ///
        /// A&lt;T&gt; implements B&lt;E&gt; and B&lt;E&gt; implements C&lt;F&gt;
        ///
        /// this map provides the following mappings:
        ///
        ///   - T -> T (self referencing)
        ///   - E -> T
        ///   - F -> T (flattened transitive relation F -> E -> T)
        ///
        /// This is later used for retrieve the actual argument from the current scope
        private List<Map<Symbol, Type>> mappings = List.nil();
        private List<GroupState> states = List.nil();
        private Symbol.VarSymbol constructorPassedDescriptor;

        public GroupRemover pushClassGroup(Map<Symbol, Type> mappings, Symbol.VarSymbol descriptorField) {
            this.mappings = this.mappings.prepend(Map.copyOf(mappings));
            var typeArguments = getAllParams(classContext.classSymbol);
            var addedCount = 0;

            if (classContext.classSymbol.type.isParameterized()) {
                var group = classContext.classSymbol.isInterface()
                        ? new InterfaceGroup(typeArguments)
                        : new ClassGroup(typeArguments, descriptorField);

                groups = groups.prepend(group);
                addedCount++;
            }
            return new GroupRemover(addedCount, this, GroupRemover.Action.POP_MAPPING);
        }

        public GroupRemover pushMethodGroup(Symbol.MethodSymbol method) {
            var id = new GroupStateId();
            var addedCount = 0;

            if (method.isConstructor() && method.owner.type.isParameterized()) {
                // if we are in a constructor we know that the topmost group is a class group.
                var constructorGroup = new ConstructorGroup(groups.head.variableParams(), id, method);
                groups = groups.prepend(constructorGroup);
                constructorPassedDescriptor = constructorGroup.variable(method);
                addedCount++;
            }

            if (method.type.getTypeArguments().nonEmpty()) {
                groups = groups.prepend(new MethodGroup(getTypeArguments(method), id, method));
                addedCount++;
            }

            states = states.prepend(new GroupState(id, groups, method));
            return new GroupRemover(addedCount, this, GroupRemover.Action.POP_STATE);
        }

        public GroupRemover pushComputeSuperGroup(Symbol.VarSymbol variable, Symbol.MethodSymbol method) {
            var id = new GroupStateId();

            groups = groups.prepend(new ComputeSuperGroup(getTypeArguments(classContext.classSymbol), id, variable));
            states = states.prepend(new GroupState(id, groups, method));
            return new GroupRemover(1, this, GroupRemover.Action.POP_STATE);
        }

        public Symbol.VarSymbol constructorPassedDescriptor() {
            return constructorPassedDescriptor;
        }

        public void generateVariables(Consumer<JCTree.JCStatement> statementConsumer) {
            states.head.generateVariables(statementConsumer);
        }

        public JCTree.JCExpression resolve(Symbol.TypeSymbol typeVar) {
            var groupIndex = 0;
            for (var group : groups) {
                var index = group.index(typeVar);
                if (index == null) {
                    groupIndex++;
                    continue;
                }
                var variable = states.head.variable(groupIndex);
                JCTree.JCExpression result = make.Ident(variable);
                for (int i = 0; i < index.outerIndex(); i++) {
                    result = instanceMethodInvocation(constantHolder.outerMethod, result);
                }
                if (group.variableIsMethodDescriptor()) {
                    var call = instanceMethodInvocation(constantHolder.extractFromMethodMethod, variable);
                    call.args = List.of(make.Literal(index.index()));
                    result = call;
                } else {
                    var call = instanceMethodInvocation(constantHolder.extractFromClassMethod, result);
                    call.args = List.of(make.Literal(index.index()));
                    result = call;
                }
                return result;
            }
            return null;
        }

        public Type mapToConcrete(Type typeVar) {
            for (var mapping : mappings) {
                var type = mapping.get(typeVar.tsym);
                if (type != null) {
                    return type;
                }
            }

            throw new AssertionError("Type not found in mapping: " + typeVar + " in " + mappings);
        }

        private static final class GroupStateId {
        }

        public static final class GroupState {
            private final List<Slot> groups;
            private final GroupStateId id;

            private GroupState(GroupStateId id, List<Group> groups, Symbol variableOwner) {
                this.id = id;
                this.groups = groups.map(g -> new Slot(g, g.variable(variableOwner)));
            }

            public Symbol.VarSymbol variable(int index) {
                Objects.checkIndex(index, groups.size());

                // we also mark the group as used
                var slot = groups.get(index);
                slot.used = slot.group.markAsUsed();

                return groups.get(index).variable;
            }

            public void generateVariables(Consumer<JCTree.JCStatement> statementConsumer) {
                groups.forEach(slot -> {
                    if (!slot.group.shouldGenerate(slot.used, id)) {
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

        private record TypeVarIndex(int index, int outerIndex) {
        }

        sealed interface Group {

            TypeVarIndex index(Symbol.TypeSymbol typeVar);

            void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            );

            Symbol.VarSymbol variable(Symbol currentOwner);

            /// Notify the group that it has been used and returns whether the local state should also keep track of
            /// this usage.
            boolean markAsUsed();

            boolean used();

            boolean shouldGenerate(boolean usedInState, GroupStateId currentState);

            boolean variableIsMethodDescriptor();

            List<List<Symbol.TypeSymbol>> variableParams();

        }

        private static abstract sealed class Base implements Group {
            private final List<List<Symbol.TypeSymbol>> variableParams;
            protected final GroupStateId owner;
            private final boolean methodDescriptor;
            private boolean used;

            protected Base(List<List<Symbol.TypeSymbol>> variableParams, GroupStateId owner, boolean methodDescriptor) {
                this.variableParams = variableParams;
                this.owner = owner;
                this.methodDescriptor = methodDescriptor;
            }

            @Override
            public final TypeVarIndex index(Symbol.TypeSymbol typeVar) {
                var outerIndex = 0;
                for (var variableParam : variableParams) {
                    var index = variableParam.indexOf(typeVar);
                    if (index != -1) return new TypeVarIndex(index, outerIndex);
                    outerIndex++;
                }
                return null;
            }

            @Override
            public final List<List<Symbol.TypeSymbol>> variableParams() {
                return variableParams;
            }

            @Override
            public final boolean markAsUsed() {
                used = true;
                return shouldSaveInLocal();
            }

            @Override
            public final boolean used() {
                return used;
            }

            @Override
            public final boolean variableIsMethodDescriptor() {
                return methodDescriptor;
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
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.MethodSymbol method
            ) {
                super(List.of(variableParams), owner, true);
                this.method = method;
                this.variable = createVariable(
                        constantHolder.methodTypeArgumentsLocalVarName,
                        syms.methodDescriptorType,
                        method
                );
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                if (!used()) { // if not used, we at least avoid generating a local variable
                    var call = staticMethodInvocation(constantHolder.popMethod);
                    statementConsumer.accept(make.Exec(call));
                    return;
                }

                // MethodTypeArgs methodTypeArgs = null;
                statementConsumer.accept(make.VarDef(declarationVariable, nullLiteral()));

                JCTree.JCMethodInvocation init;

                // For methods that can be overriden outside the prototype, we need to add a check using the caller class.
                if (!needsCallerInfo(method)) {
                    init = staticMethodInvocation(constantHolder.methodTypeArgumentsAccessMethod);
                    init.args = List.of(nullLiteral());
                } else {
                    var getInstance = staticMethodInvocation(constantHolder.argStackWalkerMethod);
                    var callerClass = instanceMethodInvocation(constantHolder.stackWalkerGetCallerClassMethod, getInstance);
                    init = staticMethodInvocation(constantHolder.methodTypeArgumentsAccessMethod);
                    init.args = List.of(callerClass);

                }

                var assign = make.Assign(make.Ident(declarationVariable), init);
                statementConsumer.accept(make.Exec(assign));

                // if (methodTypeArgs == null) methodTypeArgs = *raw type arguments;
                var mArgsFallback = staticMethodInvocation(constantHolder.rawMethodDescriptorMethod);
                statementConsumer.accept(
                        fallbackIf(
                                declarationVariable,
                                mArgsFallback
                        )
                );
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                return currentState == owner; // we always generate the code, as we at least need to pop the information
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }
        }

        private static final class ClassGroup extends Base {
            private final Symbol.VarSymbol variable;

            private ClassGroup(List<List<Symbol.TypeSymbol>> variableParams, Symbol.VarSymbol descriptorField) {
                super(variableParams, null, false);
                Objects.requireNonNull(descriptorField);
                this.variable = descriptorField;
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
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

            private InterfaceGroup(List<List<Symbol.TypeSymbol>> variableParams) {
                super(variableParams, null, false);
                this.type = classContext.classSymbol;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return true;
            }

            @Override
            public void declaration(Symbol.VarSymbol declarationVariable, Consumer<JCTree.JCStatement> statementConsumer) {
                // SpecializedType args = ...;
                var init = staticMethodInvocation(classContext.fieldAccessorStaticMethod);
                init.args = List.of(make.This(type.type));
                var decl = make.VarDef(declarationVariable, init);
                statementConsumer.accept(decl);

                // if (args == null) raw version
                var fallback = fallbackIf(declarationVariable, rawTypeCreation(classContext.classSymbol.type));
                statementConsumer.accept(fallback);
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return createVariable(
                        constantHolder.objectTypeArgumentsLocalVarName,
                        syms.classDescriptorType,
                        currentOwner
                );
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                // FIXME, maybe we need to change the whole system to avoid generating too much interface access.
                return usedInState;
            }
        }

        private final class ConstructorGroup extends Base {
            private final Symbol.VarSymbol variable;

            private ConstructorGroup(
                    List<List<Symbol.TypeSymbol>> variableParams,
                    GroupStateId owner,
                    Symbol.MethodSymbol constructor
            ) {
                super(variableParams, owner, false);
                this.variable = createVariable(constantHolder.constructorTypeArgumentsLocalVarName, syms.specializedTypeDescriptorType, constructor);
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                // SpecializedType constructorArguments = MethodArgStack.constructorTypeArguments();
                var call = staticMethodInvocation(constantHolder.constructorTypeArgumentsAccessMethod);
                statementConsumer.accept(make.VarDef(declarationVariable, call));

                // if (args == null) args = *create raw type*;
                statementConsumer.accept(
                        fallbackIf(
                                declarationVariable,
                                rawTypeCreation(classContext.classSymbol.type)
                        )
                );
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                // We always need to generate it in its constructor as it will at least be used to setup the
                // constructorTypeArgs.
                return currentState == owner;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }
        }

        private static final class ComputeSuperGroup extends Base {

            private final Symbol.VarSymbol variable;

            private ComputeSuperGroup(
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.VarSymbol variable
            ) {
                super(List.of(variableParams), owner, false);
                this.variable = variable;
            }


            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }

            @Override
            public void declaration(Symbol.VarSymbol declarationVariable, Consumer<JCTree.JCStatement> statementConsumer) {

            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                return false;
            }
        }

        private static class GroupRemover implements AutoCloseable {
            private final int count;
            private final ParameterizedScope scope;
            private final Action action;
            private boolean closed;

            private GroupRemover(int count, ParameterizedScope scope, Action action) {
                if (count < 0) throw new IllegalArgumentException("count = " + count + " < 0");
                this.count = count;
                this.scope = scope;
                this.action = action;
            }

            @Override
            public void close() {
                if (closed) throw new AssertionError();
                closed = true;
                for (var i = 0; i < count; i++) {
                    scope.groups = scope.groups.tail;
                }
                switch (action) {
                    case POP_STATE -> {
                        scope.states = scope.states.tail;
                        scope.constructorPassedDescriptor = null;
                    }
                    case POP_MAPPING -> scope.mappings = scope.mappings.tail;
                }
            }

            enum Action {
                POP_STATE,
                POP_MAPPING,
                ;
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

    private JCTree.JCExpression rawTypeCreation(Type type) {
        if (type == null || Type.noType.equals(type)) return nullLiteral();

        var outerConstruction = rawTypeCreation(type.getEnclosingType());

        var tsym = (Symbol.ClassSymbol) type.tsym;
        var isAccessible = isAccessible(tsym);
        var classFieldAcc = classArgParam(tsym, isAccessible);

        var typeArguments = erasedList(tsym);

        var call = constructorInvocation(constantHolder.classDescriptorConstructor);
        call.args = typeArguments
                .prepend(make.Literal(true))
                .prepend(classFieldAcc)
                .prepend(outerConstruction);

        return call;
    }

    private List<JCTree.JCExpression> erasedList(Symbol sym) {
        var erasedList = List.<JCTree.JCExpression>nil();
        for (var current = sym.type.getTypeArguments(); current.nonEmpty(); current = current.tail) {
            var arg = staticMethodInvocation(constantHolder.erasedTypeInstanceMethod);
            erasedList = erasedList.prepend(arg);
        }
        return erasedList;
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
        if (isAccessible) return make.ClassLiteral(sym);
        var fqName = make.Literal(sym.flatname.toString());
        var call = staticMethodInvocation(constantHolder.findClassByNameMethod);
        call.args = List.of(fqName);
        return call;
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
                0L, name.append(names.fromString(String.valueOf(offset))),
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
    private static List<List<Symbol.TypeSymbol>> getAllParams(Symbol.ClassSymbol sym) {
        var list = new ListBuffer<List<Symbol.TypeSymbol>>();
        var current = sym;
        while (true) {
            list = list.append(getTypeArguments(current));
            var next = current.owner;
            if (!(next instanceof Symbol.ClassSymbol cl)) break;
            current = cl;
        }
        return list.toList();
    }

    private static List<Symbol.TypeSymbol> getTypeArguments(Symbol sym) {
        var list = new ListBuffer<Symbol.TypeSymbol>();
        sym.type.getTypeArguments().forEach(t -> list.add(t.tsym));
        return list.toList();
    }


    private static Symbol.ClassSymbol highestGenericSuperType(Symbol.ClassSymbol sym) {
        if (sym.isInterface()) return null;

        Symbol.ClassSymbol result = null;
        var current = sym;
        while (true) {
            if (isGenericOrHasGenericInterface(current)) {
                result = current;
            }
            var next = current.getSuperclass();
            if (next.hasTag(Type.noType.getTag())) {
                break;
            }
            current = (Symbol.ClassSymbol) next.tsym;
        }

        return result;
    }

    /// We only check 1 level for the super class, but the whole hierarchy tree for interfaces
    private static boolean isGenericOrHasGenericInterface(Symbol.ClassSymbol sym) {
        if (sym.type.isParameterized()) return true;
        var interfaces = sym.getInterfaces();
        while (interfaces.nonEmpty()) {
            var next = interfaces.head;
            if (next.isParameterized()) return true;
            interfaces = interfaces.tail;
            for (var itf : ((Symbol.ClassSymbol) next.tsym).getInterfaces()) {
                interfaces = interfaces.prepend(itf);
            }
        }
        return false;
    }

    private int optionalSynthetic() {
        return enableSynthetic ? SYNTHETIC : 0;
    }
    //endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree classDef, TreeMaker make) {
        Objects.requireNonNull(env);
        Objects.requireNonNull(classDef);
        Objects.requireNonNull(make);
        if (!enabled) return classDef;
        if (constantHolder == null) constantHolder = new ConstantHolder();
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
