package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.*;
import com.sun.tools.javac.jvm.ByteCodes;
import com.sun.tools.javac.jvm.PoolConstant;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.*;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

import static com.sun.tools.javac.code.Flags.*;

public final class TransParameterizedTypes {

    //region fields
    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private static final int CONSTANT_DESCRIPTOR_BSM_FLAG_RAW = 1 << 1;

    private final boolean enableSynthetic;
    private final boolean enabled;

    private final InstructionVisitor instructionVisitor;
    private final ArgLiteralGenerator argLiteralGenerator;
    private final ParameterizedScope typeParameterScopes;
    private final Translator translator;
    private ConstantHolder constantsHolder;

    private final Log log;
    private final Symtab syms;
    private final Names names;
    private final Types types;
    private final Operators operators;
    private final Resolve resolve;
    private final LambdaToMethod lambdaToMethod; // TODO avoid using LambdaToMethod to convert type do string desc

    private Env<AttrContext> env;
    private TreeMaker make;

    private ClassContext classContext;

    /// Simple value appended to generated local variable names to easily identify them (e.g. x$34). We assume that
    /// there will never be more than 99.
    private int nameOffsetIndex;

    private static final class ClassContext {
        private final JCTree.JCClassDecl classTree;
        private final Symbol.ClassSymbol classSymbol;
        private final Symbol.VarSymbol descriptorField;
        private final boolean isHighestClassInGenericHierarchy;
        private final boolean isInGenericHierarchy;
        private final Symbol.MethodSymbol computeSuperMethod;

        /// Field containing all the fields and blocks that are declared in the current class. These declarations must
        /// be moved to the constructor to be able to access the method parameters.
        private List<JCTree.JCStatement> inlineAndBlockDeclarations = List.nil();

        private ClassContext(
                JCTree.JCClassDecl classTree,
                Symbol.ClassSymbol classSymbol,
                Symbol.VarSymbol descriptorField,
                boolean isHighestClassInGenericHierarchy,
                boolean isInGenericHierarchy,
                Symbol.MethodSymbol computeSuperMethod
        ) {
            this.classTree = classTree;
            this.classSymbol = classSymbol;
            this.descriptorField = descriptorField;
            this.isHighestClassInGenericHierarchy = isHighestClassInGenericHierarchy;
            this.isInGenericHierarchy = isInGenericHierarchy;
            this.computeSuperMethod = computeSuperMethod;
        }

    }

    private final class ConstantHolder {

        public final Name objectTypeArgumentsFieldName = names.fromString("$typeArguments");

        public final Name populateSuperMethodName = names.fromString("$populateSuperSet");

        public final StaticMethod methodTypeArguments = new StaticMethod(
                names.fromString("methodTypeArguments"),
                syms.typeDescriptorPassingHandlerType
        );

        public final StaticMethod lambdaTypeArguments = new StaticMethod(
                names.fromString("lambdaTypeArguments"),
                syms.typeDescriptorPassingHandlerType
        );

        public final StaticMethod constructorTypeArguments = new StaticMethod(
                names.fromString("constructorTypeArguments"),
                syms.typeDescriptorPassingHandlerType
        );

        public final InstanceMethod typeArgument = new InstanceMethod(
                names.fromString("typeArgument"),
                syms.typeDescriptorAccessorType
        );

        public final InstanceMethod $outer = new InstanceMethod(
                names.fromString("$outer"),
                syms.classDescriptorType
        );

        public final InstanceMethod viewAsSuper = new InstanceMethod(
                names.fromString("viewAsSuper"),
                syms.derivedClassDescriptorType
        );

        public final StaticMethod arrayTypeOf = new StaticMethod(
                names.of,
                syms.arrayDescriptorType
        );

        public final StaticMethod methodDescriptorOf = new StaticMethod(
                names.of,
                syms.methodDescriptorType
        );

        public final StaticMethod classDescriptorOf = new StaticMethod(
                names.of,
                syms.classDescriptorType
        );

        public final StaticMethod hiddenClassDescriptorOf = new StaticMethod(
                names.of,
                syms.hiddenClassDescriptorType
        );

        public final StaticMethod classDescriptorOfRaw = new StaticMethod(
                names.fromString("ofRaw"),
                syms.classDescriptorType
        );

        public final StaticMethod erasedClassDescriptorInstance = new StaticMethod(
                names.fromString("instance"),
                syms.erasedClassDescriptorType
        );

        public final StaticMethod pushMethod = new StaticMethod(
                names.fromString("pushMethod"),
                syms.typeDescriptorPassingHandlerType
        );

        public final StaticMethod pushConstructor = new StaticMethod(
                names.fromString("pushConstructor"),
                syms.typeDescriptorPassingHandlerType
        );

        public final StaticMethod pushHiddenClass = new StaticMethod(
                names.fromString("pushHiddenClass"),
                syms.typeDescriptorPassingHandlerType
        );

        public final StaticMethod filterPartialDescriptor = new StaticMethod(
                names.fromString("filterPartialDescriptor"),
                syms.specializedTypeDescriptorType
        );

        public final StaticMethod classDescriptor$From = new StaticMethod(
                names.fromString("$from"),
                syms.specializedTypeDescriptorType
        );

        public final BootstrapMethod constantClassDescriptorBsm = new BootstrapMethod(
                names.fromString("classDescriptor"),
                syms.constantTypeDescriptorsType,
                syms.classDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod constantArrayDescriptorBsm = new BootstrapMethod(
                names.fromString("arrayDescriptor"),
                syms.constantTypeDescriptorsType,
                syms.specializedTypeDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod erasedTypeDescriptorBsm = new BootstrapMethod(
                names.fromString("erasedClassDescriptor"),
                syms.constantTypeDescriptorsType,
                syms.erasedClassDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod constantMethodDescriptorBsm = new BootstrapMethod(
                names.fromString("methodDescriptor"),
                syms.constantTypeDescriptorsType,
                syms.methodDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

       public final BootstrapMethod constantHiddenClassDescriptorBsm = new BootstrapMethod(
                names.fromString("hiddenClassDescriptor"),
                syms.constantTypeDescriptorsType,
                syms.hiddenClassDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final ConstructorMethod newSuperTypeMapping = new ConstructorMethod(
                syms.superTypeMappingType
        );

        public final InstanceMethod setAdd = new InstanceMethod(
                names.fromString("add"),
                syms.specializedTypeHashSetType
        );

        public final Symbol.OperatorSymbol objectEqOperator = operators
                .lookupBinaryOp(o -> o.opcode == ByteCodes.if_acmpeq);

        public final Attribute.Compound instrumentedAnnotation = new Attribute.Compound(
                syms.instrumentedAnnotationType,
                List.nil(),
                null
        );

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
        types = Types.instance(context);
        operators = Operators.instance(context);
        resolve = Resolve.instance(context);
        lambdaToMethod = LambdaToMethod.instance(context);
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
        if (module == null) return true;
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

        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) {
            result = tree;

            if (!tree.sym.hasNewGenerics()) {
                super.visitClassDef(tree);
                return;
            }

            if (!enabled) return;

            if (constantsHolder == null) constantsHolder = new ConstantHolder();

            try {
                rewriteClass(tree);
            } catch (RuntimeException | AssertionError t) {
                log.printRawLines("error in class: " + tree.sym.fullname);
                throw t;
            }
        }

        @Override
        public void visitMethodDef(JCTree.JCMethodDecl tree) {
            super.visitMethodDef(tree);
//            if (hasPrototypeInternal(tree.sym)) {
//                tree.sym.flags_field |= SYNTHETIC;
//            }
        }

    }

    private void rewriteClass(JCTree.JCClassDecl tree) {
        // we add the instrumented annotation
        tree.sym.appendAttributes(List.of(constantsHolder.instrumentedAnnotation));

        var oldClassContext = classContext;

        var highestClassInGenericHierarchy = highestConcreteGenericSuperType(tree.sym);
        var isHighestClassInGenericHierarchy = tree.sym == highestClassInGenericHierarchy;

        Symbol.VarSymbol descriptorField = null;
        if (highestClassInGenericHierarchy != null) {
            if (isHighestClassInGenericHierarchy) {
                descriptorField = tree.sym.descriptorField = createDescriptorFieldSymbol(tree.sym);
            } else {
                var it = highestClassInGenericHierarchy.members().getSymbolsByName(constantsHolder.objectTypeArgumentsFieldName, Scope.LookupKind.NON_RECURSIVE).iterator();
                Assert.check(it.hasNext());
                descriptorField = (Symbol.VarSymbol) it.next();
            }
        }

        Symbol.MethodSymbol computeSuper = null;
        if (tree.sym.hasPopulateSuperMethod()) {
            computeSuper = populateSuperMethodSymbol(tree.sym);
        }

        classContext = new ClassContext(
                tree,
                tree.sym,
                descriptorField,
                isHighestClassInGenericHierarchy,
                highestClassInGenericHierarchy != null,
                computeSuper
        );

        try (var _ = typeParameterScopes.pushClassScope(descriptorField)) {
            rewriteDefs(tree);

            var currentClass = classContext.classSymbol;

            // if we are parameterized or if the current class is plain but is under a generic interface
            if (
                    currentClass.hasNewGenerics()
                            && !currentClass.isInterface()
                            && (currentClass.type.isParameterized() || currentClass == highestClassInGenericHierarchy)
            ) {
                // in case the field has not been initialized
                currentClass.getInterfaces();

                // we make the type implement the ClassDescriptorHolder interface
                var type = (Type.ClassType) currentClass.type;
                type.interfaces_field = type.interfaces_field.prepend(syms.classDescriptorHolderType);
                tree.implementing = tree.implementing.prepend(make.Type(syms.classDescriptorHolderType));

                var fieldAccessorInstanceMethod = fieldAccessorInstanceMethodSymbol(currentClass);
                var accessor = fieldAccessorInstanceMethod(fieldAccessorInstanceMethod);
                tree.defs = tree.defs.prepend(accessor);
                currentClass.members().enterIfAbsent(accessor.sym);
            }

            if (classContext.computeSuperMethod != null) {
//                var computeSuperDef = computePopulateSuperMethod(
//                        classContext.computeSuperMethod,
//                        List.of(currentClass.type),
//                        getTypeArguments(tree.sym),
//                        false
//                );
//                tree.defs = tree.defs.prepend(computeSuperDef);
//                currentClass.members().enterIfAbsent(computeSuperDef.sym);
            }

            // add the descriptor field if we are the highest
            if (isHighestClassInGenericHierarchy) {
                tree.defs = tree.defs.prepend(make.VarDef(descriptorField, null));
                currentClass.members().enterIfAbsent(descriptorField);
            }
        } finally {
            classContext = oldClassContext;
        }
    }

    private Symbol.VarSymbol createDescriptorFieldSymbol(Symbol.ClassSymbol owner) {
        return new Symbol.VarSymbol(
                PRIVATE | FINAL | TRANSIENT | optionalSynthetic(),
                constantsHolder.objectTypeArgumentsFieldName,
                syms.classDescriptorType,
                owner
        );
    }

    private JCTree.JCMethodDecl computePopulateSuperMethod(
            Symbol.MethodSymbol method,
            List<Type> types,
            List<Symbol.TypeSymbol> typeArguments,
            boolean includeDirect
    ) {
        var methodDef = make.MethodDef(
                method,
                make.Block(0L, List.nil())
        );

        methodDef.body.stats = computeSuperStatements(
                method,
                methodDef.params.getFirst().sym,
                methodDef.params.get(1).sym,
                types,
                typeArguments,
                includeDirect
        );

        return methodDef;
    }

    private List<JCTree.JCStatement> computeSuperStatements(
            Symbol.MethodSymbol symbol,
            Symbol.VarSymbol accessor,
            Symbol.VarSymbol set,
            List<Type> types,
            List<Symbol.TypeSymbol> typeArguments,
            boolean includeDirect
    ) {
        try (var _ = typeParameterScopes.pushComputeSuperGroup(accessor, symbol, typeArguments)) {
            var statements = new ListBuffer<JCTree.JCStatement>();

            types.forEach(type -> {
                if (type == syms.objectType) return;
                var sym = (Symbol.ClassSymbol) type.tsym;

                if (includeDirect) {
                    populateWith(symbol, statements, type, accessor, set);
                    return;
                }

                if (!sym.isInterface()) {
                    populateWith(symbol, statements, sym.getSuperclass(), accessor, set);
                }
                for (var t : sym.getInterfaces()) {
                    populateWith(symbol, statements, t, accessor, set);
                }
            });

            return statements.toList();
        }
    }

    private void populateWith(
            Symbol.MethodSymbol enclosingMethod,
            ListBuffer<JCTree.JCStatement> statements,
            Type type,
            Symbol.VarSymbol descriptor,
            Symbol.VarSymbol set
    ) {
        var sym = (Symbol.ClassSymbol) type.tsym;
        if (!sym.hasPopulateSuperMethod()) {
            return;
        }

        Symbol.VarSymbol superCallDescriptor = descriptor;
        // set.add(...);
        // we only add to the set if the type is parameterized, otherwise, we only call the super method.
        // interface A<T>
        // interface B extends A<String>
        // interface C extends B
        // here we want to call the populate method from B, but there is no reason to add B to the map
        if (sym.type.isParameterized()) {
            var value = argLiteralGenerator.generateArgs(type);
            var superDescriptorVar = make.VarDef(
                    createVariable(syms.classDescriptorType, enclosingMethod),
                    value
            );
            statements.add(superDescriptorVar);
            var mapping = constantsHolder.newSuperTypeMapping.call(
                    classLiteral(type),
                    make.Ident(superDescriptorVar)
            );
            var addCall = constantsHolder.setAdd.call(set, mapping);
            statements.add(make.Exec(addCall));
            superCallDescriptor = superDescriptorVar.sym;
        }

        // SuperType.$populateSuper(desc, set);
        var superPopulateCall = staticMethodInvocation(populateSuperMethodSymbol(sym));
        superPopulateCall.args = List.of(make.Ident(superCallDescriptor), make.Ident(set));
        statements.add(make.Exec(superPopulateCall));
    }

    private Symbol.MethodSymbol populateSuperMethodSymbol(Symbol owner) {
        return new Symbol.MethodSymbol(
                PUBLIC | STATIC | optionalSynthetic(),
                constantsHolder.populateSuperMethodName,
                new Type.MethodType(
                        List.of(syms.typeDescriptorAccessorType, syms.specializedTypeHashSetType),
                        syms.voidType,
                        List.nil(),
                        syms.methodClass
                ),
                owner
        );
    }

    private JCTree.JCMethodDecl fieldAccessorInstanceMethod(Symbol.MethodSymbol method) {
        Assert.checkNonNull(classContext.descriptorField);
        var ret = make.Return(make.Ident(classContext.descriptorField));
        return make.MethodDef(
                method,
                make.Block(0L, List.of(ret))
        );
    }

    private Symbol.MethodSymbol fieldAccessorInstanceMethodSymbol(Symbol.ClassSymbol owner) {
        return new Symbol.MethodSymbol(
                PUBLIC | optionalSynthetic(),
                names.fromString("$descriptor"),
                new Type.MethodType(
                        List.nil(),
                        syms.derivedClassDescriptorType,
                        List.nil(),
                        syms.methodClass
                ),
                owner
        );
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

        var inlineAndBlocksBuffer = new ListBuffer<JCTree.JCStatement>();
        tree.defs.forEach(member -> {
            switch (member.getTag()) {
                case VARDEF -> {
                    var field = (JCTree.JCVariableDecl) member;
                    if (field.init != null) { // we gather all fields with inits
                        if (field.sym.isStatic()) {
                            staticBlock.add(make.Assignment(field.sym, field.init));
                        } else {
                            inlineAndBlocksBuffer.add(make.Assignment(field.sym, field.init));
                        }
                        field.init = null;
                    }
                    buffer.add(field);
                }
                case BLOCK -> {
                    var block = (JCTree.JCBlock) member;
                    if ((block.flags & STATIC) != 0) { // static blocks are ok
                        staticBlock.addAll(block.stats);
                    } else { // we totally remove the instance blocks
                        inlineAndBlocksBuffer.addAll(block.stats);
                    }
                }
                default -> buffer.add(member);
            }
        });
        if (staticBlock.nonEmpty()) {
            buffer.add(make.Block(STATIC, staticBlock.toList()));
        }

        classContext.inlineAndBlockDeclarations = inlineAndBlocksBuffer.toList();
        return buffer.toList();
    }
    //endregion

    //region rewriting (method)
    private void rewriteBasicMethod(JCTree.JCMethodDecl method) {
//        if (hasPrototypeInternal(method.sym)) {
//            method.sym.flags_field |= SYNTHETIC;
//        }
        if (method.body == null) return;

        try (var _ = typeParameterScopes.pushMethodScope(method.sym)) {
            instructionVisitor.visitRegularMethod(method);
            adjustRegularMethodBody(method);
        }
    }

    private void rewriteConstructor(JCTree.JCMethodDecl method) {
        try (var _ = typeParameterScopes.pushMethodScope(method.sym)) {
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

        typeParameterScopes.generateVariables(newInstructions::add);

        if (argsVariable != null && classContext.isHighestClassInGenericHierarchy) {
            var fieldInit = make.Exec(
                    make.Assign(
                            make.Ident(classContext.descriptorField),
                            make.TypeCast(syms.classDescriptorType, make.Ident(argsVariable))
                    )
            );
            newInstructions.add(fieldInit);
        }

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private void adjustRegularMethodBody(JCTree.JCMethodDecl method) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        // insert a pop in all non-private non-generic instance methods
        if (!method.sym.isStatic() && !method.sym.isPrivate() && method.type.getTypeArguments().isEmpty()) {
            var pop = constantsHolder.methodTypeArguments.call();
            newInstructions.add(make.Exec(pop));
        }

        typeParameterScopes.generateVariables(newInstructions::add);

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
        binary.operator = constantsHolder.objectEqOperator;
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
            var isConstructorFromParameterizedClass = sym.isConstructor() && isParameterized(sym.owner);
            if (!isParameterizedMethod && !isConstructorFromParameterizedClass) return;

            List<JCTree.JCStatement> pushStatements = List.nil();

            // should be used instead of 'tree' when it is used as a whole (e.g., foo(tree) -> foo(actualTree))
            // 'tree' fields can still be used to access information
            JCTree.LetExpr letExpr = null;

            Symbol.VarSymbol enclVariable = null;

            if (isConstructorFromParameterizedClass) {
                if (tree.encl != null) {
                    enclVariable = createVariable(tree.encl.type, extraVariablesOwner);
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
            var methodDef = make.VarDef(
                    createVariable(syms.methodDescriptorType, extraVariablesOwner),
                    constantsHolder.methodTypeArguments.call()
            );
            var constructorDef = make.VarDef(
                    createVariable(syms.classDescriptorType, extraVariablesOwner),
                    constantsHolder.constructorTypeArguments.call()
            );

            var tryBlock = make.Block(0L, tree.stats);

            var methodPush = constantsHolder.pushMethod.call(make.Ident(methodDef));
            var constructorPush = constantsHolder.pushConstructor.call(make.Ident(constructorDef));

            var finallyBlock = make.Block(0L, List.of(make.Exec(methodPush), make.Exec(constructorPush)));
            var tryFinally = make.Try(tryBlock, List.nil(), finallyBlock);

            tree.stats = List.of(methodDef, constructorDef, tryFinally);
        }

        @Override
        public void visitLambda(JCTree.JCLambda tree) {
            var type = tree.target;
            List<Type> superTypes;
            switch (type.getKind()) {
                case DECLARED -> {
                    if (!hasGenericTypeInHierarchy(List.of(type))) {
                        super.visitLambda(tree);
                        return;
                    }
                    superTypes = List.of(type);
                }
                case INTERSECTION -> {
                    var intersection = (Type.IntersectionClassType) type;
                    if (!hasGenericTypeInHierarchy(intersection.getComponents())) {
                        super.visitLambda(tree);
                        return;
                    }
                    // the tail here is to remove Object which is the first arg
                    superTypes = intersection.getComponents().tail;
                }
                default -> throw new AssertionError("Unexpected kind: " + type);
            }

            // we must create the list before introducing the lambda scope
            var arguments = superTypes.map(argLiteralGenerator::generateArgs);

            // the cast is safe as we moved all blocks to constructor => we care in a method
//            try (var _ = typeParameterScopes.pushLambdaScope((Symbol.MethodSymbol) tree.owner, superTypes)) {
            super.visitLambda(tree);

            var body = new ListBuffer<JCTree.JCStatement>();

            // we insert the methodTypeArguments pop
            body.add(make.Exec(constantsHolder.methodTypeArguments.call()));

            switch (tree.getBodyKind()) {
                case EXPRESSION -> body.add(make.Exec((JCTree.JCExpression) tree.body));
                case STATEMENT -> body.appendList(((JCTree.JCBlock) tree.body).stats);
            }

            tree.body = make.Block(0L, body.toList());

            JCTree.JCExpression pushed;
            if (constantList(arguments)) {
                pushed = constantsHolder.constantHiddenClassDescriptorBsm.call(arguments.map(i -> {
                    var ident = (JCTree.JCIdent) i;
                    return (Symbol.DynamicVarSymbol) ident.sym;
                }));
                tree.specialisationKind = JCTree.JCFunctionalExpression.SpecialisationKind.CONSTANT;
            } else {
                pushed = constantsHolder.hiddenClassDescriptorOf.call(arguments);
                tree.specialisationKind = JCTree.JCFunctionalExpression.SpecialisationKind.DYNAMIC;
            }

            var push = constantsHolder.pushHiddenClass.call(pushed);

            result = make.LetExpr(
                    List.of(make.Exec(push)),
                    tree
            ).setType(tree.type);
//            }
        }

        @Override
        public void visitReference(JCTree.JCMemberReference tree) {
            super.visitReference(tree);
//            System.out.println(tree.type);
        }

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
                    isRaw
            );
            JCTree.JCExpression pushedExpression = generatedArgs.map(l -> {
                        if (!constantList(l)) {
                            return constantsHolder.methodDescriptorOf.call(l);
                        }
                        var args = l.map(e -> (Symbol.DynamicVarSymbol) ((JCTree.JCIdent) e).sym);
                        return constantsHolder.constantMethodDescriptorBsm.call(args);
                    })
                    .orElseGet(TransParameterizedTypes.this::nullLiteral);
            return constantsHolder.pushMethod.call(pushedExpression);
        }

        private JCTree.JCExpression pushConstructorCode(List<Type> types, Symbol.MethodSymbol sym, Symbol.VarSymbol enclVariable) {
            var enclosingType = sym.owner.type.getEnclosingType();
            JCTree.JCExpression enclosingDescriptor = nullLiteral();
//            if (enclosingType != null && !Type.noType.equals(enclosingType) && enclosingType.tsym.hasNewGenerics()) {
//                // if the enclosing is parameterized, it will have a field that we can extract
//                if (enclosingType.isParameterized()) {
//                    var descriptorOwner = enclVariable != null
//                            ? make.Ident(enclVariable)
//                            : make.This(classContext.classSymbol.type);
//                    enclosingDescriptor = constantsHolder.classDescriptor$From.call(
//                            descriptorOwner,
//                            classLiteral(enclosingType)
//                    );
//                    // otherwise, it is a regular class, we can just create a class descriptor from it
//                } else {
//                    var type = enclVariable != null ? enclVariable.type : classContext.classSymbol.type;
//                    enclosingDescriptor = argLiteralGenerator.generateArgs(type);
//                }
//            } else {
//                enclosingDescriptor = nullLiteral();
//            }

            var fullArguments = new ListBuffer<JCTree.JCExpression>();
            types.forEach(t -> fullArguments.add(argLiteralGenerator.generateArgs(t)));
            var captureStart = fullArguments.size();

            var outerParams = allParams(sym.owner.getEnclosingElement());
            if (outerParams.nonEmpty()) {
                outerParams.forEach(p -> fullArguments.add(argLiteralGenerator.generateArgs(p.type)));
            }

            var args = fullArguments.toList();

            JCTree.JCExpression pushedValue;
            if (captureStart == fullArguments.size() && constantList(args)) {
                pushedValue = argLiteralGenerator.constantClassDescriptor(null, (Type.ClassType) sym.owner.type, args);
            } else {
                pushedValue = classDescriptorConstructorInvocation(enclosingDescriptor, sym.owner.type, args, captureStart);
            }

            return constantsHolder.pushConstructor.call(pushedValue);
        }

        private JCTree.JCExpression pushSuperOrThisCode(JCTree.JCMethodInvocation tree) {
            var methodName = Objects.requireNonNull(TreeInfo.name(tree.meth));

            // easiest case, this, we just repush the args without any modification
            if (methodName == methodName.table.names._this) {
                if (constructorArgsVariable == null) {
                    throw new AssertionError("No constructor args variable in context");
                }
                return constantsHolder.pushConstructor.call(make.Ident(constructorArgsVariable));
            }

            // for super there are two cases:
            // 1. we are in a parameterized class -> just push the constructor argument
            // 2. we are in a regular class -> we need to compute the super

            if (constructorArgsVariable != null) {
                return constantsHolder.pushConstructor.call(make.Ident(constructorArgsVariable));
            }

            var descriptor = argLiteralGenerator.generateArgs(classContext.classSymbol.getSuperclass());
            return constantsHolder.pushConstructor.call(descriptor);
        }


        private Optional<List<JCTree.JCExpression>> basicMethodArgConstruction(
                Symbol.MethodSymbol sym,
                Type.MethodType methodType,
                List<JCTree.JCExpression> explicitTypes,
                boolean isRaw
        ) {
            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
            // use the inferred types `String s = foo();`
            if (!explicitTypes.isEmpty()) { // provided type arguments
                var list = explicitTypes.map(t -> argLiteralGenerator.generateArgs(t.type));
                return Optional.of(list);
            }

            var inferredTypes = methodType.inferenceMapping;
            if (isRaw || inferredTypes == null || inferredTypes.isEmpty()) {
                return Optional.empty();
            }

            var list = sym.type
                    .getTypeArguments()
                    .map(t -> argLiteralGenerator.generateArgs(computeTypeFromInference(inferredTypes, t)));
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
                    newArguments.add(next);
                    continue;
                }

                var tempVariable = createVariable(next.type, extraVariablesOwner);
                var statements = letStatements.prepend(make.VarDef(tempVariable, next));
                var let = make.LetExpr(statements, make.Ident(tempVariable)).setType(tempVariable.type.baseType());
                newArguments.add(let);
            }

            return newArguments.toList();
        }

        private void handleCompilerIntrinsic(JCTree.JCMethodInvocation tree, Symbol.MethodSymbol sym) {
            if (!syms.specializedTypeDescriptorType.equals(sym.owner.type)) return;

            switch (sym.name.toString()) {
                case "of" -> {
                    var methodType = tree.meth.type.asMethodType();

                    JCTree.JCExpression arg;
                    if (tree.typeargs.isEmpty()) {
                        arg = nullLiteral();
                    } else {
                        var generatedArgs = basicMethodArgConstruction(
                                sym,
                                methodType,
                                tree.typeargs,
                                false
                        );
                        arg = generatedArgs.orElseThrow(AssertionError::new).getFirst();
                    }

                    result = constantsHolder.filterPartialDescriptor.call(arg);
                }
            }
        }

        private JCTree.JCMethodDecl generateLambdaComputeSuper(
                JCTree.JCFunctionalExpression lambda,
                List<Type> lambdaSuperTypes,
                List<Symbol.TypeSymbol> typeArguments
        ) {
            var method = lambdaComputeSuperMethodSymbol(lambda);
            return computePopulateSuperMethod(
                    method,
                    lambdaSuperTypes,
                    typeArguments,
                    true
            );
        }

        private JCTree.JCExpression lambdaDescriptorConstructorInvocation(
                Symbol lambdaOwner,
                List<JCTree.JCExpression> typeArguments,
                Symbol.MethodSymbol handle
        ) {
            var computeSuper = computeSuperLambdaCall(lambdaOwner, handle);
            var args = typeArguments // captures
                    .prepend(make.Literal(0)) // captureStart (0 for lambda)
                    .prepend(nullLiteral()); // type (null as the class is hidden)
//                    .prepend(nullLiteral()) // outer (null, lambdas do not have outer class)
//                    .prepend(computeSuper); // compute super method

            return constantsHolder.classDescriptorOf.call(args);
        }

        private Symbol.MethodSymbol lambdaComputeSuperMethodSymbol(JCTree.JCFunctionalExpression lambda) {
            return new Symbol.MethodSymbol(
                    PRIVATE | STATIC | SYNTHETIC,
                    lambdaComputeSuperName(lambda.owner),
                    new Type.MethodType(
                            List.of(syms.typeDescriptorAccessorType, syms.specializedTypeHashSetType),
                            syms.voidType,
                            List.nil(),
                            syms.methodClass
                    ),
                    classContext.classSymbol
            );
        }

        private Name lambdaComputeSuperName(Symbol owner) {
            String buf = names.lambda +
                    "computeSuper$" +
                    syntheticMethodNameComponent(owner) +
                    "$" +
                    nextVariableId();
            return names.fromString(buf);
        }

        String syntheticMethodNameComponent(Symbol owner) {
            long ownerFlags = owner.flags();
            if ((ownerFlags & BLOCK) != 0) {
                return (ownerFlags & STATIC) != 0 ?
                        "static" : "new";
            } else if (owner.isConstructor()) {
                return "new";
            } else {
                return owner.name.toString();
            }
        }

    }

    private final class ArgLiteralGenerator {

        public JCTree.JCExpression generateArgs(Type type) {
            return actualGenerateArgs(type);
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
            var component = actualGenerateArgs(type.elemtype);
            if (component.hasTag(JCTree.Tag.IDENT)) {
                var ident = (JCTree.JCIdent) component;
                var componentCondy = (Symbol.DynamicVarSymbol) ident.sym;
                return constantsHolder.constantArrayDescriptorBsm.call(componentCondy);
            }
            return constantsHolder.arrayTypeOf.call(component);
        }

        private JCTree.JCExpression generateWildcardKind(Type.WildcardType type) {
            return constantsHolder.erasedTypeDescriptorBsm.call();
        }

        private JCTree.JCExpression generateIntersectionKind(Type.IntersectionClassType type) {
            return constantsHolder.erasedTypeDescriptorBsm.call();
        }

        private JCTree.JCExpression generateClassKind(Type.ClassType type) {
            var outerType = generateOuterClass(type);
            var constantOuter = outerType == null || outerType.hasTag(JCTree.Tag.IDENT);

            var fullArguments = new ListBuffer<JCTree.JCExpression>();
            type.getTypeArguments().forEach(t -> fullArguments.add(argLiteralGenerator.generateArgs(t)));
            var captureStart = fullArguments.size();
            addOuterTypes(fullArguments, type);

            var args = fullArguments.toList();

            var constantArguments = constantList(args);

            if (captureStart == fullArguments.size() && constantArguments) {
                return constantClassDescriptor(outerType, type, args);
            } else {
                outerType = outerType == null ? nullLiteral() : outerType;
            }

            return classDescriptorConstructorInvocation(outerType, type, args, captureStart);
        }

        private JCTree.JCExpression generateTypeVarKind(Type.TypeVar type) {
            // TODO investigate this
            var owner = type.tsym.owner;
            var index = owner.type.getTypeArguments().indexOf(type);
            if (index == -1) {
                index = owner.type.allparams().indexOf(type);
            }

            // if the owner of this type does not have it in its declared type parameters, it is a wildcard
            if (index == -1) { // wildcard
                return constantsHolder.erasedClassDescriptorInstance.call();
            }

            return typeVarResolution(type.tsym);
        }

        private JCTree.JCExpression generatePrimitiveType(Type.JCPrimitiveType type) {
            return constantsHolder.constantClassDescriptorBsm.call(primitiveDescriptor(type));
        }

        private JCTree.JCExpression generateOuterClass(Type current) {
            var enclosingType = current.getEnclosingType();
            // if there is no enclosing type, we have nothing to do
            if (enclosingType == null || Type.noType.equals(enclosingType)) return null;

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

        private String primitiveDescriptor(Type.JCPrimitiveType type) {
            return switch (type.getTag()) {
                case BOOLEAN -> "Z";
                case BYTE -> "B";
                case SHORT -> "S";
                case INT -> "I";
                case LONG -> "J";
                case CHAR -> "C";
                case FLOAT -> "F";
                case DOUBLE -> "D";
                default -> throw new AssertionError(type);
            };
        }

        private JCTree.JCExpression constantClassDescriptor(JCTree.JCExpression outer, Type.ClassType type, List<JCTree.JCExpression> arguments) {
            Symbol.DynamicVarSymbol outerCondy = null;
            if (outer != null) {
                var ident = (JCTree.JCIdent) outer;
                outerCondy = (Symbol.DynamicVarSymbol) ident.sym;
            }

            var condyArgs = new ListBuffer<>();
            condyArgs.add(typeToDescriptor(type));

            var flags = 0;
            if (type.isRaw()) {
                flags |= CONSTANT_DESCRIPTOR_BSM_FLAG_RAW;
            }

            condyArgs.add(flags);
//            if (outerCondy != null) {
//                condyArgs.add(outerCondy);
//            }

            for (var argument : arguments) {
                var ident = (JCTree.JCIdent) argument;
                var argCondy = (Symbol.DynamicVarSymbol) ident.sym;
                condyArgs.add(argCondy);
            }

            return constantsHolder.constantClassDescriptorBsm.call(condyArgs.toList());
        }

        private void addOuterTypes(ListBuffer<JCTree.JCExpression> buffer, Type.ClassType type) {
            Type currentType = type;
            Symbol currentSym = type.tsym.getEnclosingElement();
            while (true) {
                var toGenerate = switch (currentSym) {
                    case Symbol.MethodSymbol methodSymbol -> methodSymbol.type;
                    case Symbol.ClassSymbol _ -> currentType = currentType.getEnclosingType();
                    default -> null;
                };
                if (toGenerate == null) return;
                toGenerate.getTypeArguments().forEach(t -> buffer.add(generateArgs(t)));
                currentSym = currentSym.getEnclosingElement();
            }
        }

    }

    private final class ParameterizedScope {
        // Invariants:
        // - Each pushClass also pushes a mapping. If the class actually have no mapping, it pushes an empty map
        // - Each pushMethod also pushes a new state.
        private List<Scope> scopes = List.nil();
        private List<GroupState> states = List.nil();
        private Symbol.VarSymbol constructorPassedDescriptor;

        public GroupRemover pushClassScope(Symbol.VarSymbol descriptorField) {
            var typeParameters = allParams(classContext.classSymbol);
            var addedCount = 0;

            if (typeParameters.nonEmpty()) {
                Scope scope = classContext.classSymbol.isInterface()
                        ? new InterfaceScope(typeParameters)
                        : new ClassScope(typeParameters, descriptorField);

                scopes = scopes.prepend(scope);
                addedCount++;
            }
            return new GroupRemover(addedCount, this, GroupRemover.Action.NO_OP);
        }

        public GroupRemover pushMethodScope(Symbol.MethodSymbol method) {
            var id = new GroupStateId();
            var addedCount = 0;

            if (method.isConstructor() && classContext.isInGenericHierarchy) {
                // this is to handle regular classes that derives generic types, as the class won't have type params
                var variableParams = isParameterized(method.owner)
                        ? scopes.head.variableParams()
                        : List.<Symbol.TypeSymbol>nil();

                var constructorGroup = new ConstructorScope(variableParams, id, method);
                scopes = scopes.prepend(constructorGroup);
                constructorPassedDescriptor = constructorGroup.variable(method);
                addedCount++;
            }

            if (method.type.getTypeArguments().nonEmpty()) {
                scopes = scopes.prepend(new MethodScope(getTypeArguments(method), id, method));
                addedCount++;
            }

            states = states.prepend(new GroupState(id, scopes, method));
            return new GroupRemover(addedCount, this, GroupRemover.Action.POP_STATE);
        }

        public GroupRemover pushLambdaScope(
                Symbol.MethodSymbol method,
                List<Symbol.TypeSymbol> types
        ) {
            var id = new GroupStateId();

            var scope = new LambdaBodyScope(types, id, method);
            scopes = scopes.prepend(scope);
            states = states.prepend(new GroupState(id, scopes, method));
            return new GroupRemover(1, this, GroupRemover.Action.POP_STATE);
        }

        public GroupRemover pushComputeSuperGroup(Symbol.VarSymbol variable, Symbol.MethodSymbol method, List<Symbol.TypeSymbol> typeArguments) {
            var id = new GroupStateId();

            scopes = scopes.prepend(new ComputeSuperScope(typeArguments, id, variable));
            states = states.prepend(new GroupState(id, scopes, method));
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
            for (var group : scopes) {
                var index = group.index(typeVar);
                if (index == -1) {
                    groupIndex++;
                    continue;
                }
                var variable = states.head.variable(groupIndex);
                return group.access(variable, index);
            }
            return null;
        }

        private static final class GroupStateId {
        }

        public static final class GroupState {
            private final List<Slot> groups;
            private final GroupStateId id;

            private GroupState(GroupStateId id, List<Scope> scopes, Symbol variableOwner) {
                this.id = id;
                this.groups = scopes.map(g -> new Slot(g, g.variable(variableOwner)));
            }

            public Symbol.VarSymbol variable(int index) {
                Objects.checkIndex(index, groups.size());

                // we also mark the group as used
                var slot = groups.get(index);
                slot.used = slot.scope.markAsUsed();

                return slot.variable;
            }

            public void generateVariables(Consumer<JCTree.JCStatement> statementConsumer) {
                groups.forEach(slot -> {
                    if (!slot.scope.shouldGenerate(slot.used, id)) {
                        return;
                    }
                    slot.scope.declaration(slot.variable, statementConsumer);
                });
            }

            private static final class Slot {
                private final Scope scope;
                private final Symbol.VarSymbol variable;
                private boolean used;

                private Slot(Scope scope, Symbol.VarSymbol variable) {
                    this.scope = scope;
                    this.variable = variable;
                }

            }
        }

        sealed interface Scope {

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

            List<Symbol.TypeSymbol> variableParams();

            int index(Symbol.TypeSymbol typeVar);

            JCTree.JCExpression access(Symbol.VarSymbol variable, int index);

        }

        private abstract sealed class Base implements Scope {
            private final List<Symbol.TypeSymbol> variableParams;
            protected final GroupStateId owner;
            private boolean used;

            protected Base(List<Symbol.TypeSymbol> variableParams, GroupStateId owner) {
                this.variableParams = variableParams;
                this.owner = owner;
            }

            @Override
            public final int index(Symbol.TypeSymbol typeVar) {
                return variableParams.indexOf(typeVar);
            }

            @Override
            public final JCTree.JCExpression access(Symbol.VarSymbol variable, int index) {
                return constantsHolder.typeArgument.call(
                        make.Ident(variable),
                        make.Literal(index)
                );
            }

            @Override
            public final List<Symbol.TypeSymbol> variableParams() {
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
            public final String toString() {
                return getClass().getSimpleName() + ": [" + variableParams.toString() + "]";
            }

            protected abstract boolean shouldSaveInLocal();

        }

        private final class MethodScope extends Base {
            private final Symbol.VarSymbol variable;

            private MethodScope(
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.MethodSymbol method
            ) {
                super(variableParams, owner);
                this.variable = createVariable(syms.methodDescriptorType, method);
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
                    var call = constantsHolder.methodTypeArguments.call();
                    statementConsumer.accept(make.Exec(call));
                    return;
                }

                // MethodTypeArgs methodTypeArgs = null;
                statementConsumer.accept(
                        make.VarDef(
                                declarationVariable,
                                constantsHolder.methodTypeArguments.call()
                        )
                );

                // if (methodTypeArgs == null) methodTypeArgs = *raw type arguments;
                var mArgsFallback = constantsHolder.methodDescriptorOf.call();
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

        private final class ClassScope extends Base {
            private final Symbol.VarSymbol variable;

            private ClassScope(List<Symbol.TypeSymbol> variableParams, Symbol.VarSymbol descriptorField) {
                super(variableParams, null);
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

        private final class InterfaceScope extends Base {
            // Method group only has one args variable declared.
            private final Symbol.ClassSymbol type;

            private InterfaceScope(List<Symbol.TypeSymbol> variableParams) {
                super(variableParams, null);
                this.type = classContext.classSymbol;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return true;
            }

            @Override
            public void declaration(Symbol.VarSymbol declarationVariable, Consumer<JCTree.JCStatement> statementConsumer) {
                // SpecializedType args = ...;
                var init = constantsHolder.classDescriptor$From.call(
                        make.This(type.type),
                        classLiteral(classContext.classSymbol.type)
                );
                var decl = make.VarDef(declarationVariable, init);
                statementConsumer.accept(decl);

                // if (args == null) raw version
                var fallback = fallbackIf(
                        declarationVariable,
                        argLiteralGenerator.generateArgs(types.erasure(classContext.classSymbol.type))
                );
                statementConsumer.accept(fallback);
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return createVariable(syms.classDescriptorType, currentOwner);
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                // FIXME, maybe we need to change the whole system to avoid generating too much interface access.
                return usedInState;
            }

        }

        private final class ConstructorScope extends Base {
            private final Symbol.VarSymbol variable;

            private ConstructorScope(
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.MethodSymbol constructor
            ) {
                super(variableParams, owner);
                this.variable = createVariable(syms.classDescriptorType, constructor);
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                // SpecializedType constructorArguments = MethodArgStack.constructorTypeArguments();
                var call = constantsHolder.constructorTypeArguments.call();
                statementConsumer.accept(make.VarDef(declarationVariable, call));

                // if (args == null) args = *create raw type*;
                statementConsumer.accept(
                        fallbackIf(
                                declarationVariable,
                                argLiteralGenerator.generateArgs(types.erasure(classContext.classSymbol.type))
                        )
                );
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                // We always need to generate it in its constructor as it will at least be used to set up the
                // constructorTypeArgs.
                return currentState == owner;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }
        }

        private final class ComputeSuperScope extends Base {

            private final Symbol.VarSymbol variable;

            private ComputeSuperScope(
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.VarSymbol variable
            ) {
                super(variableParams, owner);
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

        private final class LambdaBodyScope extends Base {

            private final Symbol.VarSymbol variable;

            private LambdaBodyScope(
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.MethodSymbol method
            ) {
                super(variableParams, owner);
                this.variable = createVariable(syms.lambdaDescriptorType, method);
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return false;
            }

            @Override
            public void declaration(Symbol.VarSymbol declarationVariable, Consumer<JCTree.JCStatement> statementConsumer) {
                if (!used()) { // if not used, we at least avoid generating a local variable
                    var call = constantsHolder.lambdaTypeArguments.call();
                    statementConsumer.accept(make.Exec(call));
                    return;
                }

                // LambdaDescriptor lambdaTypeArgs = lambdaTypeArguments();
                var init = constantsHolder.lambdaTypeArguments.call();
                statementConsumer.accept(make.VarDef(declarationVariable, init));
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                return currentState == owner; // we always generate the code, as we at least need to pop the information
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
                    scope.scopes = scope.scopes.tail;
                }
                switch (action) {
                    case POP_STATE -> {
                        scope.states = scope.states.tail;
                        scope.constructorPassedDescriptor = null;
                    }
                    case NO_OP -> {
                    }
                }
            }

            enum Action {
                NO_OP,
                POP_STATE,
                ;
            }

        }

    }

    //region method invocations
    private final class StaticMethod {
        private final Name name;
        private final Type ownerType;

        private StaticMethod(Name name, Type owner) {
            this.name = name;
            this.ownerType = owner;
        }

        public JCTree.JCMethodInvocation call(List<JCTree.JCExpression> arguments) {
            return externalMethodInvocation(
                    name,
                    ownerType,
                    arguments,
                    m -> make.Select(make.Ident(ownerType.tsym), m)
            );
        }

        public JCTree.JCMethodInvocation call(JCTree.JCExpression... arguments) {
            return call(List.from(arguments));
        }

    }

    private final class InstanceMethod {
        private final Name name;
        private final Type ownerType;

        private InstanceMethod(Name name, Type ownerType) {
            this.name = name;
            this.ownerType = ownerType;
        }

        public JCTree.JCMethodInvocation call(JCTree.JCExpression receiver, List<JCTree.JCExpression> arguments) {
            Assert.check(
                    types.isSubtype(receiver.type, ownerType),
                    "Receiver " + receiver + " (" + receiver.type + ") is not subtype of type " + ownerType + "."
            );
            return externalMethodInvocation(
                    name,
                    ownerType,
                    arguments,
                    m -> make.Select(receiver, m)
            );
        }

        public JCTree.JCMethodInvocation call(JCTree.JCExpression receiver, JCTree.JCExpression... arguments) {
            return call(receiver, List.from(arguments));
        }

        public JCTree.JCMethodInvocation call(Symbol.VarSymbol receiver, JCTree.JCExpression... arguments) {
            return call(make.Ident(receiver), List.from(arguments));
        }

        public Symbol.MethodSymbol symbol(List<Type> types) {
            return resolve.resolveInternalMethod(classContext.classTree, env, ownerType, name, types, null);
        }

        public Symbol.MethodSymbol symbol(Type... types) {
            return symbol(List.from(types));
        }

    }

    private final class ConstructorMethod {
        private final Type ownerType;

        private ConstructorMethod(Type ownerType) {
            this.ownerType = ownerType;
        }

        public JCTree.JCNewClass call(List<JCTree.JCExpression> arguments) {
            var constructor = resolve.resolveInternalConstructor(
                    classContext.classTree,
                    env,
                    ownerType,
                    arguments.map(e -> e.type),
                    null
            );
            var res = make.NewClass(null, List.nil(), make.Ident(constructor.type.tsym), List.nil(), null);
            res.setType(ownerType);
            res.constructor = constructor;
            if (constructor.isVarArgs()) {
                res.varargsElement = ((Type.ArrayType) constructor.type.asMethodType().argtypes.last()).elemtype;
            }
            return res;
        }

        public JCTree.JCNewClass call(JCTree.JCExpression... arguments) {
            return call(List.from(arguments));
        }

    }

    private final class BootstrapMethod {
        private final Name name;
        private final Type ownerType;
        private final Type expressionType;
        private final Kind kind;

        enum Kind {
            INDY,
            CONDY,
            ;
        }

        private BootstrapMethod(Name name, Type owner, Type expressionType, Kind kind) {
            this.name = name;
            this.ownerType = owner;
            this.expressionType = expressionType;
            this.kind = kind;
        }

        public JCTree.JCExpression call(List<?> arguments) {
            var method = resolve.resolveInternalMethod(
                    classContext.classTree,
                    env,
                    ownerType,
                    name,
                    arguments.map(this::objectToType)
                            .prepend(
                                    types.subst(
                                            syms.classType,
                                            List.of(syms.classType.getTypeArguments().getFirst()),
                                            List.of(expressionType)
                                    )
                            )
                            .prepend(syms.stringType)
                            .prepend(syms.methodHandleLookupType),
                    List.nil()
            );
            var constants = arguments.map(BootstrapMethod::objectToConstant)
                    .toArray(PoolConstant.LoadableConstant[]::new);

            return switch (kind) {
                case INDY -> throw new AssertionError();
                case CONDY -> {
                    var condy = new Symbol.DynamicVarSymbol(
                            name,
                            syms.noSymbol,
                            method.asHandle(),
                            expressionType,
                            constants
                    );
                    yield make.Ident(condy);
                }
            };
        }

        public JCTree.JCExpression call(Object... argumentsType) {
            return call(List.from(argumentsType));
        }

        private Type objectToType(Object o) {
            return switch (o) {
                case Integer _ -> syms.intType;
                case Long _ -> syms.longType;
                case Float _ -> syms.floatType;
                case Double _ -> syms.doubleType;
                case String _ -> syms.stringType;
                case Type.ClassType _ -> syms.classType;
                case PoolConstant.LoadableConstant _ -> syms.objectType;
                default -> throw new AssertionError("Unexpected type for " + o + " (" + o.getClass() + ").");
            };
        }

        private static PoolConstant.LoadableConstant objectToConstant(Object o) {
            return switch (o) {
                case Integer i -> PoolConstant.LoadableConstant.Int(i);
                case Long l -> PoolConstant.LoadableConstant.Long(l);
                case Float f -> PoolConstant.LoadableConstant.Float(f);
                case Double d -> PoolConstant.LoadableConstant.Double(d);
                case String s -> PoolConstant.LoadableConstant.String(s);
                case PoolConstant.LoadableConstant c -> c;
                default -> throw new AssertionError("Unexpected type for " + o);
            };
        }


    }

    public JCTree.JCMethodInvocation externalMethodInvocation(
            Name name,
            Type site,
            List<JCTree.JCExpression> arguments,
            Function<Symbol.MethodSymbol, JCTree.JCExpression> fnMapper
    ) {
        Symbol.MethodSymbol method = null;
        try {
            method = resolve.resolveInternalMethod(
                    classContext.classTree,
                    env,
                    site,
                    name,
                    arguments.map(e -> e.type),
                    null
            );
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
        var call = make.Apply(
                List.nil(),
                fnMapper.apply(method),
                arguments
        );
        call.setType(method.type.asMethodType().getReturnType());
        if (method.isVarArgs()) {
            call.varargsElement = ((Type.ArrayType) method.type.asMethodType().argtypes.last()).elemtype;
        }
        return call;
    }


    private JCTree.JCExpression classDescriptorConstructorInvocation(
            JCTree.JCExpression outer,
            Type type,
            List<JCTree.JCExpression> typeArguments,
            int captureStart
    ) {
        if (type.isRaw()) {
            return constantsHolder.classDescriptorOfRaw.call(classLiteral(type));
        }
        var arguments = typeArguments
                .prepend(make.Literal(captureStart)) // FIXME we need to compute the capture index
                .prepend(classLiteral(type));
//                .prepend(outer)
//                .prepend(computeSuperLambdaCall(currentMethod, classContext.computeSuperMethod));
        return constantsHolder.classDescriptorOf.call(arguments);
    }

    private JCTree.JCMethodInvocation staticMethodInvocation(Symbol.MethodSymbol method) {
        return externalMethodInvocation(method, method.type.asMethodType().getReturnType(), make.Ident(method));
    }

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

    private JCTree.JCLambda computeSuperLambdaCall(Symbol owner, Symbol.MethodSymbol computeSuperMethod) {
        var lambdaVariables = List.of(
                make.VarDef(createVariable(syms.typeDescriptorAccessorType, owner), null),
                make.VarDef(createVariable(syms.specializedTypeHashSetType, owner), null)
        );

        var handleCall = staticMethodInvocation(computeSuperMethod);
        handleCall.args = lambdaVariables.map(v -> make.Ident(v));

        var callback = make.Lambda(lambdaVariables, handleCall).setType(syms.lambdaDescriptorComputeSuperType);
        callback.owner = owner;
        callback.target = syms.lambdaDescriptorComputeSuperType;

        return callback;
    }
    //endregion

    //region utils
    public static boolean hasPopulateSuperMethod(Symbol.ClassSymbol sym) {
        if (!sym.hasNewGenerics()) return false;
        if (sym.type.isParameterized()) return true;
        if (
                sym.getSuperclass() != Type.noType
                        && ((Symbol.ClassSymbol) sym.getSuperclass().tsym).hasPopulateSuperMethod()
        ) return true;
        for (var interfac : sym.getInterfaces()) {
            var cl = (Symbol.ClassSymbol) interfac.tsym;
            if (cl.hasPopulateSuperMethod()) return true;
        }
        return false;
    }

    private static boolean isParameterized(Symbol symbol) {
        var current = symbol;

        while (true) {
            if (!(current instanceof Symbol.MethodSymbol || current instanceof Symbol.ClassSymbol)) {
                break;
            }
            if (current.type.getTypeArguments().nonEmpty()) {
                return true;
            }
            if (current.isStatic()) {
                break;
            }
            current = current.getEnclosingElement();
        }

        return false;
    }

    private static List<Symbol.TypeSymbol> allParams(Symbol symbol) {
        var current = symbol;
        var buffer = new ListBuffer<Symbol.TypeSymbol>();

        while (true) {
            if (!(current instanceof Symbol.MethodSymbol || current instanceof Symbol.ClassSymbol)) {
                break;
            }
            current.type.getTypeArguments().forEach(t -> buffer.add(t.tsym));
            if (current.isStatic()) {
                break;
            }
            current = current.getEnclosingElement();
        }

        return buffer.toList();
    }

    private String typeToDescriptor(Type type) {
        return "()" + lambdaToMethod.typeSig(types.erasure(type));
    }

    private JCTree.JCExpression nullLiteral() {
        return make.Literal(TypeTag.BOT, null).setType(syms.botType);
    }

    private Symbol.VarSymbol createVariable(Type type, Symbol owner) {
        return new Symbol.VarSymbol(
                0L,
                names.fromString(nextVariableId("x")),
                type,
                owner
        );
    }

    private String nextVariableId(String prefix) {
        var offset = nameOffsetIndex;
        nameOffsetIndex = (byte) (nameOffsetIndex < 99 ? nameOffsetIndex + 1 : 0);
        var actualPrefix = prefix != null ? prefix + "$" : "";
        return actualPrefix + String.format("%02d", offset);
    }

    private String nextVariableId() {
        return nextVariableId(null);
    }

    private static List<Symbol.TypeSymbol> getTypeArguments(Symbol sym) {
        var list = new ListBuffer<Symbol.TypeSymbol>();
        sym.type.getTypeArguments().forEach(t -> list.add(t.tsym));
        return list.toList();
    }

    private static Symbol.ClassSymbol highestConcreteGenericSuperType(Symbol.ClassSymbol sym) {
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

    private static List<Symbol.TypeSymbol> lambdaTypeParams(List<Type> types) {
        var buffer = new ListBuffer<Symbol.TypeSymbol>();
        types.forEach(type -> {
            if (!type.tsym.hasNewGenerics() || !type.isParameterized()) return;
            // here type is an interface, meaning that there is an implicit static and then
            // allparams == getTypeArguments
            type.getTypeArguments().forEach(typeArgument -> {
                if (!typeArgument.hasTag(TypeTag.TYPEVAR)) return;
                buffer.add(typeArgument.tsym);
            });
        });
        return buffer.toList();
    }

    private static boolean hasGenericTypeInHierarchy(List<Type> types) {
        for (var type : types) {
            if (type == Type.noType || type == null || !type.tsym.hasNewGenerics()) continue;
            if (type.isParameterized()) return true;
            var cl = (Symbol.ClassSymbol) type.tsym;
            if (hasGenericTypeInHierarchy(List.of(cl.getSuperclass()))) return true;
            if (hasGenericTypeInHierarchy(cl.getInterfaces())) return true;
        }
        return false;
    }

    /// We only check 1 level for the super class, but the whole hierarchy tree for interfaces
    private static boolean isGenericOrHasGenericInterface(Symbol.ClassSymbol sym) {
        if (isParameterized(sym)) return true;
        var interfaces = sym.getInterfaces();
        while (interfaces.nonEmpty()) {
            var next = interfaces.head;
            if (isParameterized(next.tsym)) return true;
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

    private boolean constantList(List<JCTree.JCExpression> expressions) {
        if (expressions.isEmpty()) return true;
        for (var expr : expressions) {
            if (!expr.hasTag(JCTree.Tag.IDENT)) return false;
        }
        return true;
    }

    private JCTree.JCExpression classLiteral(Type type) {
        return make.ClassLiteral(types.erasure(type)).setType(syms.classType);
    }

    private boolean hasPrototypeInternal(Symbol symbol) {
        var current = symbol;
        while (current != null) {
            if (current.attribute(syms.prototypeInternalAnnotationType.tsym) != null) {
                return true;
            }
            current = current.owner;
        }
        return false;
    }
    //endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree classDef, TreeMaker make) {
        Objects.requireNonNull(env);
        Objects.requireNonNull(classDef);
        Objects.requireNonNull(make);

        try {
            this.env = env;
            this.make = make;
            return translator.translate(classDef);
        } finally {
            this.env = null;
            this.make = null;
        }
    }

}
