package com.sun.tools.javac.comp;

import com.sun.source.tree.LambdaExpressionTree;
import com.sun.tools.javac.code.*;
import com.sun.tools.javac.jvm.ByteCodes;
import com.sun.tools.javac.jvm.PoolConstant;
import com.sun.tools.javac.tree.*;
import com.sun.tools.javac.util.*;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

import static com.sun.tools.javac.code.Flags.*;

public final class TransParameterizedTypes {

    //region fields
    private static final Context.Key<TransParameterizedTypes> KEY = new Context.Key<>();

    private static final int CONSTANT_DESCRIPTOR_BSM_FLAG_RAW = 1;

    private final boolean enableSynthetic;
    private final boolean enabled;

    private final InstructionVisitor instructionVisitor;
    private final TypeDescriptorFactory typeDescriptorFactory;
    private final TypeVariableScopes typeParameterScopes;
    private final Translator translator;
    private ConstantHolder constantsHolder;

    private final Log log;
    private final Symtab symbols;
    private final Names names;
    private final Types types;
    private final Operators operators;
    private final Resolve resolve;
    private final LambdaToMethod lambdaToMethod; // TODO avoid using LambdaToMethod to convert type do string desc
    private final TransTypes transTypes;
    private final Enter enter;

    private TreeMaker make;

    private ClassContext classContext;
    private CurrentClassNestingMetadata classNestingMetadata = CurrentClassNestingMetadata.NONE;

    /// Simple value appended to generated local variable names to easily identify them (e.g. x$34). We assume that
    /// there will never be more than 99.
    private int nameOffsetIndex;

    private record ClassContext(
            JCTree.JCClassDecl classTree,
            Symbol.ClassSymbol classSymbol,
            Symbol.VarSymbol descriptorField,
            boolean isHighestClassInGenericHierarchy,
            boolean isInGenericHierarchy,
            Env<AttrContext> env,
            int baseNameOffsetIndex,
            boolean currentClassParameterized
    ) {
    }

    private final class ConstantHolder {

        public final Name objectTypeArgumentsFieldName = names.fromString("$typeArguments");

        public final StaticMethod methodTypeArguments = new StaticMethod(
                names.fromString("methodTypeArguments"),
                symbols.typeDescriptorPassingHandlerType
        );

        public final StaticMethod constructorTypeArguments = new StaticMethod(
                names.fromString("constructorTypeArguments"),
                symbols.typeDescriptorPassingHandlerType
        );

        public final InstanceMethod typeArgument = new InstanceMethod(
                names.fromString("typeArgument"),
                symbols.typeDescriptorAccessorType
        );

        public final InstanceMethod argument = new InstanceMethod(
                names.fromString("argument"),
                symbols.classDescriptorType
        );

        public final StaticMethod arrayTypeOf = new StaticMethod(
                names.of,
                symbols.arrayDescriptorType
        );

        public final StaticMethod methodDescriptorOf = new StaticMethod(
                names.of,
                symbols.methodDescriptorType
        );

        public final StaticMethod classDescriptorOf = new StaticMethod(
                names.of,
                symbols.classDescriptorType
        );

        public final StaticMethod hiddenClassDescriptorOf = new StaticMethod(
                names.of,
                symbols.hiddenClassDescriptorType
        );

        public final StaticMethod pushMethod = new StaticMethod(
                names.fromString("pushMethod"),
                symbols.typeDescriptorPassingHandlerType
        );

        public final StaticMethod pushConstructor = new StaticMethod(
                names.fromString("pushConstructor"),
                symbols.typeDescriptorPassingHandlerType
        );

        public final StaticMethod pushHiddenClass = new StaticMethod(
                names.fromString("pushHiddenClass"),
                symbols.typeDescriptorPassingHandlerType
        );

        public final StaticMethod filterPartialDescriptor = new StaticMethod(
                names.fromString("filterPartialDescriptor"),
                symbols.specializedTypeDescriptorType
        );

        public final StaticMethod classDescriptor$From = new StaticMethod(
                names.fromString("$from"),
                symbols.specializedTypeDescriptorType
        );

        public final InstanceMethod viewAsSuper = new InstanceMethod(
                names.fromString("viewAsSuper"),
                symbols.derivedClassDescriptorType
        );

        public final BootstrapMethod constantClassDescriptorBsm = new BootstrapMethod(
                names.fromString("classDescriptor"),
                symbols.constantTypeDescriptorsType,
                symbols.classDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod constantArrayDescriptorBsm = new BootstrapMethod(
                names.fromString("arrayDescriptor"),
                symbols.constantTypeDescriptorsType,
                symbols.specializedTypeDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod erasedTypeDescriptorBsm = new BootstrapMethod(
                names.fromString("erasedClassDescriptor"),
                symbols.constantTypeDescriptorsType,
                symbols.erasedClassDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod constantMethodDescriptorBsm = new BootstrapMethod(
                names.fromString("methodDescriptor"),
                symbols.constantTypeDescriptorsType,
                symbols.methodDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final BootstrapMethod constantHiddenClassDescriptorBsm = new BootstrapMethod(
                names.fromString("hiddenClassDescriptor"),
                symbols.constantTypeDescriptorsType,
                symbols.hiddenClassDescriptorType,
                BootstrapMethod.Kind.CONDY
        );

        public final Symbol.OperatorSymbol objectEqOperator = operators
                .lookupBinaryOp(o -> o.opcode == ByteCodes.if_acmpeq);

    }

    private enum CurrentClassNestingMetadata {
        NONE,
        STATIC,
        INSTANCE,
        ;
    }

    @SuppressWarnings("this-escape")
    private TransParameterizedTypes(Context context) {
        context.put(KEY, this);
        make = TreeMaker.instance(context);
        instructionVisitor = new InstructionVisitor();
        translator = new Translator();
        log = Log.instance(context);
        symbols = Symtab.instance(context);
        names = Names.instance(context);
        types = Types.instance(context);
        operators = Operators.instance(context);
        resolve = Resolve.instance(context);
        lambdaToMethod = LambdaToMethod.instance(context);
        typeParameterScopes = new TypeVariableScopes();
        typeDescriptorFactory = new TypeDescriptorFactory();
        transTypes = TransTypes.instance(context);
        enter = Enter.instance(context);
        var options = Options.instance(context);
        enabled = options.isSet("enableSpecialization");
        enableSynthetic = !options.isSet("showGeneratedCode");
    }

    public static TransParameterizedTypes instance(Context context) {
        var instance = context.get(KEY);
        if (instance == null) instance = new TransParameterizedTypes(context);
        return instance;
    }
    //endregion

    private boolean hasNewGenerics(Symbol.TypeSymbol clazz) {
        return hasNewGenerics(clazz, symbols);
    }

    public static boolean hasNewGenerics(Symbol.TypeSymbol clazz, Symtab symbols) {
        Objects.requireNonNull(clazz);
        Objects.requireNonNull(symbols);
        if (clazz.specializationFlagInitialized()) {
            return clazz.isSpecialized();
        }

        boolean value;
        if (clazz.getDeclarationAttributes().contains(symbols.instrumentedAnnotation)) {
            value = true;
        } else {
            Symbol.PackageSymbol packge = clazz.packge();
            var modle = packge.modle;
            value = packge.getQualifiedName().contentEquals("org.example") || modle != null && modle.name.contentEquals("jdk.compiler");
//            var pkgName = clazz.packge().getQualifiedName();
//            value = !(
//                    pkgName.startsWith(names.java_lang)
//                            || pkgName.startsWith(names.jdk_internal)
//                            || pkgName.contentEquals("java.util.ptype")
//            );
        }

        clazz.initSpecializationFlag(value);
        return value;
    }

    //region rewriting (class)
    private final class Translator extends TreeTranslator {

        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) {
            result = tree;

            if (!enabled) return;

            if (constantsHolder == null) constantsHolder = new ConstantHolder();

            // this call needs to occur after constantsHolder has been initialized
            if (!hasNewGenerics(tree.sym)) return;

            try {
                rewriteClass(tree);
            } catch (RuntimeException | AssertionError t) {
                log.printRawLines("error in class: " + tree.sym.fullname);
                throw t;
            }
        }

    }

    private void rewriteClass(JCTree.JCClassDecl tree) {
        var oldClassContext = classContext;

        var highestClassInGenericHierarchy = tree.sym.highestGenericClassInHierarchy(symbols);
        var isHighestClassInGenericHierarchy = tree.sym == highestClassInGenericHierarchy;

        Symbol.VarSymbol descriptorField = null;
        if (highestClassInGenericHierarchy != null) {
            var iterator = highestClassInGenericHierarchy
                    .members()
                    .getSymbolsByName(
                            constantsHolder.objectTypeArgumentsFieldName,
                            s -> s instanceof Symbol.VarSymbol,
                            Scope.LookupKind.NON_RECURSIVE
                    )
                    .iterator();
            // the field already exists
            if (iterator.hasNext()) {
                descriptorField = (Symbol.VarSymbol) iterator.next();
            } else {
                // here we create the field in the highest class in the hierarchy.
                descriptorField = createDescriptorFieldSymbol(highestClassInGenericHierarchy);
                highestClassInGenericHierarchy.members().enterIfAbsent(descriptorField);
                // FIXME The field is not added to the tree, which might be an issue.
            }
        }

        classContext = new ClassContext(
                tree,
                tree.sym,
                descriptorField,
                isHighestClassInGenericHierarchy,
                highestClassInGenericHierarchy != null,
                enter.getEnv(tree.sym),
                nameOffsetIndex,
                isParameterized(tree.sym)
        );

        addAnnotations();

        try (var _ = typeParameterScopes.pushClassScope(descriptorField)) {
            rewriteDefinitions();

            var currentClass = classContext.classSymbol();

            // if we are parameterized or if the current class is plain but is under a generic interface
            if (
                    hasNewGenerics(currentClass)
                            && !currentClass.isInterface()
                            && currentClass == highestClassInGenericHierarchy
            ) {
                // in case the field has not been initialized
                currentClass.getInterfaces();

                // we make the type implement the ClassDescriptorHolder interface
                var type = (Type.ClassType) currentClass.type;
                type.interfaces_field = type.interfaces_field.prepend(symbols.classDescriptorHolderType);
                // here we need to append to ensure keeping the correct typing in case we are instrumenting an anonymous
                // class deriving an interface, e.g. new Iterator<>() { ... } because the first implementing will be
                // used.
                tree.implementing = tree.implementing.append(make.Type(symbols.classDescriptorHolderType));

                var fieldAccessorInstanceMethod = fieldAccessorInstanceMethodSymbol(currentClass);
                var accessor = fieldAccessorInstanceMethod(fieldAccessorInstanceMethod);
                tree.defs = tree.defs.prepend(accessor);
                currentClass.members().enterIfAbsent(accessor.sym);
            }
        } finally {
            classContext = oldClassContext;
        }
    }

    private void addAnnotations() {
        classContext.classSymbol().appendAttributes(List.of(symbols.instrumentedAnnotation));

        if (classNestingMetadata == CurrentClassNestingMetadata.NONE) {
            return;
        }
        var isStatic = resolve.resolveInternalMethod(
                classContext.classTree(),
                classContext.env(),
                symbols.nestedClassMetadataAnnotationType,
                names.fromString("isStatic"),
                List.nil(),
                List.nil()
        );
        var attribute = new Attribute.Constant(
                symbols.booleanType,
                (classNestingMetadata == CurrentClassNestingMetadata.STATIC) ? 1 : 0
        );
        var annotation = new Attribute.Compound(
                symbols.nestedClassMetadataAnnotationType,
                List.of(Pair.of(isStatic, attribute))
        );
        classContext.classSymbol().appendAttributes(List.of(annotation));
    }

    private Symbol.VarSymbol createDescriptorFieldSymbol(Symbol.ClassSymbol owner) {
        return new Symbol.VarSymbol(
                PROTECTED | FINAL | TRANSIENT | optionalSynthetic(),
                constantsHolder.objectTypeArgumentsFieldName,
                symbols.classDescriptorType,
                owner
        );
    }

    private JCTree.JCMethodDecl fieldAccessorInstanceMethod(Symbol.MethodSymbol method) {
        if (classContext.descriptorField() == null) {
            throw new AssertionError("Descriptor field should not be null.");
        }
        var ret = make.Return(make.Ident(classContext.descriptorField()));
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
                        symbols.derivedClassDescriptorType,
                        List.nil(),
                        symbols.methodClass
                ),
                owner
        );
    }

    private void rewriteDefinitions() {
        normalizeDefinitions().forEach(member -> {
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

                case VARDEF -> {
                    // fields init have been moved to blocks, nothing to do.
                }

                case BLOCK -> rewriteBlock((JCTree.JCBlock) member);

                default -> throw new AssertionError("Unexpected member type: " + member.getTag());
            }
            resetIndex();
        });
    }

    /// Normalize the definitions of a class to be able to process fields and static blocks. All static blocks and
    /// static field initializers are moved into a single static block. All instance field initializers are moved to a
    /// single instruction block.
    private List<JCTree> normalizeDefinitions() {
        var buffer = new ListBuffer<JCTree>();
        var staticBlock = new ListBuffer<JCTree.JCStatement>();

        classContext.classTree().defs.forEach(member -> {
            switch (member.getTag()) {
                case VARDEF -> {
                    var field = (JCTree.JCVariableDecl) member;
                    if (field.init == null) {
                        buffer.add(field);
                        return;
                    }
                    if (field.sym.isStatic()) {
                        if (!field.sym.isEnum()) {
                            staticBlock.add(make.Assignment(field.sym, field.init));
                            field.init = null;
                        }
                    } else {
                        var assign = make.Assignment(field.sym, field.init);
                        buffer.add(make.Block(0L, List.of(assign)));
                        field.init = null;
                    }
                    buffer.add(field);
                }
                case BLOCK -> {
                    var block = (JCTree.JCBlock) member;
                    if ((block.flags & STATIC) != 0) {
                        staticBlock.addAll(block.stats);
                    } else {
                        buffer.add(block);
                    }
                }
                default -> buffer.add(member);
            }
        });
        if (staticBlock.nonEmpty()) {
            buffer.add(make.Block(STATIC, staticBlock.toList()));
        }

        return classContext.classTree().defs = buffer.toList();
    }
    //endregion

    //region rewriting (members)
    private void rewriteBasicMethod(JCTree.JCMethodDecl method) {
        if (method.body == null) return;

        try (var _ = typeParameterScopes.pushMethodScope(method.sym)) {
            instructionVisitor.rewriteMethod(method);
            adjustRegularMethodBody(method);
        }
    }

    private void rewriteConstructor(JCTree.JCMethodDecl method) {
        try (var _ = typeParameterScopes.pushMethodScope(method.sym)) {
            var constructorArgsVariable = typeParameterScopes.constructorPassedDescriptor();
            instructionVisitor.rewriteConstructor(method, constructorArgsVariable);
            var doesCallOverload = TreeInfo.hasConstructorCall(method, names._this);
            adjustConstructorBody(method, doesCallOverload ? null : constructorArgsVariable);
        }
    }

    private void rewriteBlock(JCTree.JCBlock block) {
        if (block.stats.isEmpty()) return;

        var blockMethod = new Symbol.MethodSymbol(
                block.isStatic() ? STATIC : 0,
                block.isStatic() ? names.clinit : names.init,
                new Type.MethodType(List.nil(), symbols.voidType, List.nil(), symbols.methodClass),
                classContext.classSymbol()
        );

        if (!block.isStatic()) {
            var oldMetadata = classNestingMetadata;
            classNestingMetadata = CurrentClassNestingMetadata.INSTANCE;
            try (var _ = typeParameterScopes.pushInitBlockScope(blockMethod)) {
                instructionVisitor.rewriteBlock(block, blockMethod);
                var buffer = new ListBuffer<JCTree.JCStatement>();
                typeParameterScopes.generateVariables(buffer::add);
                buffer.addAll(block.stats);
                block.stats = buffer.toList();
            } finally {
                classNestingMetadata = oldMetadata;
            }
            return;
        }

        var oldClassNestingMetadata = classNestingMetadata;
        classNestingMetadata = CurrentClassNestingMetadata.STATIC;
        try {
            var methodDef = make.VarDef(
                    createVariable(symbols.methodDescriptorType, blockMethod),
                    constantsHolder.methodTypeArguments.call()
            );
            var constructorDef = make.VarDef(
                    createVariable(symbols.classDescriptorType, blockMethod),
                    constantsHolder.constructorTypeArguments.call()
            );

            instructionVisitor.rewriteBlock(block, blockMethod);
            var tryBlock = make.Block(0L, block.stats);

            var methodPush = constantsHolder.pushMethod.call(make.Ident(methodDef));
            var constructorPush = constantsHolder.pushConstructor.call(make.Ident(constructorDef));

            var finallyBlock = make.Block(0L, List.of(make.Exec(methodPush), make.Exec(constructorPush)));
            var tryFinally = make.Try(tryBlock, List.nil(), finallyBlock);

            block.stats = List.of(methodDef, constructorDef, tryFinally);
        } finally {
            classNestingMetadata = oldClassNestingMetadata;
        }
    }

    private void adjustConstructorBody(JCTree.JCMethodDecl method, Symbol.VarSymbol argsVariable) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        typeParameterScopes.generateVariables(newInstructions::add);

        if (argsVariable != null && classContext.isHighestClassInGenericHierarchy()) {
            var fieldInit = make.Exec(
                    make.Assign(
                            make.Ident(classContext.descriptorField()),
                            make.TypeCast(symbols.classDescriptorType, make.Ident(argsVariable))
                    )
            );
            newInstructions.add(fieldInit);
        }

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private void adjustRegularMethodBody(JCTree.JCMethodDecl method) {
        var newInstructions = new ListBuffer<JCTree.JCStatement>();

        // insert a pop in all non-private non-generic instance methods
        // (because for generic method it will be inserted automatically)
        if (!method.sym.isStatic() && !method.sym.isPrivate() && !method.sym.type.hasTag(TypeTag.FORALL)) {
            var pop = constantsHolder.methodTypeArguments.call();
            newInstructions.add(make.Exec(pop));
        }

        typeParameterScopes.generateVariables(newInstructions::add);

        method.body.stats = newInstructions.appendList(method.body.stats).toList();
    }

    private JCTree.JCStatement fallbackIf(Symbol.VarSymbol variable, JCTree.JCExpression fallback) {
        return make.If(
                nullComp(variable),
                make.Exec(make.Assign(make.Ident(variable), fallback)),
                null
        );
    }

    private JCTree.JCBinary nullComp(Symbol.VarSymbol variable) {
        var binary = make.Binary(JCTree.Tag.EQ, make.Ident(variable), nullLiteral());
        binary.operator = constantsHolder.objectEqOperator;
        binary.type = symbols.booleanType;
        return binary;
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
            var sym = (Symbol.MethodSymbol) TreeInfo.symbol(tree.meth);
            if (sym == null) throw new AssertionError("No symbol for " + tree.meth);
            if (sym.attribute(symbols.compilerIntrinsicType.tsym) != null) {
                super.visitApply(tree);
                handleCompilerIntrinsic(tree, sym);
                return;
            }

            if (!hasNewGenerics(sym.owner.type.tsym)) {
                super.visitApply(tree);
                return;
            }

            var isParameterizedMethod = sym.type.getTypeArguments().nonEmpty();
            var isConstructorFromParameterizedClass = sym.isConstructor() && sym.owner.type.isParameterized();
            if (!isParameterizedMethod && !isConstructorFromParameterizedClass) {
                super.visitApply(tree);
                return;
            }

            List<JCTree.JCStatement> pushStatements = List.nil();

            if (isConstructorFromParameterizedClass) {
                var push = pushSuperOrThisCode(tree);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            if (isParameterizedMethod) {
                var push = pushMethodCode(tree.typeargs, tree.inferenceMapping, sym, tree.isRaw);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            // we init this variable only if we need to wrap the meth in a let.
            JCTree.JCFieldAccess methSelect = null;
            if (tree.meth.hasTag(JCTree.Tag.SELECT)) {
                var select = (JCTree.JCFieldAccess) tree.meth;
                var selectedSym = TreeInfo.symbol(select.selected);
                if (selectedSym == null || selectedSym.kind != Kinds.Kind.TYP) {
                    methSelect = select;
                }
            }

            super.visitApply(tree);

            // if the method has arguments, we need to push the information after the evaluation of the last arg.
            if (tree.args.nonEmpty()) {
                tree.args = insertPostCall(pushStatements, sym, tree.meth.type.asMethodType(), tree.args);
                // if the method has a meth that is not a class, we need to push after evaluating it
            } else if (methSelect != null) {
                var selectedVariable = make.VarDef(
                        createVariable(methSelect.selected.type, extraVariablesOwner),
                        methSelect.selected
                );
                var let = make.LetExpr(
                        pushStatements.prepend(selectedVariable),
                        make.Ident(selectedVariable)
                ).setType(selectedVariable.type);
                var select = make.Select(let, sym);
                // the Select factory will put <init> as the name, we need to put the actual 'this' or 'super' name.
                select.name = methSelect.name;
                tree.meth = select;
            } else {
                if (tree.type.hasTag(TypeTag.VOID)) {
                    result = make.Block(
                            0L,
                            List.<JCTree.JCStatement>of(make.Exec(tree)).prependList(pushStatements)
                    );
                } else {
                    result = make.LetExpr(pushStatements, tree).setType(tree.type.baseType());
                }
            }
        }

        @Override
        public void visitNewClass(JCTree.JCNewClass tree) {
            super.visitNewClass(tree);
            var sym = (Symbol.MethodSymbol) tree.constructor;
            if (!hasNewGenerics(sym.owner.type.tsym)) return;

            var isParameterizedMethod = sym.type.getTypeArguments().nonEmpty();
            var isConstructorFromParameterizedClass = hasGenericTypeInHierarchy(sym.owner.type);
            if (!isParameterizedMethod && !isConstructorFromParameterizedClass) return;

            List<JCTree.JCStatement> pushStatements = List.nil();

            if (isConstructorFromParameterizedClass) {
                var push = pushConstructorCode(tree, tree.type.getTypeArguments(), sym);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            if (isParameterizedMethod) {
                var push = pushMethodCode(tree.typeargs, tree.inferenceMapping, sym, tree.isRaw);
                pushStatements = pushStatements.prepend(make.Exec(push));
            }

            // if the method has arguments, we need to push the information after the evaluation of the last arg.
            if (!tree.args.isEmpty()) {
                tree.args = insertPostCall(pushStatements, sym, tree.constructorType.asMethodType(), tree.args);
                result = tree;
                // if the constructor has an outer, we need to push after evaluating it
            } else if (tree.encl != null) {
                var selectedVariable = make.VarDef(
                        createVariable(tree.encl.type, extraVariablesOwner),
                        tree.encl
                );
                tree.encl = make.LetExpr(
                        pushStatements.prepend(selectedVariable),
                        make.Ident(selectedVariable)
                ).setType(selectedVariable.type);
            } else {
                result = make.LetExpr(pushStatements, tree).setType(tree.type.baseType());
            }
        }

        @Override
        public void visitClassDef(JCTree.JCClassDecl tree) {
            // do not recurse on inner classes
            rewriteClass(tree);
            result = tree;
        }

        @Override
        public void visitLambda(JCTree.JCLambda tree) {
            var type = tree.target;

            List<Type> superTypes;
            switch (type.getKind()) {
                case DECLARED -> {
                    if (!hasGenericTypeInHierarchy(type)) {
                        handleRegularLambda(tree);
                        return;
                    }
                    superTypes = List.of(type);
                }
                case INTERSECTION -> {
                    var intersection = (Type.IntersectionClassType) type;
                    if (!hasGenericTypeInHierarchy(intersection.getComponents())) {
                        handleRegularLambda(tree);
                        return;
                    }
                    // the tail here is to remove Object which is the first arg
                    superTypes = intersection.getComponents().tail;
                }
                default -> throw new AssertionError("Unexpected type kind: " + type);
            }

            super.visitLambda(tree);

            var body = new ListBuffer<JCTree.JCStatement>();

            body.add(make.Exec(constantsHolder.methodTypeArguments.call()));

            switch (tree.getBodyKind()) {
                case EXPRESSION -> {
                    var meth = tree.getDescriptorType(types).asMethodType();
                    var expr = (JCTree.JCExpression) tree.body;
                    var statement = meth.getReturnType().hasTag(TypeTag.VOID)
                            ? make.Exec(expr)
                            : make.Return(expr);
                    body.add(statement);
                }
                case STATEMENT -> body.appendList(((JCTree.JCBlock) tree.body).stats);
            }

            tree.body = make.Block(0L, body.toList());

            JCTree.JCExpression pushed;
            var arguments = superTypes.map(typeDescriptorFactory::createTypeDescriptor);
            if (constantList(arguments)) {
                pushed = constantsHolder.constantHiddenClassDescriptorBsm.call(arguments.map(i -> {
                    var identifier = (JCTree.JCIdent) i;
                    return (Symbol.DynamicVarSymbol) identifier.sym;
                }));
                tree.specializationKind = JCTree.JCFunctionalExpression.SpecializationKind.CONSTANT;
            } else {
                pushed = constantsHolder.hiddenClassDescriptorOf.call(arguments);
                tree.specializationKind = JCTree.JCFunctionalExpression.SpecializationKind.DYNAMIC;
            }

            var push = constantsHolder.pushHiddenClass.call(pushed);

            result = make.LetExpr(
                    List.of(make.Exec(push)),
                    tree
            ).setType(tree.type);
        }

        @Override
        public void visitReference(JCTree.JCMemberReference tree) {
            // we always convert to a lambda, as we at least need to add a pop.
            var expr = transTypes.convertToLambda(tree, make, classContext.env());
            var lambda = switch (expr.getTag()) {
                case LETEXPR -> {
                    var let = (JCTree.LetExpr) expr;
                    yield (JCTree.JCLambda) let.expr;
                }
                case LAMBDA -> (JCTree.JCLambda) expr;
                default -> throw new AssertionError("Unexpected tree: " + expr);
            };
            if (lambda.getBodyKind() != LambdaExpressionTree.BodyKind.EXPRESSION) {
                throw new AssertionError("Lambda body should be an expression but was: " + lambda.body);
            }
            setMappings(lambda.body, tree.inferenceMapping);

            expr.accept(this);
        }

        public void rewriteMethod(JCTree.JCMethodDecl method) {
            var oldExtraVariablesOwner = extraVariablesOwner;
            try {
                extraVariablesOwner = method.sym;
                visitMethodDef(method);
            } finally {
                extraVariablesOwner = oldExtraVariablesOwner;
            }
        }

        public void rewriteConstructor(JCTree.JCMethodDecl method, Symbol.VarSymbol constructorArgsVariable) {
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

        public void rewriteBlock(
                JCTree.JCBlock block,
                Symbol.MethodSymbol enclosing
        ) {
            var oldExtraVariablesOwner = extraVariablesOwner;
            try {
                extraVariablesOwner = enclosing;
                visitBlock(block);
            } finally {
                extraVariablesOwner = oldExtraVariablesOwner;
            }
        }

        private JCTree.JCExpression pushMethodCode(
                List<JCTree.JCExpression> explicitTypes,
                List<Pair<Type, Type>> inferredTypes,
                Symbol.MethodSymbol sym,
                boolean isRaw
        ) {
            var generatedArgs = basicMethodArgConstruction(
                    sym,
                    inferredTypes,
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

        private JCTree.JCExpression pushConstructorCode(
                JCTree.JCNewClass tree,
                List<Type> typeArguments,
                Symbol.MethodSymbol sym
        ) {
            var isRaw = typeArguments.isEmpty() != sym.owner.type.getTypeArguments().isEmpty();
            if (isRaw) {
                return typeDescriptorFactory.rawClassDescriptor((Type.ClassType) tree.type);
            }

            var fullArguments = new ListBuffer<JCTree.JCExpression>();
            typeArguments.forEach(t -> fullArguments.add(typeDescriptorFactory.createTypeDescriptor(t)));
            var captureStart = fullArguments.size();
            allParams(sym.owner.getEnclosingElement())
                    .forEach(p -> fullArguments.add(typeDescriptorFactory.createTypeDescriptor(p.type)));

            // if encl is null, it means that all the types that need to be captured are in the current scopes.
            // We can safely generate the literal for the type params.
            // We check if the owner is an inner class, because an absence of encl usually just means that the class is
            // not an inner class.
//            if (
//                    tree.encl == null
//                            && (sym.owner.isInner() || sym.owner.isAnonymous())
//                            && !sym.owner.isStatic()
//                            && isParameterized(sym.owner)
//            ) {
//                allParams(sym.owner.getEnclosingElement())
//                        .forEach(p -> fullArguments.add(typeDescriptorFactory.createTypeDescriptor(p.type)));
//                // on the other hand, if there is an explicit encl, we need to look a all the enclosing classes' types to
//                // to generate the captures that come from classes, and use the method type params of the enclosing methods
//                // for the captures that come from methods.
//            } else if (tree.encl != null && isParameterized(tree.encl.type.tsym)) {
//                addExplicitEnclosingArguments(fullArguments, tree.type);
//            }

            var args = fullArguments.toList();

            var pushedValue = constantList(args)
                    ? typeDescriptorFactory.constantClassDescriptor((Type.ClassType) sym.owner.type, args)
                    : classDescriptorConstructorInvocation(sym.owner.type, args, captureStart);

            return constantsHolder.pushConstructor.call(pushedValue);
        }

        private JCTree.JCExpression pushSuperOrThisCode(JCTree.JCMethodInvocation tree) {
            var methodName = Objects.requireNonNull(TreeInfo.name(tree.meth));

            // easiest case, this, we just push back the args without any modification
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

            var descriptor = typeDescriptorFactory.createTypeDescriptor(classContext.classSymbol().getSuperclass());
            return constantsHolder.pushConstructor.call(descriptor);
        }

        private void handleRegularLambda(JCTree.JCLambda tree) {
            super.visitLambda(tree);
            var pop = make.Exec(constantsHolder.methodTypeArguments.call());
            switch (tree.getBodyKind()) {
                case EXPRESSION -> {
                    var expr = (JCTree.JCExpression) tree.body;
                    var meth = tree.getDescriptorType(types).asMethodType();
                    var statement = meth.getReturnType().hasTag(TypeTag.VOID)
                            ? make.Exec(expr)
                            : make.Return(expr);
                    var body = List.of(pop, statement);
                    tree.body = make.Block(0L, body);
                }
                case STATEMENT -> {
                    var body = (JCTree.JCBlock) tree.body;
                    body.stats = body.stats.prepend(pop);
                }
            }
        }

        private Optional<List<JCTree.JCExpression>> basicMethodArgConstruction(
                Symbol.MethodSymbol sym,
                List<Pair<Type, Type>> inferredTypes,
                List<JCTree.JCExpression> explicitTypes,
                boolean isRaw
        ) {
            // by default, we try to use the provided type arguments Foo.<String>foo();, but if none are provided, we
            // use the inferred types `String s = foo();`
            if (explicitTypes.nonEmpty()) { // provided type arguments
                var list = explicitTypes.map(t -> typeDescriptorFactory.createTypeDescriptor(t.type));
                return Optional.of(list);
            }

            if (isRaw) {
                return Optional.empty();
            }
            if (inferredTypes.isEmpty()) {
                throw new AssertionError("Inferred types should not be empty");
            }

            var list = sym.type
                    .getTypeArguments()
                    .map(t -> typeDescriptorFactory.createTypeDescriptor(computeTypeFromInference(inferredTypes, t)));
            return Optional.of(list);
        }

        private Type computeTypeFromInference(List<Pair<Type, Type>> inferredTypes, Type t) {
            var typeSymbol = t.tsym;
            for (var pair : inferredTypes) { // we try to find the type in the map
                if (pair.fst.tsym == typeSymbol) {
                    return pair.snd;
                }
            }

            // if we end up here, it means that we are facing a '?'
            var upperBound = typeSymbol.type.getUpperBound();
            return new Type.WildcardType(
                    upperBound,
                    upperBound == symbols.objectType ? BoundKind.UNBOUND : BoundKind.EXTENDS,
                    null
            );
        }

        private List<JCTree.JCExpression> insertPostCall(
                List<JCTree.JCStatement> letStatements,
                Symbol.MethodSymbol method,
                Type.MethodType methodType,
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

                var lastArgType = methodType.getParameterTypes().last();
                var variableType = method.isVarArgs() ? ((Type.ArrayType) lastArgType).elemtype : lastArgType;
                var tempVariable = createVariable(variableType, extraVariablesOwner);
                var statements = letStatements.prepend(make.VarDef(tempVariable, next));
                var let = make.LetExpr(statements, make.Ident(tempVariable)).setType(variableType);
                newArguments.add(let);
            }

            return newArguments.toList();
        }

        private void addExplicitEnclosingArguments(ListBuffer<JCTree.JCExpression> buffer, Type type) {
            Symbol currentSymbol = type.getEnclosingType().tsym;
            var currentType = type;

            while (currentSymbol != null) {
                switch (currentSymbol.kind) {
                    case MTH -> currentSymbol
                            .type
                            .getTypeArguments()
                            .forEach(t -> buffer.add(typeDescriptorFactory.createTypeDescriptor(t)));
                    case TYP -> {
                        currentType = currentType.getEnclosingType();
                        currentType
                                .getTypeArguments()
                                .forEach(t -> buffer.add(typeDescriptorFactory.createTypeDescriptor(t)));
                    }
                    default -> {
                        return;
                    }
                }
                currentSymbol = currentSymbol.getEnclosingElement();
            }
        }

        private void handleCompilerIntrinsic(JCTree.JCMethodInvocation tree, Symbol.MethodSymbol sym) {
            if (!symbols.specializedTypeDescriptorType.equals(sym.owner.type)) return;

            if (names.of.equals(sym.name)) {
                JCTree.JCExpression arg;
                if (tree.typeargs.isEmpty()) {
                    arg = nullLiteral();
                } else {
                    var generatedArgs = basicMethodArgConstruction(
                            sym,
                            tree.inferenceMapping,
                            tree.typeargs,
                            false
                    );
                    arg = generatedArgs.orElseThrow(AssertionError::new).getFirst();
                }

                result = constantsHolder.filterPartialDescriptor.call(arg);
            }
        }

    }

    private final class TypeDescriptorFactory {

        public JCTree.JCExpression createTypeDescriptor(Type type) {
            return create(type);
        }

        private JCTree.JCExpression create(Type current) {
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
            var component = create(type.elemtype);
            if (component.hasTag(JCTree.Tag.IDENT)) {
                var identifier = (JCTree.JCIdent) component;
                var componentCondy = (Symbol.DynamicVarSymbol) identifier.sym;
                return constantsHolder.constantArrayDescriptorBsm.call(componentCondy);
            }
            return constantsHolder.arrayTypeOf.call(component);
        }

        private JCTree.JCExpression generateWildcardKind(Type.WildcardType ignored) {
            return constantsHolder.erasedTypeDescriptorBsm.call();
        }

        private JCTree.JCExpression generateIntersectionKind(Type.IntersectionClassType ignored) {
            return constantsHolder.erasedTypeDescriptorBsm.call();
        }

        private JCTree.JCExpression generateClassKind(Type.ClassType type) {
            if (type.isRaw()) {
                return constantClassDescriptor(type, List.nil());
            }

            var fullArguments = new ListBuffer<JCTree.JCExpression>();
            type.getTypeArguments().forEach(t -> fullArguments.add(typeDescriptorFactory.createTypeDescriptor(t)));
            var captureStart = fullArguments.size();
            addOuterTypes(fullArguments, type);

            var args = fullArguments.toList();

            var constantArguments = constantList(args);

            if (captureStart == fullArguments.size() && constantArguments) {
                return constantClassDescriptor(type, args);
            }

            return classDescriptorConstructorInvocation(type, args, captureStart);
        }

        private JCTree.JCExpression generateTypeVarKind(Type.TypeVar type) {
            if (type.isCaptured() || (type.tsym.flags() & SYNTHETIC) != 0) {
                return constantsHolder.erasedTypeDescriptorBsm.call();
            }

            return typeVarResolution(type.tsym);
        }

        private JCTree.JCExpression generatePrimitiveType(Type.JCPrimitiveType type) {
            return constantsHolder.constantClassDescriptorBsm.call(primitiveDescriptor(type), 0);
        }

        /// Resolves the usage of a type variable. This method generates the code that fetch the information of a type
        /// parameter at runtime.
        ///
        /// @param typeVar the symbol of the type variable that we are looking for
        private JCTree.JCExpression typeVarResolution(Symbol.TypeSymbol typeVar) {
            var res = typeParameterScopes.resolve(typeVar);
            if (res != null) return res;
            throw new AssertionError(
                    "Could not find type var "
                            + typeVar
                            + " (owner = "
                            + typeVar.owner
                            + ")"
                            + " in class "
                            + classContext.classSymbol()
                            + " with the following scope "
                            + typeParameterScopes
            );
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

        private JCTree.JCExpression constantClassDescriptor(Type.ClassType type, List<JCTree.JCExpression> arguments) {
            var condyArgs = new ListBuffer<>();
            condyArgs.add(typeToDescriptor(type));

            var flags = 0;
            if (type.isRaw()) {
                flags |= CONSTANT_DESCRIPTOR_BSM_FLAG_RAW;
            }

            condyArgs.add(flags);

            for (var argument : arguments) {
                var identifier = (JCTree.JCIdent) argument;
                var argCondy = (Symbol.DynamicVarSymbol) identifier.sym;
                condyArgs.add(argCondy);
            }

            return constantsHolder.constantClassDescriptorBsm.call(condyArgs.toList());
        }

        public JCTree.JCExpression rawClassDescriptor(Type.ClassType type) {
            if (!hasGenericTypeInHierarchy(type)) {
                throw new AssertionError("Type is not in a generic hierarchy: " + type);
            }
            return constantsHolder.constantClassDescriptorBsm.call(
                    typeToDescriptor(type),
                    CONSTANT_DESCRIPTOR_BSM_FLAG_RAW
            );
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
                toGenerate.getTypeArguments().forEach(t -> buffer.add(createTypeDescriptor(t)));
                currentSym = currentSym.getEnclosingElement();
            }
        }

    }

    /// Class managing scoping of type variables. Each time we enter an element declaring type variables (class,
    /// interface, method or constructor), we push the corresponding scope. The scope is responsible for generating
    /// local variables that allow access to the type variables, and also to provide through the
    /// [Scope#access(Symbol.VarSymbol, int)] method, a handle to generate accesses to the type variables of the scope
    /// (e.g., field$.argument(i) or method$.argument(i)), using the local variable the scope generated.
    private final class TypeVariableScopes {
        private List<Scope> scopes = List.nil();
        private List<ScopesSnapshot> snapshots = List.nil();
        private Symbol.VarSymbol constructorPassedDescriptor;

        public ScopeRemover pushClassScope(Symbol.VarSymbol descriptorField) {
            var typeParameters = allParams(classContext.classSymbol());
            var addedCount = 0;

            if (typeParameters.nonEmpty()) {
                Scope scope = classContext.classSymbol().isInterface()
                        ? new InterfaceScope(typeParameters)
                        : new ClassScope(typeParameters, descriptorField);

                scopes = scopes.prepend(scope);
                addedCount++;
            }
            return new ScopeRemover(addedCount, this, ScopeRemover.Action.NO_OP);
        }

        public ScopeRemover pushMethodScope(Symbol.MethodSymbol method) {
            return pushMethodAndConstructor(method, true);
        }

        public ScopeRemover pushInitBlockScope(Symbol.MethodSymbol method) {
            return pushMethodAndConstructor(method, false);
        }

        private ScopeRemover pushMethodAndConstructor(
                Symbol.MethodSymbol method,
                boolean allowConstructorPushing
        ) {
            var id = new GroupStateId();
            var addedCount = 0;

            if (allowConstructorPushing && method.isConstructor() && classContext.isInGenericHierarchy()) {
                // this is to handle regular classes that derives generic types, as the class won't have type params
                var variableParams = isParameterized(method.owner)
                        ? scopes.getFirst().variableParams()
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

            snapshots = snapshots.prepend(new ScopesSnapshot(id, scopes, method));
            return new ScopeRemover(addedCount, this, ScopeRemover.Action.POP_STATE);
        }

        public Symbol.VarSymbol constructorPassedDescriptor() {
            return constructorPassedDescriptor;
        }

        public void generateVariables(Consumer<JCTree.JCStatement> statementConsumer) {
            snapshots.getFirst().generateVariables(statementConsumer);
        }

        public JCTree.JCExpression resolve(Symbol.TypeSymbol typeVar) {
            var groupIndex = 0;
            for (var group : scopes) {
                var index = group.index(typeVar);
                if (index == -1) {
                    groupIndex++;
                    continue;
                }
                var variable = snapshots.head.variable(groupIndex);
                return group.access(variable, index);
            }
            return null;
        }

        @Override
        public String toString() {
            var builder = new StringBuilder();
            builder.append("Scopes:\n");
            for (var scope : scopes) {
                builder.append("\t - ");
                builder.append(scope);
                builder.append('\n');
            }
            return builder.toString();
        }

        private static final class GroupStateId {
        }

        /// A snapshot of the current group of scopes. A snapshot is generated each time we enter a method or a block,
        /// and each method and block have their own snapshot.
        ///
        /// Snapshots store the potential local variables for all the existing scopes at the time of the snapshot.
        /// This is done to avoid nested method wrongly access the variable from their enclosing methods.
        public static final class ScopesSnapshot {
            private final List<Slot> groups;
            private final GroupStateId id;

            private ScopesSnapshot(GroupStateId id, List<Scope> scopes, Symbol variableOwner) {
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
            private final InstanceMethod accessor;

            protected Base(List<Symbol.TypeSymbol> variableParams, GroupStateId owner, InstanceMethod accessor) {
                this.variableParams = variableParams;
                this.owner = owner;
                this.accessor = accessor;
            }

            @Override
            public final int index(Symbol.TypeSymbol typeVar) {
                return variableParams.indexOf(typeVar);
            }

            @Override
            public final JCTree.JCExpression access(Symbol.VarSymbol variable, int index) {
                return accessor.call(
                        accessVariable(variable),
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
                var builder = new StringBuilder();
                builder.append(getClass().getSimpleName());
                builder.append(": [");
                for (var iterator = variableParams.iterator(); iterator.hasNext(); ) {
                    var variableParam = iterator.next();
                    builder.append(variableParam);
                    builder.append(" (owner = ");
                    builder.append(variableParam.owner);
                    builder.append(")");
                    if (iterator.hasNext()) {
                        builder.append(", ");
                    }
                }
                builder.append("]");
                return builder.toString();
            }

            protected JCTree.JCExpression accessVariable(Symbol.VarSymbol variable) {
                return make.Ident(variable);
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
                super(variableParams, owner, constantsHolder.typeArgument);
                this.variable = createVariable(symbols.methodDescriptorType, method);
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

                // MethodTypeArgs methodTypeArgs = methodTypeArguments();
                statementConsumer.accept(
                        make.VarDef(
                                declarationVariable,
                                constantsHolder.methodTypeArguments.call()
                        )
                );

                // if (methodTypeArgs == null) methodTypeArgs = *raw type arguments;
                statementConsumer.accept(
                        fallbackIf(
                                declarationVariable,
                                constantsHolder.constantMethodDescriptorBsm.call()
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
            private final Symbol.VarSymbol descriptorField;
            private final Symbol.ClassSymbol classSymbol;

            private ClassScope(
                    List<Symbol.TypeSymbol> variableParams,
                    Symbol.VarSymbol descriptorField
            ) {
                super(variableParams, null, constantsHolder.argument);
                Objects.requireNonNull(descriptorField);
                this.descriptorField = descriptorField;
                this.classSymbol = classContext.classSymbol();
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return createVariable(symbols.classDescriptorType, currentOwner);
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                var fromConversion = constantsHolder.viewAsSuper.call(
                        make.Ident(descriptorField),
                        classLiteral(classSymbol.type)
                );
                statementConsumer.accept(make.VarDef(declarationVariable, fromConversion));
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                return usedInState;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return true;
            }

        }

        private final class InterfaceScope extends Base {
            private final Type.ClassType classType;

            private InterfaceScope(List<Symbol.TypeSymbol> variableParams) {
                super(variableParams, null, constantsHolder.argument);
                this.classType = (Type.ClassType) classContext.classSymbol().type;
            }

            @Override
            protected boolean shouldSaveInLocal() {
                return true;
            }

            @Override
            public void declaration(Symbol.VarSymbol declarationVariable, Consumer<JCTree.JCStatement> statementConsumer) {
                // SpecializedType args = ...;
                var init = constantsHolder.classDescriptor$From.call(
                        make.This(classType),
                        classLiteral(classType)
                );
                var declaration = make.VarDef(declarationVariable, init);
                statementConsumer.accept(declaration);

                // if (args == null) args = *raw type*
                var fallback = fallbackIf(
                        declarationVariable,
                        typeDescriptorFactory.rawClassDescriptor(classType)
                );
                statementConsumer.accept(fallback);
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return createVariable(symbols.classDescriptorType, currentOwner);
            }

            @Override
            public boolean shouldGenerate(boolean usedInState, GroupStateId currentState) {
                return usedInState;
            }

        }

        private final class ConstructorScope extends Base {
            private final Symbol.VarSymbol variable;
            private final Type.ClassType classType;

            private ConstructorScope(
                    List<Symbol.TypeSymbol> variableParams,
                    GroupStateId owner,
                    Symbol.MethodSymbol constructor
            ) {
                super(variableParams, owner, constantsHolder.argument);
                this.variable = createVariable(symbols.classDescriptorType, constructor);
                this.classType = (Type.ClassType) classContext.classSymbol().type;
            }

            @Override
            public void declaration(
                    Symbol.VarSymbol declarationVariable,
                    Consumer<JCTree.JCStatement> statementConsumer
            ) {
                // SpecializedType constructorArguments = MethodArgStack.constructorTypeArguments();
                var call = constantsHolder.constructorTypeArguments.call();
                statementConsumer.accept(make.VarDef(declarationVariable, call));

                // if (constructorArguments == null) constructorArguments = *raw type*
                statementConsumer.accept(
                        fallbackIf(
                                declarationVariable,
                                typeDescriptorFactory.rawClassDescriptor(classType)
                        )
                );
            }

            @Override
            public Symbol.VarSymbol variable(Symbol currentOwner) {
                return variable;
            }

            @Override
            protected JCTree.JCExpression accessVariable(Symbol.VarSymbol variable) {
                return constantsHolder.viewAsSuper.call(
                        make.Ident(variable),
                        classLiteral(classType)
                );
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

        private static class ScopeRemover implements AutoCloseable {
            private final int count;
            private final TypeVariableScopes scope;
            private final Action action;
            private boolean closed;

            private ScopeRemover(int count, TypeVariableScopes scope, Action action) {
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
                        scope.snapshots = scope.snapshots.tail;
                        scope.constructorPassedDescriptor = null;
                    }
                    case NO_OP -> {
                    }
                }
            }

            enum Action {
                NO_OP,
                POP_STATE,
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
            if (!types.isSubtype(receiver.type, ownerType)) {
                throw new AssertionError(
                        "Receiver " + receiver + " (" + receiver.type + ") is not subtype of type " + ownerType + "."
                );
            }
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

    }

    private final class BootstrapMethod {
        private final Name name;
        private final Type ownerType;
        private final Type expressionType;
        private final Kind kind;

        enum Kind {
            INDY,
            CONDY,
        }

        private BootstrapMethod(Name name, Type owner, Type expressionType, Kind kind) {
            this.name = name;
            this.ownerType = owner;
            this.expressionType = expressionType;
            this.kind = kind;
        }

        public JCTree.JCExpression call(List<?> arguments) {
            var method = resolve.resolveInternalMethod(
                    classContext.classTree(),
                    classContext.env(),
                    ownerType,
                    name,
                    arguments.map(this::objectToType)
                            .prepend(
                                    types.subst(
                                            symbols.classType,
                                            List.of(symbols.classType.getTypeArguments().getFirst()),
                                            List.of(expressionType)
                                    )
                            )
                            .prepend(symbols.stringType)
                            .prepend(symbols.methodHandleLookupType),
                    List.nil()
            );
            var constants = arguments.map(BootstrapMethod::objectToConstant)
                    .toArray(PoolConstant.LoadableConstant[]::new);

            return switch (kind) {
                case INDY -> throw new AssertionError();
                case CONDY -> {
                    var condy = new Symbol.DynamicVarSymbol(
                            name,
                            symbols.noSymbol,
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
                case Integer _ -> symbols.intType;
                case Long _ -> symbols.longType;
                case Float _ -> symbols.floatType;
                case Double _ -> symbols.doubleType;
                case String _ -> symbols.stringType;
                case Type.ClassType _ -> symbols.classType;
                case PoolConstant.LoadableConstant _ -> symbols.objectType;
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

    private JCTree.JCMethodInvocation externalMethodInvocation(
            Name name,
            Type site,
            List<JCTree.JCExpression> arguments,
            Function<Symbol.MethodSymbol, JCTree.JCExpression> fnMapper
    ) {
        Symbol.MethodSymbol method;
        try {
            method = resolve.resolveInternalMethod(
                    classContext.classTree(),
                    classContext.env(),
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
            Type type,
            List<JCTree.JCExpression> typeArguments,
            int captureStart
    ) {
        if (type.isRaw()) {
            return constantsHolder.constantClassDescriptorBsm.call(
                    typeToDescriptor(type),
                    CONSTANT_DESCRIPTOR_BSM_FLAG_RAW
            );
        }
        var arguments = typeArguments
                .prepend(make.Literal(captureStart))
                .prepend(classLiteral(type));
        return constantsHolder.classDescriptorOf.call(arguments);
    }
    //endregion

    //region utils
    public static void setMappings(JCTree tree, List<Pair<Type, Type>> mapping) {
        switch (tree.getTag()) {
            case APPLY -> {
                var apply = (JCTree.JCMethodInvocation) tree;
                apply.inferenceMapping = mapping;
            }
            case REFERENCE -> {
                var reference = (JCTree.JCMemberReference) tree;
                reference.inferenceMapping = mapping;
            }
            case NEWCLASS -> {
                var newClass = (JCTree.JCNewClass) tree;
                newClass.inferenceMapping = mapping;
            }
        }
    }

    /// This method takes into account captured outer types.
    public static boolean isParameterized(Symbol symbol) {
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
        return make.Literal(TypeTag.BOT, null).setType(symbols.botType);
    }

    private Symbol.VarSymbol createVariable(Type type, Symbol owner) {
        return new Symbol.VarSymbol(
                0L,
                names.fromString(nextVariableId("x")),
                type,
                owner
        );
    }

    private void resetIndex() {
        nameOffsetIndex = classContext.baseNameOffsetIndex();
    }

    private String nextVariableId(String prefix) {
        var offset = nameOffsetIndex;
        nameOffsetIndex = (byte) (nameOffsetIndex < 99 ? nameOffsetIndex + 1 : 0);
        var actualPrefix = prefix != null ? prefix + "$" : "";
        return actualPrefix + String.format("%02d", offset);
    }

    private static List<Symbol.TypeSymbol> getTypeArguments(Symbol sym) {
        var list = new ListBuffer<Symbol.TypeSymbol>();
        sym.type.getTypeArguments().forEach(t -> list.add(t.tsym));
        return list.toList();
    }

    private Symbol.ClassSymbol highestConcreteGenericSuperType(Symbol.ClassSymbol sym) {
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

    private boolean hasGenericTypeInHierarchy(Type type) {
        return hasGenericTypeInHierarchy(List.of(type));
    }

    private boolean hasGenericTypeInHierarchy(List<Type> types) {
        for (var type : types) {
            if (type == Type.noType || type == null || !hasNewGenerics(type.tsym)) continue;
            if (isParameterized(type.tsym)) return true;
            var cl = (Symbol.ClassSymbol) type.tsym;
            if (hasGenericTypeInHierarchy(List.of(cl.getSuperclass()))) return true;
            if (hasGenericTypeInHierarchy(cl.getInterfaces())) return true;
        }
        return false;
    }

    /// We only check 1 level for the super class, but the whole hierarchy tree for interfaces
    private boolean isGenericOrHasGenericInterface(Symbol.ClassSymbol sym) {
        if (!hasNewGenerics(sym)) return false;
        if (isParameterized(sym)) return true;

        var interfaces = sym.getInterfaces();
        while (interfaces.nonEmpty()) {
            var next = interfaces.getFirst();
            if (isParameterized(next.tsym)) return true;
            interfaces = interfaces.tail;
            for (var itf : ((Symbol.ClassSymbol) next.tsym).getInterfaces()) {
                interfaces = interfaces.prepend(itf);
            }
        }
        return false;
    }

    private int optionalSynthetic() {
        return 0;
//        return enableSynthetic ? SYNTHETIC : 0;
    }

    private boolean constantList(List<JCTree.JCExpression> expressions) {
        if (expressions.isEmpty()) return true;
        for (var expr : expressions) {
            if (!expr.hasTag(JCTree.Tag.IDENT)) return false;
        }
        return true;
    }

    private JCTree.JCExpression classLiteral(Type type) {
        return make.ClassLiteral(types.erasure(type)).setType(symbols.classType);
    }
    //endregion

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree classDef, TreeMaker make) {
        Objects.requireNonNull(env);
        Objects.requireNonNull(classDef);
        Objects.requireNonNull(make);

        try {
            this.make = make;
            return translator.translate(classDef);
        } finally {
            this.make = null;
        }
    }

}
