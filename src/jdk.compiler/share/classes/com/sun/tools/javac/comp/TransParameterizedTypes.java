package com.sun.tools.javac.comp;

import com.sun.source.tree.Tree;
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
        var fields = generateFields(tree);
        rewriteMethods(tree, fields);

        result = tree;
    }

    // region Base arg field generation

    private Map<Symbol, JCTree.JCVariableDecl> generateFields(JCTree.JCClassDecl tree) {
        var result = new LinkedHashMap<Symbol, JCTree.JCVariableDecl>();

        // first generate the base field
        var baseField = generateField(tree, tree.sym);
        result.put(tree.sym, baseField);

        // then, optionally generate the fields for the super type
        if (tree.extending != null && tree.extending.type.isParameterized() && isParameterizedByClass((JCTree.JCTypeApply) tree.extending)) {
            var classExpression = (JCTree.JCIdent) ((JCTree.JCTypeApply) tree.extending).clazz;
            var field = generateField(tree, classExpression.sym);
            result.put(classExpression.sym, field);
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
            result.put(classExpression.sym, field);
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

    private void rewriteMethods(JCTree.JCClassDecl tree, Map<Symbol, JCTree.JCVariableDecl> fields) {
        for (var member : tree.defs) {
            if (member.getKind() != Tree.Kind.METHOD) {
                continue;
            }
            var method = (JCTree.JCMethodDecl) member;

            if (method.getName().equals(names.init)) {
                rewriteConstructor(tree, method, fields);
            } else {
                rewriteBasicMethod(tree, method, fields);
            }
        }
    }

    // region Constructor rewriting

    private void rewriteConstructor(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl method,
        Map<Symbol, JCTree.JCVariableDecl> fields
    ) {
        var copy = copyConstructor(method);
        prependArgToConstructorParams(method);

        // before modifying the constructor, we create the overload for backward compatibility
        backwardCompatibilityOverloadConstructor(tree, method, copy);

        // we then check whether the constructor calls another constructor
        var doesCallOverload = doesCallConstructor(method);

        // if so, we just need to add the extra argument to the call of the other constructor
        if (doesCallOverload) {
            appendArgToThisCall(method);
        } else { // otherwise, this constructor must set the base field
            setFieldsValues(tree, method, fields);
        }
    }

    private void backwardCompatibilityOverloadConstructor(
        JCTree.JCClassDecl clazz,
        JCTree.JCMethodDecl modified,
        JCTree.JCMethodDecl copy
    ) {
        var baseParamTypeArgs = modified.sym.type.getTypeArguments();
        List<JCTree.JCExpression> baseParamTypeArgsCopy = baseParamTypeArgs.isEmpty()
            ? List.nil()
            : make.Types(baseParamTypeArgs);

        var params = new ListBuffer<JCTree.JCExpression>();
        copy.params.forEach(p -> params.add(make.Ident(p)));
        var defaultTypeArgs = createDefaultTypeArgs(copy.pos, clazz);
        params.prepend(defaultTypeArgs);
        ((Type.MethodType) copy.type).argtypes = modified.sym.type.getTypeArguments();
        copy.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
        copy.sym.flags_field |= Flags.SYNTHETIC | Flags.MANDATED;

        var ident = make.Ident(modified.sym);
        ident.name = names._this;
        copy.body = make.at(modified.pos)
            .Block(
                0L,
                List.of(
                    make.Exec(
                        make.Apply(
                            baseParamTypeArgsCopy,
                            ident,
                            params.toList()
                        ).setType(syms.voidType)
                    )
                )
            );

        clazz.sym.members_field.enter(copy.sym);
        clazz.defs = clazz.defs.append(copy);
    }

    private JCTree.JCExpression createDefaultTypeArgs(int pos, JCTree.JCClassDecl classDecl) {
        var rawTypeCall = externalMethodInvocation(
            pos,
            "of",
            syms.rawTypeTypeArgs,
            syms.rawTypeTypeArgs,
            List.of(syms.parameterizedTypeTypeArgs)
        );
        var parameterizedTypeCall = externalMethodInvocation(
            pos,
            "of",
            syms.parameterizedTypeTypeArgs,
            syms.parameterizedTypeTypeArgs,
            List.of(syms.classType, syms.argBaseType, types.makeArrayType(syms.argBaseType))
        );
        rawTypeCall.args = List.of(parameterizedTypeCall);

        // MyType.class call as first argument of ParameterizedType.of
        var classFieldAccess = make.Select(
            make.Ident(classDecl.sym),
            syms.getClassField(classDecl.type, types)
        );

        // append of all type arguments wrapped in a ClassType.of call
        var builder = new ListBuffer<JCTree.JCExpression>();
        builder.add(classFieldAccess);
        classDecl.sym.getTypeParameters().forEach(parameter -> {
            var head = parameter.getBounds().head;
            var classFieldAcc = make.Select(
                make.Type(head),
                syms.getClassField(head, types)
            );
            var call = externalMethodInvocation(
                pos,
                "of",
                syms.classTypeArgs,
                syms.classTypeArgs,
                List.of(syms.classType)
            );
            call.args = List.of(classFieldAcc);
            builder.add(call);
        });

        parameterizedTypeCall.args = builder.toList();

        return rawTypeCall;
    }

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

    /**
     * Copy the given constructor without its body.
     */
    private JCTree.JCMethodDecl copyConstructor(JCTree.JCMethodDecl baseConstructor) {
        var treeCopy = make.at(baseConstructor.pos).MethodDef(
            baseConstructor.mods,
            baseConstructor.name,
            baseConstructor.restype,
            baseConstructor.typarams,
            baseConstructor.params,
            baseConstructor.thrown,
            null,
            baseConstructor.defaultValue
        );
        var symbolCopy = new Symbol.MethodSymbol(
            baseConstructor.sym.flags(),
            baseConstructor.sym.name,
            baseConstructor.sym.type,
            baseConstructor.sym.owner
        );
        symbolCopy.params = baseConstructor.sym.params;
        symbolCopy.extraParams = baseConstructor.sym.extraParams;
        symbolCopy.capturedLocals = baseConstructor.sym.capturedLocals;
        symbolCopy.defaultValue = baseConstructor.sym.defaultValue;
        symbolCopy.type = new Type.MethodType(
            baseConstructor.sym.type.getParameterTypes(),
            baseConstructor.sym.type.getReturnType(),
            baseConstructor.sym.type.getThrownTypes(),
            baseConstructor.type.tsym
        );
        treeCopy.sym = symbolCopy;
        treeCopy.type = new Type.MethodType(
            baseConstructor.sym.type.getParameterTypes(),
            baseConstructor.sym.type.getReturnType(),
            baseConstructor.sym.type.getThrownTypes(),
            baseConstructor.type.tsym
        );

        return treeCopy;
    }

    private void prependArgToConstructorParams(JCTree.JCMethodDecl method) {
        var oldPos = method.pos;
        var extraArgParam = make.at(method.pos).Param(
            names.fromString("0" + target.syntheticNameChar() + "arg"),
            syms.argBaseType,
            method.sym
        );
        extraArgParam.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
        make.at(oldPos);

        method.params = method.params.prepend(extraArgParam);
        var mtype = (Type.MethodType) method.type;
        mtype.argtypes = mtype.argtypes.prepend(syms.argBaseType);
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
        Map<Symbol, JCTree.JCVariableDecl> fields
    ) {
        var instructions = constructor.body.stats;

        var buffer = new ListBuffer<JCTree.JCStatement>();
        buffer.addAll(instructions);

        fields.forEach((sym, field) -> {
            if (tree.sym.equals(sym)) { // base field
                var assign = make.Assign(
                    make.Ident(field.sym),
                    make.Ident(constructor.params.getFirst().sym)
                );
                buffer.prepend(make.Exec(assign));
                return;
            }

            var factoryCall = externalMethodInvocation(
                constructor.pos,
                "toSuper",
                syms.typeArgUtils,
                syms.argBaseType,
                List.of(syms.argBaseType, syms.classType, types.makeArrayType(syms.intType))
            );


            // TODO change last parameter. It will imply to modify the TypeArg API because it does not cover all cases
            factoryCall.args = List.of(
                make.Ident(constructor.params.getFirst().sym),
                make.Select(
                    make.Ident(sym),
                    syms.getClassField(sym.type, types)
                ),
                make.Literal(sym.getTypeParameters().size())
            );
            buffer.prepend(make.Exec(factoryCall));
        });

        // we set the field value before calling the super constructor as it is now allowed
        constructor.body.stats = buffer.toList();
    }

    // endregion

    // region Basic method rewriting

    private void rewriteBasicMethod(
        JCTree.JCClassDecl tree,
        JCTree.JCMethodDecl method,
        Map<Symbol, JCTree.JCVariableDecl> baseField
    ) {

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
