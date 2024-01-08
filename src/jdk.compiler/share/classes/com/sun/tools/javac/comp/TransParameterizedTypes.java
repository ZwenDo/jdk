package com.sun.tools.javac.comp;

import com.sun.source.tree.Tree;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.TypeTag;
import com.sun.tools.javac.jvm.Target;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Names;

public final class TransParameterizedTypes extends TreeTranslator {

    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private TreeMaker make;
    private final Symtab syms;
    private final Names names;
    private final Type argType;
    private final Target target;
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
        argType = new Type.ClassType(Type.noType, List.nil(), null);
        argType.tsym = new Symbol.ClassSymbol(
            Flags.PUBLIC | Flags.INTERFACE | Flags.ABSTRACT,
            names.fromString("Arg"),
            argType,
            new Symbol.PackageSymbol(
                names.fromString("java.util.ptype"),
                null
            )
        );
    }

    @Override
    public void visitClassDef(JCTree.JCClassDecl tree) {
        // generate the base arg field
        if (!tree.name.contentEquals("Foo")) {
            result = tree;
            return;
        }
        var baseField = generateBaseArgField(tree);
        rewriteConstructors(tree, baseField);

//        var table = tree.name.table;
//        var returnType = syms.intType;
//        var flags = Flags.STATIC | Flags.PUBLIC;
//        var methodName = table.fromString("foo");
//
//        var returnValue = make.Literal(42);
//        var returnStatement = make.Return(returnValue);
//        var method = make.MethodDef(
//            make.Modifiers(flags),
//            methodName,
//            make.Type(returnType),
//            List.nil(),
//            List.nil(),
//            List.nil(),
//            make.Block(0L, List.of(returnStatement)),
//            null
//        );
//
//        var methodType = new Type.MethodType(List.nil(), returnType, List.nil(), tree.sym);
//        method.type = methodType;
//        var sym = new Symbol.MethodSymbol(flags, methodName, methodType, tree.sym);
//        method.sym = sym;
//        tree.sym.members_field.enter(sym);
//        tree.defs = tree.defs.append(method);

        result = tree;
    }

    private JCTree.JCVariableDecl generateBaseArgField(JCTree.JCClassDecl tree) {
        var fieldFlags = Flags.PRIVATE | Flags.FINAL;
        var fieldName = names.fromString(computeArgName(tree));
        var field = make.VarDef(
            make.Modifiers(fieldFlags),
            fieldName,
            make.Type(argType),
            null // init is done in constructor
        );
        var fieldSym = new Symbol.VarSymbol(fieldFlags, fieldName, argType, tree.sym);
        field.sym = fieldSym;
        field.type = argType;

        tree.sym.members_field.enter(fieldSym);
        tree.defs = tree.defs.append(field);
        return field;
    }

    private String computeArgName(JCTree.JCClassDecl owner) {
        var synChar = target.syntheticNameChar();
        var pkg = owner.sym.packge().name.toString().replace('.', synChar);
        var name = owner.name.toString();
        return "0" + synChar + "typeArgs" + synChar + pkg + synChar + synChar + name;
    }

    private void rewriteConstructors(JCTree.JCClassDecl tree, JCTree.JCVariableDecl baseField) {
        for (var member : tree.defs) {
            if (member.getKind() != Tree.Kind.METHOD) {
                continue;
            }

            var method = (JCTree.JCMethodDecl) member;
            if (!method.getName().equals(names.init)) {
                continue;
            }

            // before modifying the constructor, we create the overload for backward compatibility
            backwardCompatibilityOverloadConstructor(tree, method);

            // we then check whether the constructor calls another constructor
            var doesCallOverload = doesCallConstructor(method);

            // if so, we just need to add the extra argument to the call of the other constructor
            if (doesCallOverload) {
                appendArgToThisCall(method);
            } else { // otherwise, this constructor must set the base field
                setFieldValue(method, baseField);
            }
        }
    }

    private JCTree.JCMethodDecl backwardCompatibilityOverloadConstructor(
        JCTree.JCClassDecl clazz,
        JCTree.JCMethodDecl baseConstructor
    ) {
        var copy = copyConstructor(baseConstructor);
        var modified = appendArgToConstructorParams(baseConstructor);

        var baseParamTypeArgs = modified.sym.type.getTypeArguments();
        List<JCTree.JCExpression> baseParamTypeArgsCopy = baseParamTypeArgs.isEmpty()
            ? List.nil()
            : make.Types(baseParamTypeArgs);

        var params = new ListBuffer<JCTree.JCExpression>();
        copy.params.forEach(p -> params.add(make.Ident(p)));
        var defaultTypeArgs = createDefaultTypeArgs();
        params.add(defaultTypeArgs);
        ((Type.MethodType) copy.type).argtypes = modified.sym.type.getTypeArguments();
        copy.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
        copy.sym.flags_field |= Flags.SYNTHETIC | Flags.MANDATED;

        copy.body = make.at(modified.pos)
            .Block(
                0L,
                List.of(
                    make.Exec(
                        make.Apply(
                            baseParamTypeArgsCopy,
                            make.Ident(modified.sym),
                            List.from(params)
                        ).setType(syms.voidType)
                    )
                )
            );

        clazz.sym.members_field.enter(copy.sym);
        clazz.defs = clazz.defs.append(copy);

        return modified;
    }

    private JCTree.JCExpression createDefaultTypeArgs() {
        var rawTypeCall = make.Apply(
            List.nil(),
            make.Ident(names.fromString("java.util.ptype.RawType.of")),
            List.of(make.Literal(TypeTag.BOT, null).setType(syms.botType))
        );
        return rawTypeCall;
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

    private JCTree.JCMethodDecl appendArgToConstructorParams(JCTree.JCMethodDecl method) {
        var oldPos = method.pos;
        var extraArgParam = make.at(method.pos).Param(
            names.fromString("0" + target.syntheticNameChar() + "arg"),
            argType,
            method.sym
        );
        extraArgParam.mods.flags |= Flags.SYNTHETIC | Flags.MANDATED;
        make.at(oldPos);

        method.params = method.params.append(extraArgParam);
        var mtype = (Type.MethodType) method.type;
        mtype.argtypes = mtype.argtypes.append(argType);
        method.sym.extraParams = method.sym.extraParams.append(extraArgParam.sym);

        return method;
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
        call.args = args.append(make.Ident(constructor.params.last()));
    }

    private void setFieldValue(JCTree.JCMethodDecl constructor, JCTree.JCVariableDecl field) {
        var instructions = constructor.body.stats;
        var assign = make.Assign(
            make.Ident(field.sym),
            make.Ident(constructor.params.last().sym)
        );
        // we set the field value before calling the super constructor as it is now allowed
        instructions = instructions.prepend(make.Exec(assign));
        constructor.body.stats = instructions;
    }

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
