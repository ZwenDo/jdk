package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.Names;

public final class TransParameterizedTypes extends TreeTranslator {

    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private TreeMaker make;
    private Symtab syms;
    private Names names;
    private Type argType;
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
        generateBaseArgField(tree);

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
        var fieldFlags = Flags.PRIVATE;
        var fieldName = names.fromString(computeArgName(tree));
        var field = make.VarDef(
            make.Modifiers(fieldFlags),
            fieldName,
            make.Type(argType),
            null // init done in constructor
        );
        var fieldSym = new Symbol.VarSymbol(fieldFlags, fieldName, argType, tree.sym);
        field.sym = fieldSym;
        field.type = argType;

        tree.sym.members_field.enter(fieldSym);
        tree.defs = tree.defs.append(field);
        return field;
    }

    private String computeArgName(JCTree.JCClassDecl owner) {
        var pkg = owner.sym.packge().name.toString().replace('.', '$');
        var name = owner.name.toString();
        return "typeArgs$" + pkg + "$$" + name;
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
