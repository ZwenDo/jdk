package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.TypeTag;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;

import java.util.Set;

public final class TransParameterizedTypes extends TreeTranslator {

    /**
     * The context key for the TransParameterizedTypes phase.
     */
    private static final Context.Key<TransParameterizedTypes> typeReifierKey = new Context.Key<>();

    private TreeMaker make;
    private Symtab syms;
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
    }

    @Override
    public void visitClassDef(JCTree.JCClassDecl tree) {
        tree.mods = translate(tree.mods);
        tree.typarams = translateTypeParams(tree.typarams);
        tree.extending = translate(tree.extending);
        tree.implementing = translate(tree.implementing);
        tree.defs = translate(tree.defs);

//        var table = tree.name.table;
//        var flags = Flags.STATIC | Flags.PUBLIC | Flags.FINAL;
//        var fieldName = table.fromString("foo");
//        var type = syms.intType;
//        var init = make.Literal(TypeTag.INT, 42).setType(syms.intType.constType(42));
//
//        var field = make.VarDef(
//            make.Modifiers(flags),
//            fieldName,
//            make.Type(type),
//            init
//        );
//        var sym = new Symbol.VarSymbol(flags, fieldName, type, tree.sym);
//        field.sym = sym;
//        field.type = type;
//        tree.sym.members_field.enter(sym);
//        tree.defs = tree.defs.append(field);


//        var table = tree.name.table;
//        var returnType = syms.intType;
//        var flags = Flags.STATIC | Flags.PUBLIC;
//        var methodName = table.fromString("foo");
//
//        var returnValue = make.Literal(TypeTag.INT, 42).setType(syms.intType.constType(42));
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
//        tree.defs = defs.append(method);


        result = tree;
    }

    private void generateArgField(JCTree.JCClassDecl clazz, JCTree.JCClassDecl owner) {
//        var modifierFlags = Flags.PRIVATE | Flags.FINAL;
//        var modifiers = make.Modifiers(modifierFlags);
//        var fieldName = computeArgName(owner);
//        var symbol = new Symbol.VarSymbol();
//        var field = make.VarDef(
//            modifiers,
//            fieldName,
//
//        );
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
