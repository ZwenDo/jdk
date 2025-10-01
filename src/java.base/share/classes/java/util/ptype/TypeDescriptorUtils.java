package java.util.ptype;


import java.lang.reflect.AccessFlag;
import java.lang.reflect.Executable;

final class TypeDescriptorUtils {

    //region Stringify
    static String stringify(TypeDescriptor type) {
        Utils.requireNonNull(type);
        return stringify(new StringBuilder(), type);
    }

    static String stringify(StringBuilder builder, TypeDescriptor type) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(builder);
        appendToBuilder(builder, type);
        return builder.toString();
    }

    private static void appendToBuilder(StringBuilder builder, TypeDescriptor type) {
        switch (type) {
            case ArrayDescriptor arrayDescriptor:
                appendToBuilder(builder, arrayDescriptor.componentType());
                builder.append("[]");
                break;
            case ClassDescriptor classDescriptor:
                appendClassDescriptorToBuilder(builder, classDescriptor);
                break;
            case ErasedClassDescriptor _:
                builder.append("*erased*");
                break;
        }
    }

    private static void appendClassDescriptorToBuilder(StringBuilder builder, ClassDescriptor classDescriptor) {
        if (classDescriptor.isRaw()) {
            builder.append(classDescriptor.type().getSimpleName());
            builder.append("<*raw*>");
            return;
        }
        appendClass(builder, classDescriptor, 0, classDescriptor.type());
    }

    private static void appendClass(StringBuilder builder, ClassDescriptor classDescriptor, int offset, Class<?> current) {
        final var currentTypeParamsCount = current.getTypeParameters().length;
        final var outerOffset = offset + currentTypeParamsCount;

        if (!current.accessFlags().contains(AccessFlag.STATIC)) {
            var enclosingMethod = enclosingMethod(current);
            var enclosingMethTypeParamsCount = 0;
            var isEnclosingMethodStatic = false;

            if (enclosingMethod != null) {
                enclosingMethTypeParamsCount = enclosingMethod.getTypeParameters().length;
                isEnclosingMethodStatic = enclosingMethod.accessFlags().contains(AccessFlag.STATIC);
            }

            if (!isEnclosingMethodStatic) {
                var enclosingClass = current.getEnclosingClass();
                if (enclosingClass != null) {
                    appendClass(builder, classDescriptor, outerOffset + enclosingMethTypeParamsCount, enclosingClass);
                    builder.append('.');
                }
            }

            if (enclosingMethod != null) {
                builder.append(enclosingMethod.getName());
                appendTypeArguments(builder, classDescriptor, enclosingMethTypeParamsCount, outerOffset);
                builder.append("().");
            }
        }

        builder.append(current.getSimpleName());
        appendTypeArguments(builder, classDescriptor, currentTypeParamsCount, offset);
    }

    private static void appendTypeArguments(
            StringBuilder builder,
            ClassDescriptor classDescriptor,
            int end,
            int offset
    ) {
        if (end == 0) return;
        builder.append('<');
        for (var i = 0; i < end; i++) {
            appendToBuilder(builder, classDescriptor.argument(i + offset));
            if (i + 1 < end) {
                builder.append(", ");
            }
        }
        builder.append('>');
    }

    private static Executable enclosingMethod(Class<?> current) {
        var method = current.getEnclosingMethod();
        if (method != null) return method;
        return current.getEnclosingConstructor();
    }

    //endregion

    private TypeDescriptorUtils() {
        throw new AssertionError();
    }

}
