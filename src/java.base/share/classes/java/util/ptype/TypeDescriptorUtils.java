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
            appendClassName(builder, classDescriptor.type());
            builder.append("<*raw*>");
            return;
        }
        appendClass(builder, classDescriptor, 0, classDescriptor.type());
    }

    private static void appendClass(StringBuilder builder, ClassDescriptor classDescriptor, int offset, Class<?> current) {
        final var currentTypeParamsCount = current.getTypeParameters().length;
        final var outerOffset = offset + currentTypeParamsCount;

        if (!current.accessFlags().contains(AccessFlag.STATIC)) {
            var enclosingMethod = ReflectionUtils.enclosingMethod(current);
            var enclosingMethTypeParamsCount = 0;
            var processOuterClass = true;

            if (enclosingMethod != null) {
                enclosingMethTypeParamsCount = enclosingMethod.getTypeParameters().length;
                processOuterClass = !enclosingMethod.accessFlags().contains(AccessFlag.STATIC);
            }

            // Special case for classes defined in blocks or field initializers
            if (enclosingMethod == null && !current.isMemberClass()) {
                var annotation = current.getAnnotation(NestedClassMetadata.class);
                processOuterClass = annotation != null && !annotation.isStatic();
            }

            if (processOuterClass) {
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

        appendClassName(builder, current);
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

    private static void appendClassName(StringBuilder builder, Class<?> type) {
        var name = type.getSimpleName();
        if (!name.isEmpty()) {
            builder.append(name);
        } else {
            builder.append(type.getName());
            builder.append('(');
            var interfaces = type.getInterfaces();
            if (interfaces.length == 0 || type.getSuperclass() != Object.class) {
                builder.append(type.getSuperclass().getSimpleName());
                if (interfaces.length > 1 || (interfaces.length == 1 && interfaces[0] != ClassDescriptorHolder.class)) {
                    builder.append(" & ");
                }
            }
            for (int i = 0; i < interfaces.length; i++) {
                if (interfaces[i] == ClassDescriptorHolder.class) {
                    continue;
                }
                builder.append(interfaces[i].getSimpleName());
                if (i + 1 < interfaces.length) {
                    if (interfaces[i + 1] == ClassDescriptorHolder.class) {
                        break;
                    }
                    builder.append(" & ");
                }
            }
            builder.append(")");
        }
    }

    //endregion

    private TypeDescriptorUtils() {
        throw new AssertionError();
    }

}
