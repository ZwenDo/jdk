package java.util.ptype;

import jdk.internal.misc.VM;

import java.util.Optional;
import java.util.ptype.util.Utils;

/// Class providing operations on [SpecializedTypes][SpecializedTypeDescriptor].
public final class SpecializedTypeUtils {

    //region Stringify
    static String stringify(SpecializedTypeDescriptor type) {
        Utils.requireNonNull(type);
        var builder = new StringBuilder();
        builder.append("SpecializedTypeDescriptor(");
        appendToBuilder(builder, type);
        builder.append(")");
        return builder.toString();
    }

    static void appendToBuilder(StringBuilder builder, SpecializedTypeDescriptor type) {
        Utils.requireNonNull(builder);
        Utils.requireNonNull(type);
        switch (type) {
            case ArrayDescriptor arrayDescriptor:
                appendToBuilder(builder, arrayDescriptor.componentType());
                builder.append("[]");
                break;
            case ClassDescriptor classDescriptor:
                var outer = classDescriptor.outer();
                if (outer.isPresent()) {
                    appendToBuilder(builder, outer.get());
                    builder.append('.');
                }

                builder.append(classDescriptor.type().getSimpleName());

                if (classDescriptor.isRaw()) {
                    builder.append("(raw)");
                    break;
                }

                if (!classDescriptor.hasTypeArguments()) break;

                builder.append('<');
                classDescriptor.joinArguments(builder);
                builder.append('>');

                break;
            case ErasedType _:
                builder.append("?");
                break;
        }
    }

    static void joinSpecializedTypeArray(StringBuilder builder, SpecializedTypeDescriptor[] array) {
        for (int i = 0; i < array.length; i++) {
            appendToBuilder(builder, array[i]);
            if (i + 1 < array.length) {
                builder.append(", ");
            }
        }
    }
    //endregion

    static boolean partiallyRaw(SpecializedTypeDescriptor descriptor) {
        Utils.requireNonNull(descriptor);
        switch (descriptor) {
            case ArrayDescriptor arrayDescriptor:
                return arrayDescriptor.partiallyRaw();
            case ClassDescriptor classDescriptor:
                return classDescriptor.partiallyRaw();
            case ErasedType _:
                return true;
        }
    }

    //region Type Verification

    /// Tests whether a given object has the expected [SpecializedTypeDescriptor] and returns it. This method will print an error
    /// if the `obj` is not a subtype of the `expected` specialized type.
    ///
    /// @param obj the object to test
    /// @param expected the expected type
    /// @return the object
    public static Object checkCast(
            Object obj,
            SpecializedTypeDescriptor expected
    ) {
        Utils.requireNonNull(expected);
        if (!VM.isBooted()) return obj;

        if (obj == null) return null;

        if (!isInstance(obj, expected)) {
            System.err.println(errorMessage(obj, expected));
        }

        return obj;
    }

    private static boolean isInstance(Object obj, SpecializedTypeDescriptor expected) {
        return false;
//        switch (expected) {
//            // var cast = (A<String>.B<Integer>) obj;
//            case InnerClassType innerClassType:
//                if (!isInstance(obj, innerClassType.innerType())) { // check inner type
//                    return false;
//                }
//
//                // we need to extract the expected outer class, because the actual object inner class might have an outer
//                // this that does not extend the expected outer class (e.g. Attr.ResultInfo & Resolve.MethodResultInfo).
//                Class<?> expectedOuterClass;
//                var outerClassArg = innerClassType.outerType();
//                if (outerClassArg instanceof ClassType outerClassType) {
//                    expectedOuterClass = outerClassType.type();
//                } else if (outerClassArg instanceof ParameterizedType parameterizedType) {
//                    expectedOuterClass = parameterizedType.rawType();
//                } else {
//                    throw new AssertionError("Unexpected outer type: " + innerClassType.outerType());
//                }
//
//                var outer = Internal.outerThis(obj, expectedOuterClass);
//                if (outer.isPresent()) {
//                    return isInstance(outer.get(), innerClassType.outerType());
//                } else { // by default if no outer type is specified, yield true
//                    return true;
//                }
//
//                // var cast = (String) obj; (usually (E) obj;)
//            case ClassType classType:
//                return classType.type().isAssignableFrom(obj.getClass());
//
//            // var cast = (List<String>) obj;
//            case ParameterizedType parameterizedType:
//                return validate(obj, expected, parameterizedType.rawType());
//            // var cast = (List<String>[]) obj;
//            case ArrayType arrayType:
//                if (!obj.getClass().isArray()) return false;
//                return validate(obj, expected, obj.getClass());
//
//            case null:
//            default:
//                throw new AssertionError();
//        }
    }

    private static boolean validate(Object obj, SpecializedTypeDescriptor expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var value = Internal.extractInformationField(obj);
        if (value == null) {
            return supertype.isAssignableFrom(objClass);
        }
        return isAssignable(expected, value);
    }

    private static String errorMessage(Object obj, SpecializedTypeDescriptor expected) {
        var objClass = obj.getClass();
        if (objClass.isAnonymousClass()) {
            var interfaces = objClass.getInterfaces();
            objClass = interfaces.length > 0 ? interfaces[0] : objClass.getSuperclass();
        }

        var builder = new StringBuilder();

        var type = Internal.extractInformationField(objClass);
        if (type != null) {
            appendToBuilder(builder, type);
        } else {
            builder.append(objClass.getName());
        }

        builder.append(" to ");
        appendToBuilder(builder, expected);
        return builder.toString();
    }

    private static boolean isAssignable(SpecializedTypeDescriptor expected, SpecializedTypeDescriptor actual) {
        return false;
    }
    //endregion

    /// Utility method that filters out erased types to return only fully specialized types to users.
    ///
    /// @param type the type to filter
    /// @return the filtered type or null if the type is erased
    /// @param <T> the type of the specialized type descriptor
    public static <T extends SpecializedTypeDescriptor> Optional<T> filterErasedType(T type) {
        Utils.requireNonNull(type);
        return partiallyRaw(type) ? Optional.empty() : Optional.of(type);
    }


    private SpecializedTypeUtils() {
        throw new AssertionError();
    }
}
