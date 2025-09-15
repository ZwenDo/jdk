package java.util.ptype;

import jdk.internal.misc.VM;

import java.util.ptype.util.Utils;


/// Class providing operations on [SpecializedTypes][SpecializedTypeDescriptor].
public final class SpecializedTypeUtils {

    //region Extraction
    /// Extracts a specific nested [SpecializedTypeDescriptor] from another [SpecializedTypeDescriptor].
    ///
    /// Each index in the `indices` argument represent a level of nesting in the `container` specialized type.
    ///
    /// Here is an example of how this method works:
    ///
    /// given the type `type` representing `List<Function<String, Integer>>`, the following call `extract(type, 0)` will
    /// return `Function<String, Integer>`. The call `extract(type, 0, 1)` will yield `Integer`
    ///
    /// @param container the container in which we want to extract information
    /// @param indices the indices of the information in the container, each element representing a new level of nesting
    /// @return the extracted [SpecializedTypeDescriptor]
    public static SpecializedTypeDescriptor extract(SpecializedTypeDescriptor container, int... indices) {
        Utils.requireNonNull(container);
        Utils.requireNonNull(indices);
        if (indices.length == 0) {
            throw new IllegalArgumentException("indices.length == 0");
        }

        var currentType = container;
        for (var index : indices) {
            switch (currentType) {
                case ClassDescriptor p:
                    if (p.typeArguments().isEmpty()) throw new AssertionError(p + " is not parameterized.");
                    currentType = p.typeArguments().get(index);
                    break;
                case ArrayDescriptor a:
                    currentType = a.componentType();
                    while (currentType instanceof ArrayDescriptor arrayDescriptor) {
                        currentType = arrayDescriptor.componentType();
                    }
                    break;
                default:
                    throw new AssertionError("Unexpected value: " + currentType);
            }
        }

        return currentType;
    }

    /// Extracts the specialized type information.
    ///
    /// @param obj the object containing the specialized type
    /// @return the specialized type
    public static SpecializedTypeDescriptor extractField(Object obj) {
        Utils.requireNonNull(obj);
        var field = Internal.extractInformationField(obj);
        if (field.isEmpty()) {
            return null;
        }
        return field.get();
    }
    //endregion

    //region Stringify
    static String stringify(SpecializedTypeDescriptor type) {
        Utils.requireNonNull(type);
        var builder = new StringBuilder();
        appendToBuilder(builder, type);
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

                if (classDescriptor.typeArguments().isEmpty()) break;

                if (classDescriptor.isRaw()) {
                    builder.append("(raw)");
                    break;
                }

                builder.append('<');
                classDescriptor.typeArguments().joinTo(builder, SpecializedTypeUtils::appendToBuilder, ", ");
                builder.append('>');

                break;
            case ErasedType _:
                builder.append("?");
                break;
        }
    }
    //endregion

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
        var opt = Internal.extractInformationField(obj);
        if (opt.isEmpty()) {
            return supertype.isAssignableFrom(objClass);
        }
        return isAssignable(expected, opt.get());
    }

    private static String errorMessage(Object obj, SpecializedTypeDescriptor expected) {
        var objClass = obj.getClass();
        if (objClass.isAnonymousClass()) {
            var interfaces = objClass.getInterfaces();
            objClass = interfaces.length > 0 ? interfaces[0] : objClass.getSuperclass();
        }

        var builder = new StringBuilder();

        var type = Internal.extractInformationField(objClass);
        if (type.isPresent()) {
            appendToBuilder(builder, type.get());
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


    private SpecializedTypeUtils() {
        throw new AssertionError();
    }
}
