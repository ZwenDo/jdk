package java.util.ptype;

import jdk.internal.misc.VM;

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

                if (classDescriptor.hasTypeArguments()) {
                    builder.append('<');
                    classDescriptor.forEahTypeArgument(new BiConsumer<TypeDescriptor, Boolean>() {
                        @Override
                        public void accept(TypeDescriptor descriptor, Boolean hasNext) {
                            appendToBuilder(builder, descriptor);
                            if (hasNext) {
                                builder.append(", ");
                            }
                        }
                    });
                    builder.append('>');
                }

                if (classDescriptor.hasCapture()) {
                    builder.append(" (");
                    classDescriptor.forEachCapture(new BiConsumer<TypeDescriptor, Boolean>() {
                        @Override
                        public void accept(TypeDescriptor descriptor, Boolean hasNext) {
                            appendToBuilder(builder, descriptor);
                            if (hasNext) {
                                builder.append(", ");
                            }
                        }
                    });
                    builder.append(')');
                }

                break;
            case ErasedClassDescriptor _:
                builder.append("*erased*");
                break;
        }
    }
    //endregion

    //region Type Verification

    /// Tests whether a given object has the expected [TypeDescriptor] and returns it. This method will print an error
    /// if the `obj` is not a subtype of the `expected` specialized type.
    ///
    /// @param obj      the object to test
    /// @param expected the expected type
    /// @return the object
    public static Object checkCast(
            Object obj,
            TypeDescriptor expected
    ) {
        Utils.requireNonNull(expected);
        if (!VM.isBooted()) return obj;

        if (obj == null) return null;
        var actual = obj instanceof ClassDescriptorHolder holder ? holder.$descriptor() : null;

        if (actual == null) {
            return obj;
        }

        if (!isInstance(obj, expected)) {
            System.err.println(errorMessage(obj, expected));
        }

        return obj;
    }

    private static boolean isInstance(Object obj, TypeDescriptor expected) {
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

    private static boolean validate(Object obj, TypeDescriptor expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var value = extractInformationField(obj);
        if (value == null) {
            return supertype.isAssignableFrom(objClass);
        }
        return isAssignable(expected, value);
    }

    private static String errorMessage(Object obj, TypeDescriptor expected) {
        var objClass = obj.getClass();
        if (objClass.isAnonymousClass()) {
            var interfaces = objClass.getInterfaces();
            objClass = interfaces.length > 0 ? interfaces[0] : objClass.getSuperclass();
        }

        var builder = new StringBuilder();

        var type = extractInformationField(objClass);
        if (type != null) {
            appendToBuilder(builder, type);
        } else {
            builder.append(objClass.getName());
        }

        builder.append(" to ");
        appendToBuilder(builder, expected);
        return builder.toString();
    }

    private static boolean isAssignable(TypeDescriptor expected, TypeDescriptor actual) {
        return false;
    }

    private static ClassDescriptor extractInformationField(Object object) {
        return null;
    }
    //endregion

    private TypeDescriptorUtils() {
        throw new AssertionError();
    }

}
