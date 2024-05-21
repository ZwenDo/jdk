package java.util.ptype;

/**
 * Utility methods for working with {@link Arg}s.
 */
public final class TypeOperations {

    /**
     * Performs a cast to the given type. This cast ensures that parameterized types are cast correctly.
     *
     * @param obj      the object to cast
     * @param expected the expected type
     * @return the cast object
     */
    @SuppressWarnings("unchecked")
    public static Object checkCast(Object obj, Arg expected) {
        Utils.requireNonNull(expected);
        if (obj == null) {
            return null;
        }

        // TODO uncomment
//        if (!isInstance(obj, expected)) {
//            System.err.println((message(obj, expected)));
//        }

        return obj;
    }

    private static boolean isInstance(Object obj, Arg expected) {
        return switch (expected) {
            // var cast = (A<String>.B<Integer>) obj;
            case InnerClassType innerClassType -> {
                if (!isInstance(obj, innerClassType.innerType())) { // check inner type
                    yield false;
                }
                var outer = Internal.outerThis(obj);
                if (outer.isPresent()) {
                    yield isInstance(outer.get(), innerClassType.outerType());
                } else { // by default if no outer type is specified, yield true
                    yield true;
                }
            }

            // var cast = (String) obj;
            case ClassType classType -> classType.type().isAssignableFrom(obj.getClass());

            // var cast = (List) obj;
            case RawType rawType -> validate(obj, expected, rawType.type());

            // var cast = (List<String>) obj;
            case ParameterizedType parameterizedType -> validate(obj, expected, parameterizedType.rawType());

            // var cast = (Runnable & Serializable) obj;
            case Intersection intersection -> intersection.bounds().allMatch(bound -> isInstance(obj, bound));

            // var cast = (List<String>[]) obj;
            case ArrayType arrayType -> {
                if (!obj.getClass().isArray()) {
                    yield false;
                }
                var arg = Internal.arrayType(obj);
                if (arg.isPresent()) {
                    yield arrayType.isAssignable(arg.get());
                } else {
                    yield true;
                }
            }

            // Note that this kind of cast should not be possible
            // var cast = (? extends String) obj;
            // var cast = (? super String) obj;
            // var cast = (?) obj;    equivalent to (? extends Object)
            case Wildcard ignored -> throw new AssertionError("Wilcard cast should not be possible");
        };
    }

    private static boolean validate(Object obj, Arg expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var opt = Internal.argHandle(objClass).arg(obj, supertype);
        if (opt.isEmpty()) {
            return supertype.isAssignableFrom(objClass);
        }
        return expected.isAssignable(opt.get());
    }

    private static String message(Object obj, Arg expected) {
        var objClass = obj.getClass();

        var builder = new StringBuilder("Cannot cast ")
            .append(obj)
            .append(" (");

        var arg = Internal.argHandle(objClass)
            .arg(obj, objClass);
        if (arg.isPresent()) {
            arg.get().appendTo(builder);
        } else {
            builder.append(objClass.getName());
        }

        builder.append(") to ");
        expected.appendTo(builder);
        return builder.toString();
    }

    private TypeOperations() {
        throw new AssertionError();
    }

}
