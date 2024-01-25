package java.util.ptype;

import java.util.Objects;

/**
 * Utility methods for working with {@link Arg}s.
 */
public final class TypeOperations {

    /**
     * Performs a cast to the given type. This cast ensures that parameterized types are cast correctly.
     *
     * @param obj      the object to cast
     * @param expected the expected type
     * @param <T>      the type to cast to
     * @return the cast object
     */
    @SuppressWarnings("unchecked")
    public static <T> T checkCast(Object obj, Arg expected) {
        Objects.requireNonNull(expected);
        if (obj == null) {
            return null;
        }

        if (!isInstance(obj, expected)) {
            throw new ClassCastException(classCastExceptionMessage(obj, expected));
        }

        return (T) obj;
    }

    private static boolean isInstance(Object obj, Arg expected) {
        return switch (expected) {
            // var cast = (A<String>.B<Integer>) obj;
            case InnerClassType innerClassType -> {
                if (!isInstance(obj, innerClassType.innerType())) { // check inner type
                    yield false;
                }
                yield Internal.outerThis(obj)
                    .map(arg -> isInstance(arg, innerClassType.outerType()))
                    .orElse(true); // by default if no outer type is specified, yield true
            }

            // var cast = (String) obj;
            case ClassType classType -> classType.type().isAssignableFrom(obj.getClass());

            // var cast = (List) obj;
            case RawType rawType -> validate(obj, expected, rawType.rawArg().rawType());

            // var cast = (List<String>) obj;
            case ParameterizedType parameterizedType -> validate(obj, expected, parameterizedType.rawType());

            // var cast = (Runnable & Serializable) obj;
            case Intersection intersection -> IterableUtils.allMatch(
                intersection.bounds(),
                bound -> isInstance(obj, bound)
            );

            // var cast = (List<String>[]) obj;
            case ArrayType arrayType -> {
                if (!obj.getClass().isArray()) {
                    yield false;
                }
                var arg = Internal.arrayType(obj);
                yield arrayType.isAssignable(arg);
            }

            // Note that this kind of cast should not be possible
            // var cast = (? extends String) obj;
            // var cast = (? super String) obj;
            // var cast = (?) obj;    equivalent to (? extends Object)
            case Wildcard wildcard -> IterableUtils.allMatch(wildcard.upperBound(), bound -> isInstance(obj, bound))
                && (
                wildcard.lowerBound().isEmpty()
                    || IterableUtils.anyMatch(wildcard.lowerBound(), bound -> isInstance(obj, bound))
            );
        };
    }

    private static boolean validate(Object obj, Arg expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var opt = Internal.argHandle(objClass).arg(obj, supertype);
        return opt.map(expected::isAssignable)
            .orElseGet(() -> supertype.isAssignableFrom(objClass));
    }

    private static String classCastExceptionMessage(Object obj, Arg expected) {
        var builder = new StringBuilder("Cannot cast ");
        var objClass = obj.getClass();
        var type = Internal.argHandle(objClass)
            .arg(obj, objClass)
            .map(arg -> (Object) arg)
            .orElse(objClass);
        builder.append(obj)
            .append(" of type (")
            .append(type)
            .append(") to ");
        expected.appendTo(builder);
        return builder.toString();
    }

    private TypeOperations() {
        throw new AssertionError();
    }

}
