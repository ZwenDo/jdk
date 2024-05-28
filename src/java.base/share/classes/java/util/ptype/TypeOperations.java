package java.util.ptype;

import jdk.internal.misc.VM;

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
    public static Object checkCast(Object obj, Arg expected) {
        Utils.requireNonNull(expected);
        if (!VM.isBooted()) return obj;

        if (obj == null) {
            return null;
        }

        if (!isInstance(obj, expected)) {
            System.err.println((message(obj, expected)));
        }

        return obj;
    }

    private static boolean isInstance(Object obj, Arg expected) {
        // var cast = (A<String>.B<Integer>) obj;
        if (expected instanceof InnerClassType innerClassType) {
            if (!isInstance(obj, innerClassType.innerType())) { // check inner type
                return false;
            }
            var outer = Internal.outerThis(obj);
            if (outer.isPresent()) {
                return isInstance(outer.get(), innerClassType.outerType());
            } else { // by default if no outer type is specified, yield true
                return true;
            }
            // var cast = (String) obj;
        } else if (expected instanceof ClassType classType) {
            return classType.type().isAssignableFrom(obj.getClass());
            // var cast = (List) obj;
        } else if (expected instanceof RawType rawType) {
            return validate(obj, expected, rawType.type());
            // var cast = (List<String>) obj;
        } else if (expected instanceof ParameterizedType parameterizedType) {
            return validate(obj, expected, parameterizedType.rawType());
            // var cast = (Runnable & Serializable) obj;
        } else if (expected instanceof Intersection intersection) {
            return intersection.bounds().allMatch(new ArgList.ArgPredicate() {
                @Override
                public boolean test(Arg bound) {
                    return isInstance(obj, bound);
                }
            });
            // var cast = (List<String>[]) obj;
        } else if (expected instanceof ArrayType arrayType) {
            if (!obj.getClass().isArray()) {
                return false;
            }
            var arg = Internal.arrayType(obj);
            if (arg.isPresent()) {
                return arrayType.isAssignable(arg.get(), Arg.Variance.COVARIANT);
            } else {
                return true;
            }
            // Note that this kind of cast should not be possible
            // var cast = (? extends String) obj;
            // var cast = (? super String) obj;
            // var cast = (?) obj;    equivalent to (? extends Object)
        } else if (expected instanceof Wildcard) {
            throw new AssertionError("Wilcard cast should not be possible");
        }
        throw new IllegalArgumentException();
    }

    private static boolean validate(Object obj, Arg expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var opt = Internal.argHandle(objClass).arg(obj, supertype);
        if (opt.isEmpty()) {
            return supertype.isAssignableFrom(objClass);
        }
        return expected.isAssignable(opt.get(), Arg.Variance.COVARIANT);
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
