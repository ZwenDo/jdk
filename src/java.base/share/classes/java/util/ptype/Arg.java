package java.util.ptype;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;

/**
 * All possible type representations.
 */
public sealed interface Arg permits ArrayType, ClassType, InnerClassType, Intersection, ParameterizedType, RawType, Wildcard {

    /**
     * Whether this type is assignable from the given type.
     *
     * @param actual   the actual type
     * @param variance the variance
     * @return {@code true} if this type is assignable from the given type
     */
    boolean isAssignable(Arg actual, Variance variance);

    /**
     * Append this type to the given string builder.
     *
     * @param builder the string builder
     */
    void appendTo(StringBuilder builder);

    /**
     * The variance of a type.
     */
    enum Variance {
        /**
         * Covariant.
         */
        COVARIANT,
        /**
         * Contravariant.
         */
        CONTRAVARIANT,

        /**
         * Invariant.
         */
        INVARIANT,
    }

    /**
     * Creates an {@link Arg} from the given type.
     *
     * @param type the type
     * @return the {@link Arg}
     */
    static Arg fromType(Type type) {
        Arg.dump();
        Utils.requireNonNull(type);
        if (type instanceof Class<?> clazz) {
            return ClassType.of(clazz);
        } else if (type instanceof java.lang.reflect.ParameterizedType parameterizedType) {
            return ParameterizedType.of(
                (Class<?>) parameterizedType.getRawType(),
                fromTypes(parameterizedType.getActualTypeArguments())
            );
        } else if (type instanceof WildcardType wildcardType) {
            if (wildcardType.getLowerBounds().length == 0) { // no lower bound
                return Wildcard.ofUpper(fromTypes(wildcardType.getUpperBounds()));
            } else {
                return Wildcard.ofLower(fromTypes(wildcardType.getLowerBounds()));
            }
        } else if (type instanceof GenericArrayType genericArrayType) {
            return ArrayType.of(
                fromType(genericArrayType.getGenericComponentType())
            );
        } else if (type instanceof TypeVariable<?> typeVariable) {
            if (typeVariable.getBounds().length == 1) { // single bound
                return fromType(typeVariable.getBounds()[0]);
            }
            // intersection
            return Intersection.of(fromTypes(typeVariable.getBounds()));
        }
        throw new IllegalArgumentException("Unknown type: " + type + " (" + type.getClass() + ")");
    }

    /**
     * Returns a string representation of the given {@link Arg}.
     *
     * @param arg the {@link Arg}
     * @return the string representation
     */
    static String toString(Arg arg) {
        Utils.requireNonNull(arg);
        var builder = new StringBuilder();
        arg.appendTo(builder);
        return builder.toString();
    }


    private static Arg[] fromTypes(Type[] types) {
        var args = new Arg[types.length];
        for (int i = 0; i < types.length; i++) {
            args[i] = fromType(types[i]);
        }
        return args;
    }

    static void dump() {
//        if (true) return;
//        var stackTrace = new Exception().getStackTrace();
//        var path = Path.of("dump.txt");
//        try {
//            Files.writeString(
//                path,
//                "dump: " + stackTrace[1] + " // " + stackTrace[2],
//                StandardOpenOption.APPEND,
//                StandardOpenOption.CREATE
//            );
//        } catch (IOException e) {
//            throw new RuntimeException(e);
//        }
    }

}
