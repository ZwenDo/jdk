package java.util.ptype;

import jdk.internal.misc.VM;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;

/**
 * All possible type representations.
 */
public sealed interface Arg permits ArrayType, ClassType, InnerClassType, Intersection, ParameterizedType, RawType, Wildcard {

    /**
     * Creates an {@link Arg} from the given type.
     *
     * @param type the type
     * @return the {@link Arg}
     */
    static Arg fromType(Type type) {
        Utils.requireNonNull(type);
        return fromType(null, type);
    }

    private static Arg fromType(Type previous, Type current) {
        if (current instanceof Class<?> clazz) {
            return ClassType.of(clazz);
        } else if (current instanceof java.lang.reflect.ParameterizedType parameterizedType) {
            return ParameterizedType.of(
                (Class<?>) parameterizedType.getRawType(),
                fromTypes(current, parameterizedType.getActualTypeArguments())
            );
        } else if (current instanceof WildcardType wildcardType) {
            if (wildcardType.getLowerBounds().length == 0) { // no lower bound
                return Wildcard.ofUpper(fromTypes(current, wildcardType.getUpperBounds()));
            } else {
                return Wildcard.ofLower(fromTypes(current, wildcardType.getLowerBounds()));
            }
        } else if (current instanceof GenericArrayType genericArrayType) {
            return ArrayType.of(fromType(null, genericArrayType.getGenericComponentType()));
        } else if (current instanceof TypeVariable<?> typeVariable) {
            if (typeVariable.getBounds().length == 1) { // single bound
                var bound = typeVariable.getBounds()[0];
                return bound.equals(previous) ? ClassType.of(Object.class) : fromType(previous, bound);
            }
            // intersection
            return Intersection.of(fromTypes(previous, typeVariable.getBounds()));
        }
        throw new IllegalArgumentException("Unknown type: " + current + " (" + current.getClass() + ")");
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

    /**
     * Creates an array of {@link Arg} from the given types.
     *
     * @param types the types
     * @return the array of {@link Arg}
     */
    static Arg[] fromTypes(Type[] types) {
        Utils.requireNonNull(types);
        return fromTypes(null, types);
    }

    private static Arg[] fromTypes(Type previous, Type[] types) {
        var args = new Arg[types.length];
        for (int i = 0; i < types.length; i++) {
            var type = types[i];
            args[i] = type.equals(previous) ? ClassType.of(Object.class) : fromType(previous, type);
        }
        return args;
    }

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

}
