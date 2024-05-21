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
     * @param actual the actual type
     * @return {@code true} if this type is assignable from the given type
     */
    boolean isAssignable(Arg actual);

    /**
     * Append this type to the given string builder.
     *
     * @param builder the string builder
     */
    void appendTo(StringBuilder builder);

    /**
     * Creates an {@link Arg} from the given {@link Type}.
     *
     * @param type the type
     * @return the {@link Arg}
     */
    static Arg fromType(Type type) {
        Utils.requireNonNull(type);
        return switch (type) {
            case Class<?> clazz -> ClassType.of(clazz);
            case java.lang.reflect.ParameterizedType parameterizedType -> ParameterizedType.of(
                (Class<?>) parameterizedType.getRawType(),
                fromTypes(parameterizedType.getActualTypeArguments())
            );
            case WildcardType wildcardType -> {
                if (wildcardType.getLowerBounds().length == 0) { // no lower bound
                    yield Wildcard.ofUpper(fromTypes(wildcardType.getUpperBounds()));
                } else {
                    yield Wildcard.ofLower(fromTypes(wildcardType.getLowerBounds()));
                }
            }
            case GenericArrayType genericArrayType -> ArrayType.of(
                Arg.fromType(genericArrayType.getGenericComponentType())
            );
            case TypeVariable<?> typeVariable -> {
                if (typeVariable.getBounds().length == 1) { // single bound
                    yield Arg.fromType(typeVariable.getBounds()[0]);
                }
                // intersection
                yield Intersection.of(fromTypes(typeVariable.getBounds()));
            }
            default -> throw new IllegalArgumentException("Unknown type: " + type + " (" + type.getClass() + ")");
        };
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
            args[i] = Arg.fromType(types[i]);
        }
        return args;
    }

}
