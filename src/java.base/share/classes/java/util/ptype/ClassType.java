package java.util.ptype;

import java.util.Objects;

/**
 * Represents a class type.
 */
public non-sealed interface ClassType extends Arg {

    /**
     * Gets the {@link Class} of this type.
     *
     * @return the {@link Class}
     */
    Class<?> type();

    /**
     * Creates a {@link ClassType} from the given {@link Class}.
     *
     * @param type the {@link Class}
     * @return the {@link ClassType}
     */
    static ClassType of(Class<?> type) {
        Objects.requireNonNull(type);
        return new ClassType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                builder.append(type.getSimpleName());
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return switch (actual) {
                    case ClassType classType -> type.isAssignableFrom(classType.type());
                    case ParameterizedType parameterizedType -> type.isAssignableFrom(parameterizedType.rawType());
                    case RawType rawType -> type.isAssignableFrom(rawType.rawArg().rawType());
                    case InnerClassType innerClassType -> isAssignable(innerClassType.innerType()); // recurse on inner type
                    case Intersection intersection -> IterableUtils.anyMatch(
                        intersection.bounds(),
                        this::isAssignable
                    );
                    case Wildcard wildcard -> IterableUtils.anyMatch(
                        wildcard.upperBound(),
                        this::isAssignable
                    );
                    case ArrayType a -> false;
                };
            }

            @Override
            public Class<?> type() {
                return type;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
