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
                    case ClassType classType -> type.equals(classType.type());
                    case Intersection intersection -> intersection.bounds()
                        .stream()
                        .anyMatch(this::isAssignable);
                    case Wildcard wildcard -> wildcard.upperBound()
                        .stream()
                        .anyMatch(this::isAssignable);
                    case RawType ignored -> false;
                    case ArrayType ignored -> false;
                    case InnerClassType ignored -> false;
                    case ParameterizedType ignored -> false;
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
