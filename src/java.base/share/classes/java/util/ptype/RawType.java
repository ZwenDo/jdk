package java.util.ptype;

import java.util.Objects;

/**
 * Represents a raw type.
 */
public non-sealed interface RawType extends Arg {

    /**
     * Gets the raw type of this raw type.
     *
     * @return the raw type
     */
    Class<?> type();

    /**
     * Creates a {@link RawType} from the given raw type.
     * <p>
     * This method is only used for testing purposes. RawType should be created statically by the compiler, so this
     * dynamic method should never be called.
     *
     * @param type the raw type
     * @return the {@link RawType}
     */
    static RawType of(Class<?> type) {
        Objects.requireNonNull(type);
        return new RawType() {
            @Override
            public Class<?> type() {
                return type;
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return switch (actual) {
                    case ParameterizedType parameterizedType -> type().equals(parameterizedType.rawType());
                    case RawType rawType -> type.equals(rawType.type());
                    case Intersection intersection -> intersection.bounds()
                        .stream()
                        .anyMatch(this::isAssignable);
                    case Wildcard wildcard -> wildcard.upperBound().stream().anyMatch(this::isAssignable);
                    case InnerClassType ignored -> false;
                    case ArrayType ignored -> false;
                    case ClassType ignored -> false;
                };
            }

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                builder.append(type.getSimpleName());
                builder.append("<raw type>");
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }
        };
    }

}
