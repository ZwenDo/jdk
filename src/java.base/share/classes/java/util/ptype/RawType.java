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
    ParameterizedType rawArg();

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
        var parameters = type.getTypeParameters();
        if (parameters.length == 0) {
            throw new IllegalArgumentException("Class " + type + " is not a parameterized type");
        }
        var args = IterableUtils.map(parameters, Arg::fromType);
        return of(ParameterizedType.of(type, args));
    }

    /**
     * Creates a {@link RawType} from the given raw type.
     * <p>
     * This method is only used for testing purposes. RawType should be created statically by the compiler, so this
     * dynamic method should never be called.
     *
     * @param rawArg the raw type
     * @return the {@link RawType}
     */
    static RawType of(ParameterizedType rawArg) {
        Objects.requireNonNull(rawArg);
        return new RawType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                builder.append(rawArg.rawType().getSimpleName());
                builder.append("(raw type of: ");
                rawArg.appendTo(builder);
                builder.append(")");
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return rawArg.isAssignable(actual); // TODO check if it can be simplified for RawType and ParameterizedType if the class is the same
            }

            @Override
            public ParameterizedType rawArg() {
                return rawArg;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
