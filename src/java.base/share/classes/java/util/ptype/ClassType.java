package java.util.ptype;


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
        Utils.requireNonNull(type);
        return new ClassType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                builder.append(type.getSimpleName());
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Utils.requireNonNull(actual);
                return switch (actual) {
                    case ClassType classType -> type.equals(classType.type());
                    case Intersection intersection -> intersection.bounds()
                        .anyMatch(this::isAssignable);
                    case Wildcard wildcard -> wildcard.upperBound()
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
