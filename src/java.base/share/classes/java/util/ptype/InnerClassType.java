package java.util.ptype;

/**
 * Represents an inner class type.
 */
public non-sealed interface InnerClassType extends Arg {

    /**
     * Gets the outer type of this inner class type.
     *
     * @return the outer type
     */
    Arg outerType();

    /**
     * Gets the inner type of this inner class type.
     *
     * @return the inner type
     */
    Arg innerType();

    /**
     * Creates an {@link InnerClassType} from the given outer and inner types.
     *
     * @param outerType the outer type
     * @param innerType the inner type
     * @return the {@link InnerClassType}
     */
    static InnerClassType of(Arg outerType, Arg innerType) {
        Utils.requireNonNull(outerType);
        Utils.requireNonNull(innerType);
        return new InnerClassType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                outerType.appendTo(builder);
                builder.append(".");
                innerType.appendTo(builder);
            }

            @Override
            public boolean isAssignable(Arg actual, Variance variance) {
                Utils.requireNonNull(actual);
                Utils.requireNonNull(variance);
                if (actual instanceof InnerClassType innerClassType) {
                    return outerType.isAssignable(innerClassType.outerType(), variance) &&
                           innerType.isAssignable(innerClassType.innerType(), variance);
                } else if (actual instanceof ClassType) {
                    return false;
                } else if (actual instanceof Intersection) {
                    return false;
                } else if (actual instanceof ParameterizedType) {
                    return false;
                } else if (actual instanceof RawType) {
                    return false;
                } else if (actual instanceof Wildcard) {
                    return false;
                } else if (actual instanceof ArrayType) {
                    return false;
                }
                throw new IllegalArgumentException();
            }

            @Override
            public Arg outerType() {
                return outerType;
            }

            @Override
            public Arg innerType() {
                return innerType;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
