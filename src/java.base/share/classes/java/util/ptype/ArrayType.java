package java.util.ptype;

/**
 * Represents an array type.
 */
public non-sealed interface ArrayType extends Arg {

    /**
     * Gets the component type of this array type.
     *
     * @return the component type
     */
    Arg componentType();

    /**
     * Creates an {@link ArrayType} from the given component type.
     *
     * @param componentTypeArgs the component type
     * @return the {@link ArrayType}
     */
    static ArrayType of(Arg componentTypeArgs) {
        Utils.requireNonNull(componentTypeArgs);
        return new ArrayType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                componentTypeArgs.appendTo(builder);
                builder.append("[]");
            }

            @Override
            public boolean isAssignable(Arg actual, Variance variance) {
                Utils.requireNonNull(actual);
                Utils.requireNonNull(variance);
                if (actual instanceof ArrayType arrayType) {
                    return componentTypeArgs.isAssignable(arrayType.componentType(), Variance.INVARIANT);
                } else if (actual instanceof ClassType) {
                    return false;
                } else if (actual instanceof InnerClassType) {
                    return false;
                } else if (actual instanceof Intersection) {
                    return false;
                } else if (actual instanceof ParameterizedType) {
                    return false;
                } else if (actual instanceof RawType) {
                    return false;
                } else if (actual instanceof Wildcard) {
                    return false;
                }
                throw new IllegalArgumentException();
            }

            @Override
            public Arg componentType() {
                return componentTypeArgs;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
