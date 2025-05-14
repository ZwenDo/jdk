package java.util.ptype;

/**
 * Represents an intersection type.
 */
public non-sealed interface Intersection extends Arg {

    /**
     * Gets the bounds of this intersection type.
     *
     * @return the bounds
     */
    ArgList bounds();

    /**
     * Creates an {@link Intersection} from the given varargs of bounds.
     *
     * @param bounds the bounds
     * @return the {@link Intersection}
     */
    static Intersection of(Arg... bounds) {
        Utils.requireNonNull(bounds);
        return of(ArgList.of(bounds));
    }

    /**
     * Creates an {@link Intersection} from the given list of bounds.
     *
     * @param bounds the bounds
     * @return the {@link Intersection}
     */
    static Intersection of(ArgList bounds) {
        Utils.requireNonNull(bounds);
        if (bounds.size() < 2) {
            throw new IllegalArgumentException("bounds.size() < 2");
        }
        // TODO check that first bound is a class and that all other bounds are interfaces + bounds are distinct
        return new Intersection() {

            @Override
            public boolean isAssignable(Arg actual, Variance variance) {
                Utils.requireNonNull(actual);
                Utils.requireNonNull(variance);
                // TODO unsure
                return bounds.allMatch(new ArgList.ArgPredicate() {
                    @Override
                    public boolean test(Arg bound) {
                        return bound.isAssignable(actual, Variance.COVARIANT);
                    }
                });
            }

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                bounds.forEachIndexed(Utils.appendListLambda(builder, bounds.size(), " & "));
            }

            @Override
            public ArgList bounds() {
                return bounds;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
