package java.util.ptype;

import java.util.Arrays;
import java.util.List;

/**
 * Represents a wildcard type.
 */
public non-sealed interface Wildcard extends Arg {

    /**
     * Gets the upper bound of this wildcard type.
     *
     * @return the upper bound
     */
    ArgList upperBound();

    /**
     * Gets the lower bound of this wildcard type.
     *
     * @return the lower bound
     */
    ArgList lowerBound();

    /**
     * Creates a {@link Wildcard} representing the given upper bound.
     *
     * @param upperBound the upper bound
     * @return the {@link Wildcard}
     */
    static Wildcard ofUpper(Arg... upperBound) {
        Utils.requireNonNull(upperBound);
        if (upperBound.length == 0) {
            throw new IllegalArgumentException("upperBound.isEmpty()");
        }
        return of(ArgList.of(upperBound), ArgList.of());
    }

    /**
     * Creates a {@link Wildcard} representing the given lower bound.
     *
     * @param lowerBound the lower bound
     * @return the {@link Wildcard}
     */
    static Wildcard ofLower(Arg... lowerBound) {
        Utils.requireNonNull(lowerBound);
        if (lowerBound.length == 0) {
            throw new IllegalArgumentException("lowerBound.isEmpty()");
        }
        return of(ArgList.of(), ArgList.of(lowerBound));
    }

    private static Wildcard of(ArgList upperBound, ArgList lowerBound) {
        return new Wildcard() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                builder.append("?");
                if (lowerBound.isEmpty()) {
                    appendBounds(builder, " extends ", upperBound);
                } else {
                    appendBounds(builder, " super ", lowerBound);
                }
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Utils.requireNonNull(actual);
                return upperBound.allMatch((bound) -> bound.isAssignable(actual)) &&
                       lowerBound.allMatch((bound) -> bound.isAssignable(actual));
            }

            @Override
            public ArgList upperBound() {
                return upperBound;
            }

            @Override
            public ArgList lowerBound() {
                return lowerBound;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

            private static void appendBounds(StringBuilder builder, String prefix, ArgList bounds) {
                builder.append(prefix);
                bounds.forEachIndexed((index, bound) -> {
                    bound.appendTo(builder);
                    if (index < bounds.size() - 1) {
                        builder.append(" & ");
                    }
                });
            }

        };
    }

}
