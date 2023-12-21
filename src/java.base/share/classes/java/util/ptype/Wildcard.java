package java.util.ptype;

import java.util.List;
import java.util.Objects;

/**
 * Represents a wildcard type.
 */
public non-sealed interface Wildcard extends Arg {

    /**
     * Gets the upper bound of this wildcard type.
     *
     * @return the upper bound
     */
    List<Arg> upperBound();

    /**
     * Gets the lower bound of this wildcard type.
     *
     * @return the lower bound
     */
    List<Arg> lowerBound();

    /**
     * Creates a {@link Wildcard} representing the default bounds.
     *
     * @return the {@link Wildcard}
     */
    static Wildcard defaultBounds() {
        return ofUpper(List.of(ClassType.of(Object.class)));
    }

    /**
     * Creates a {@link Wildcard} representing the given upper bound.
     *
     * @param upperBound the upper bound
     * @return the {@link Wildcard}
     */
    static Wildcard ofUpper(List<Arg> upperBound) {
        Objects.requireNonNull(upperBound);
        if (upperBound.isEmpty()) {
            throw new IllegalArgumentException("upperBound.isEmpty()");
        }
        return of(List.copyOf(upperBound), List.of());
    }

    /**
     * Creates a {@link Wildcard} representing the given lower bound.
     *
     * @param lowerBound the lower bound
     * @return the {@link Wildcard}
     */
    static Wildcard ofLower(List<Arg> lowerBound) {
        Objects.requireNonNull(lowerBound);
        if (lowerBound.isEmpty()) {
            throw new IllegalArgumentException("lowerBound.isEmpty()");
        }
        return of(List.of(ClassType.of(Object.class)), List.copyOf(lowerBound));
    }

    private static Wildcard of(List<Arg> upperBound, List<Arg> lowerBound) {
        return new Wildcard() {

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                builder.append("?");
                if (lowerBound.isEmpty()) {
                    builder.append(" extends "); // TODO factorize branches + Intersection#appendTo
                    var index = 0;
                    for (var bound : upperBound) {
                        builder.append(bound.toString());
                        index++;
                        if (index < upperBound.size()) {
                            builder.append(" & ");
                        }
                    }
                } else {
                    builder.append(" super ");
                    var index = 0;
                    for (var bound : lowerBound) {
                        builder.append(bound.toString());
                        index++;
                        if (index < lowerBound.size()) {
                            builder.append(" & ");
                        }
                    }
                }
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return IterableUtils.allMatch(upperBound, (bound) -> bound.isAssignable(actual)) &&
                    IterableUtils.allMatch(lowerBound, (bound) -> bound.isAssignable(actual));
            }

            @Override
            public List<Arg> upperBound() {
                return upperBound;
            }

            @Override
            public List<Arg> lowerBound() {
                return lowerBound;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
