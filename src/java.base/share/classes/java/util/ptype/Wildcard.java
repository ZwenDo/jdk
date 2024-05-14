package java.util.ptype;

import java.util.Arrays;
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
     * Creates a {@link Wildcard} representing the given upper bound.
     *
     * @param upperBound the upper bound
     * @return the {@link Wildcard}
     */
    static Wildcard ofUpper(Arg... upperBound) {
        Objects.requireNonNull(upperBound);
        if (upperBound.length == 0) {
            throw new IllegalArgumentException("upperBound.isEmpty()");
        }
        return ofUpper(Arrays.asList(upperBound));
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

    /**
     * Creates a {@link Wildcard} representing the given lower bound.
     *
     * @param lowerBound the lower bound
     * @return the {@link Wildcard}
     */
    static Wildcard ofLower(Arg... lowerBound) {
        Objects.requireNonNull(lowerBound);
        if (lowerBound.length == 0) {
            throw new IllegalArgumentException("lowerBound.isEmpty()");
        }
        return ofLower(Arrays.asList(lowerBound));
    }

    /**
     * Represents a recursive wildcard type. It is used to build a wildcard type using itself in its bounds.
     */
    final class RecursiveWildcard implements Wildcard {
        private Wildcard inner;

        /**
         * Creates a recursive wildcard.
         */
        public RecursiveWildcard() {
        }

        @Override
        public boolean isAssignable(Arg actual) {
            return checkInner().isAssignable(actual);
        }

        @Override
        public void appendTo(StringBuilder builder) {
            checkInner().appendTo(builder);
        }

        @Override
        public List<Arg> upperBound() {
            return checkInner().upperBound();
        }

        @Override
        public List<Arg> lowerBound() {
            return checkInner().lowerBound();
        }

        /**
         * Sets the inner wildcard.
         *
         * @param inner the inner wildcard
         */
        public void setInner(Wildcard inner) {
            Objects.requireNonNull(inner);
            if (this.inner != null) {
                throw new IllegalStateException("inner already set");
            }
            this.inner = inner;
        }

        private Wildcard checkInner() {
            if (inner == null) {
                throw new IllegalArgumentException("Inner not set");
            }
            return inner;
        }

    }

    private static Wildcard of(List<Arg> upperBound, List<Arg> lowerBound) {
        return new Wildcard() {

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                builder.append("?");
                if (lowerBound.isEmpty()) {
                    appendBounds(builder, " extends ", upperBound);
                } else {
                    appendBounds(builder, " super ", lowerBound);
                }
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return upperBound.stream().allMatch((bound) -> bound.isAssignable(actual)) &&
                    lowerBound.stream().allMatch((bound) -> bound.isAssignable(actual));
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

            private static void appendBounds(StringBuilder builder, String prefix, List<Arg> bounds) {
                builder.append(prefix);
                var index = 0;
                for (var bound : bounds) {
                    bound.appendTo(builder);
                    index++;
                    if (index < bounds.size()) {
                        builder.append(" & ");
                    }
                }
            }

        };
    }

}
