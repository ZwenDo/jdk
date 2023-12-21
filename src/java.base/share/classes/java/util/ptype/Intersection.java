package java.util.ptype;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Represents an intersection type.
 */
public non-sealed interface Intersection extends Arg {

    /**
     * Gets the bounds of this intersection type.
     *
     * @return the bounds
     */
    List<Arg> bounds();

    /**
     * Creates an {@link Intersection} from the given varargs of bounds.
     *
     * @param bounds the bounds
     * @return the {@link Intersection}
     */
    static Intersection of(Arg... bounds) {
        Objects.requireNonNull(bounds);
        return of(Arrays.asList(bounds));
    }

    /**
     * Creates an {@link Intersection} from the given list of bounds.
     *
     * @param bounds the bounds
     * @return the {@link Intersection}
     */
    static Intersection of(List<Arg> bounds) {
        Objects.requireNonNull(bounds);
        if (bounds.size() < 2) {
            throw new IllegalArgumentException("bounds.size() < 2");
        }
        // TODO check that first bound is a class and that all other bounds are interfaces + bounds are distinct
        return new Intersection() {
            private final List<Arg> boundsCopy = List.copyOf(bounds);

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                var index = 0;
                for (var bound : boundsCopy) {
                    builder.append(bound.toString());
                    index++;
                    if (index < boundsCopy.size()) {
                        builder.append(" & ");
                    }
                }
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return IterableUtils.allMatch(boundsCopy, (bound) -> bound.isAssignable(actual));
            }

            @Override
            public List<Arg> bounds() {
                return boundsCopy;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
