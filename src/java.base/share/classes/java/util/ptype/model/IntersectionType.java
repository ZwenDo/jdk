package java.util.ptype.model;

import java.util.ptype.util.ArrayList;
import java.util.ptype.util.Utils;

/// Represents an intersection type.
public final class IntersectionType {

    private final ArrayList<SpecializedType> bounds;

    /// Creates a new intersection.
    ///
    /// @param bounds the bounds of the intersection
    public IntersectionType(SpecializedType... bounds) {
        this.bounds = ArrayList.of(Utils.requireNonNull(bounds));
    }

    /// Gets the bounds of this intersection type.
    ///
    /// @return the bounds
    public ArrayList<SpecializedType> bounds() {
        return bounds;
    }

//    /**
//     * Creates an {@link Intersection} from the given list of bounds.
//     *
//     * @param bounds the bounds
//     * @return the {@link Intersection}
//     */
//    static Intersection of(ArgList bounds) {
//        java.util.ptype.util.Utils.requireNonNull(bounds);
//        if (bounds.size() < 2) {
//            throw new IllegalArgumentException("bounds.size() < 2");
//        }
//        // TODO check that first bound is a class and that all other bounds are interfaces + bounds are distinct
//        return new Intersection() {
//
//            @Override
//            public boolean isAssignable(Arg actual, Variance variance) {
//                java.util.ptype.util.Utils.requireNonNull(actual);
//                java.util.ptype.util.Utils.requireNonNull(variance);
//                // TODO unsure
//                return bounds.allMatch(new ArgList.ArgPredicate() {
//                    @Override
//                    public boolean test(Arg bound) {
//                        return bound.isAssignable(actual, Variance.COVARIANT);
//                    }
//                });
//            }
//
//            @Override
//            public void appendTo(StringBuilder builder) {
//                java.util.ptype.util.Utils.requireNonNull(builder);
//                bounds.forEachIndexed(java.util.ptype.util.Utils.appendListLambda(builder, bounds.size(), " & "));
//            }
//
//            @Override
//            public ArgList bounds() {
//                return bounds;
//            }
//
//            @Override
//            public String toString() {
//                return Arg.toString(this);
//            }
//
//        };
//    }

}
