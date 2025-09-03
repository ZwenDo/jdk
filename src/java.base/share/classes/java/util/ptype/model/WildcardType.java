package java.util.ptype.model;

import java.util.ptype.util.ArrayList;
import java.util.ptype.util.Utils;

/// Represents a wildcard type.
public final class WildcardType {

    private final boolean isSuper;

    private final ArrayList<SpecializedType> bounds;

    /// Creates a new Wildcard.
    ///
    /// @param isSuper whether this wildcard is a `? super`
    /// @param bounds the bounds of this wildcard
    public WildcardType(boolean isSuper, SpecializedType... bounds) {
        Utils.requireNonNull(bounds);
        this.isSuper = isSuper;
        this.bounds = ArrayList.of(bounds);
    }

    /// Whether the wildcard is super.
    ///
    /// @return true if it's a super wildcard, false otherwise
    public boolean isSuper() {
        return isSuper;
    }

    /// Gets the super bound.
    ///
    /// @return the super bound
    public ArrayList<SpecializedType> superBound() {
        return isSuper ? bounds : ArrayList.of();
    }

    /// Gets the extends bound.
    ///
    /// @return the extends bound.
    public ArrayList<SpecializedType> extendsBound() {
        return isSuper ? ArrayList.of() : bounds;
    }

//    private static Wildcard of(ArgList upperBound, ArgList lowerBound) {
//        return new Wildcard() {
//
//            @Override
//            public void appendTo(StringBuilder builder) {
//                Utils.requireNonNull(builder);
//                builder.append("?");
//                if (lowerBound.isEmpty()) {
//                    appendBounds(builder, " extends ", upperBound);
//                } else {
//                    appendBounds(builder, " super ", lowerBound);
//                }
//            }
//
//            @Override
//            public boolean isAssignable(Arg actual, Variance variance) {
//                Utils.requireNonNull(actual);
//                return upperBound.allMatch(new ArgList.ArgPredicate() {
//                    @Override
//                    public boolean test(Arg bound) {
//                        return bound.isAssignable(actual, Variance.COVARIANT);
//                    }
//                }) &&
//                       lowerBound.allMatch(new ArgList.ArgPredicate() {
//                           @Override
//                           public boolean test(Arg bound) {
//                               return bound.isAssignable(actual, Variance.CONTRAVARIANT);
//                           }
//                       });
//            }
//
//            @Override
//            public ArgList upperBound() {
//                return upperBound;
//            }
//
//            @Override
//            public ArgList lowerBound() {
//                return lowerBound;
//            }
//
//            @Override
//            public String toString() {
//                return Arg.toString(this);
//            }
//
//            private static void appendBounds(StringBuilder builder, String prefix, ArgList bounds) {
//                builder.append(prefix);
//                bounds.forEachIndexed(Utils.appendListLambda(builder, bounds.size(), " & "));
//            }
//
//        };
//    }

}
