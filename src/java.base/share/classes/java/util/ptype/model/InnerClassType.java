package java.util.ptype.model;

import jdk.internal.vm.annotation.Stable;

import java.util.ptype.SpecializedTypeUtils;
import java.util.ptype.util.Utils;

/// Represents an inner class type.
public final class InnerClassType implements SpecializedType {

    @Stable
    private final SpecializedType outer;

    @Stable
    private final SpecializedType inner;

    /// Creates a new inner class type.
    ///
    /// @param outer the outer type
    /// @param inner the inner type
    public InnerClassType(SpecializedType outer, SpecializedType inner) {
        Utils.requireNonNull(outer);
        Utils.requireNonNull(inner);
        this.outer = outer;
        this.inner = inner;
    }

    /// Gets the outer type
    ///
    /// @return the outer type
    public SpecializedType outerType() {
        return outer;
    }

    /// Gets the n-th nested type, 0 being the outermost type.
    ///
    /// @return the inner type
    public SpecializedType innerType() {
        return inner;
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

//    public void appendToBuilder(StringBuilder builder) {
//        outer.appendToBuilder(builder);
//        builder.append(".");
//        inner.appendToBuilder(builder);
//    }

//    /**
//     * Creates an {@link InnerClassType} from the given outer and inner types.
//     *
//     * @param outerType the outer type
//     * @param innerType the inner type
//     * @return the {@link InnerClassType}
//     */
//    static InnerClassType of(Arg outerType, Arg innerType) {
//        Utils.requireNonNull(outerType);
//        Utils.requireNonNull(innerType);
//        return new InnerClassType(, ) {
//
//            @Override
//            public void appendTo(StringBuilder builder) {
//                Utils.requireNonNull(builder);
//                outerType.appendTo(builder);
//                builder.append(".");
//                innerType.appendTo(builder);
//            }
//
//            @Override
//            public boolean isAssignable(Arg actual, Variance variance) {
//                Utils.requireNonNull(actual);
//                Utils.requireNonNull(variance);
//                if (actual instanceof InnerClassType innerClassType) {
//                    return outerType.isAssignable(innerClassType.outerType(), variance) &&
//                           innerType.isAssignable(innerClassType.innerType(), variance);
//                } else if (actual instanceof ClassType) {
//                    return false;
//                } else if (actual instanceof Intersection) {
//                    return false;
//                } else if (actual instanceof ParameterizedType) {
//                    return false;
//                } else if (actual instanceof RawType) {
//                    return false;
//                } else if (actual instanceof Wildcard) {
//                    return false;
//                } else if (actual instanceof ArrayType) {
//                    return false;
//                }
//                throw new IllegalArgumentException();
//            }
//
//            @Override
//            public Arg outerType() {
//                return outerType;
//            }
//
//            @Override
//            public Arg innerType() {
//                return innerType;
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
