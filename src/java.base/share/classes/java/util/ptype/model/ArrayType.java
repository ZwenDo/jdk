package java.util.ptype.model;

import jdk.internal.vm.annotation.Stable;

import java.util.ptype.SpecializedTypeUtils;
import java.util.ptype.util.Utils;

/// Represents an array type.
public final class ArrayType implements SpecializedType {

    @Stable
    private final SpecializedType componentType;

    /// Creates a new array type.
    ///
    /// @param componentType the component type
    public ArrayType(SpecializedType componentType) {
        Utils.requireNonNull(componentType);
        this.componentType = componentType;
    }

    /// Gets the component type of this array type.
    ///
    /// @return the component type
    public SpecializedType componentType() {
        return componentType;
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

//    /**
//     * Creates an {@link ArrayType} from the given component type.
//     *
//     * @param componentTypeArgs the component type
//     * @return the {@link ArrayType}
//     */
//    static ArrayType of(Arg componentTypeArgs) {
//        java.util.ptype.util.Utils.requireNonNull(componentTypeArgs);
//        return new ArrayType() {
//
//            @Override
//            public void appendTo(StringBuilder builder) {
//                java.util.ptype.util.Utils.requireNonNull(builder);
//                componentTypeArgs.appendTo(builder);
//                builder.append("[]");
//            }
//
//            @Override
//            public boolean isAssignable(Arg actual, Arg.Variance variance) {
//                java.util.ptype.util.Utils.requireNonNull(actual);
//                java.util.ptype.util.Utils.requireNonNull(variance);
//                if (actual instanceof ArrayType arrayType) {
//                    return componentTypeArgs.isAssignable(arrayType.componentType(), Variance.INVARIANT);
//                } else if (actual instanceof ClassType) {
//                    return false;
//                } else if (actual instanceof InnerClassType) {
//                    return false;
//                } else if (actual instanceof Intersection) {
//                    return false;
//                } else if (actual instanceof ParameterizedType) {
//                    return false;
//                } else if (actual instanceof RawType) {
//                    return false;
//                } else if (actual instanceof Wildcard) {
//                    return false;
//                }
//                throw new IllegalArgumentException();
//            }
//
//            @Override
//            public Arg componentType() {
//                return componentTypeArgs;
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
