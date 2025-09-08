package java.util.ptype.model;


import jdk.internal.vm.annotation.Stable;

import java.util.ptype.SpecializedTypeUtils;
import java.util.ptype.util.Utils;

/// Represents a class type.
public final class ClassType extends ConcreteSpecializedType implements SpecializedType {

    @Stable
    private final Class<?> type;

    /// Gets the type
    ///
    /// @return the type
    public Class<?> type() {
        return type;
    }

    /// Creates a new [ClassType].
    ///
    /// @param type the type
    public ClassType(Class<?> type) {
        Utils.requireNonNull(type);
        this.type = type;
    }

    /// Creates a new [ClassType].
    ///
    /// @param type the type as a string.
    public ClassType(String type) {
        Utils.requireNonNull(type);
        this.type = Utils.findClassByName(type);
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

//    public void appendToBuilder(StringBuilder builder) {
//        builder.append(type.getSimpleName());
//    }

//    /**
//     * Creates a {@link ClassType} from the given {@link Class}.
//     *
//     * @param type the {@link Class}
//     * @return the {@link ClassType}
//     */
//    static ClassType of(Class<?> type) {
//        Utils.requireNonNull(type);
//        return new ClassType() {
//
//            @Override
//            public void appendTo(StringBuilder builder) {
//                Utils.requireNonNull(builder);
//                builder.append(type.getSimpleName());
//            }
//
//            @Override
//            public boolean isAssignable(Arg actual, Variance variance) {
//                Utils.requireNonNull(actual);
//                Utils.requireNonNull(variance);
//                if (variance == Variance.INVARIANT) { // invariant
//                    return actual instanceof ClassType classType && type.equals(classType.type());
//                }
//
//                if (actual instanceof ClassType classType) {
//                    return compareClass(classType.type(), variance);
//                } else if (actual instanceof Intersection intersection) {
//                    return intersection.bounds().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
//                } else if (actual instanceof Wildcard wildcard) {
//                    if (variance == Variance.COVARIANT) {
//                        return wildcard.upperBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
//                    } else if (variance == Variance.CONTRAVARIANT) {
//                        return wildcard.lowerBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
//                    }
//                    throw new IllegalArgumentException();
//                } else if (actual instanceof RawType rawType) {
//                    return compareClass(rawType.type(), variance);
//                } else if (actual instanceof ArrayType) {
//                    return false;
//                } else if (actual instanceof InnerClassType innerClassType) {
//                    return isAssignable(innerClassType.innerType(), variance);
//                } else if (actual instanceof ParameterizedType parameterizedType) {
//                    return compareClass(parameterizedType.rawType(), variance);
//                }
//                throw new IllegalArgumentException();
//            }
//
//            private boolean compareClass(Class<?> clazz, Variance variance) {
//                if (variance == Variance.INVARIANT) {
//                    throw new AssertionError("Should not reach here");
//                } else if (variance == Variance.COVARIANT) {
//                    return type.isAssignableFrom(clazz);
//                } else if (variance == Variance.CONTRAVARIANT) {
//                    return clazz.isAssignableFrom(type);
//                }
//                throw new IllegalArgumentException();
//            }
//
//            @Override
//            public Class<?> type() {
//                return type;
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
