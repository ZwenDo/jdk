package java.util.ptype.model;


import jdk.internal.vm.annotation.Stable;

import java.util.ptype.Internal;
import java.util.ptype.SpecializedTypeUtils;
import java.util.ptype.util.ArrayList;
import java.util.ptype.util.HashMap;
import java.util.ptype.util.Utils;

/// Represents a class type.
public final class ClassDescriptor implements SpecializedTypeDescriptor {

    @Stable
    private final ClassDescriptor outer;

    @Stable
    private final Class<?> type;

    @Stable
    private final ArrayList<SpecializedTypeDescriptor> typeArguments;

    /// We always set the highest bit for stable
    ///
    /// 0x0000_0001 raw
    /// 0x0000_0010 has outer
    @Stable
    private final byte flags;

    @Stable
    private HashMap<Class<?>, SpecializedTypeDescriptor> superTypes = null;

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type the type
    /// @param typeArguments the type arguments
    public ClassDescriptor(ClassDescriptor outer, Class<?> type, SpecializedTypeDescriptor... typeArguments) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(typeArguments);
        this.outer = outer;
        this.type = type;
        var flags = DEFAULT;
        if (outer != null) flags |= HAS_OUTER;
        if (isRawArray(typeArguments)) flags |= IS_RAW;
        this.typeArguments = ArrayList.of(typeArguments);
        this.flags = flags;
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type the type as a string.
    /// @param typeArguments the type arguments
    public ClassDescriptor(ClassDescriptor outer, String type, SpecializedTypeDescriptor... typeArguments) {
        this(outer, Utils.findClassByName(type), typeArguments);
    }

    /// Gets the outer type if it exists.
    ///
    /// @return the outer type
    public ClassDescriptor outer() {
        return hasOuter() ? outer : null;
    }

    /// Gets the type
    ///
    /// @return the type
    public Class<?> type() {
        return type;
    }

    /// Gets the type arguments.
    ///
    /// @return the type arguments
    public ArrayList<SpecializedTypeDescriptor> typeArguments() {
        return typeArguments;
    }

    /// Whether this parameterized type represents a raw type.
    ///
    /// @return true if this parameterized type represents a rawtype; false otherwise.
    public boolean isRaw() {
        return (flags & IS_RAW) != 0;
    }

    /// Sees the current descriptor as one of its super type.
    ///
    /// @param type the super type
    /// @return the current descriptor as one of its super types
    public SpecializedTypeDescriptor asSuper(Class<?> type) {
        if (superTypes == null) {
            superTypes = Internal.generateSuperTypes(this.type, this);
        }
        return superTypes.get(type);
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

    private boolean hasOuter() {
        return (flags & HAS_OUTER) != 0;
    }

    private static boolean isRawArray(SpecializedTypeDescriptor[] typeArguments) {
        if (typeArguments.length == 0) return false;
        var isRaw = typeArguments[0] == ErasedType.instance();
        for (var typeArgument : typeArguments) {
            Utils.requireNonNull(typeArgument);
            if (typeArgument == ErasedType.instance() != isRaw) {
                throw new IllegalArgumentException("Cannot create a partially erazed parameterized type.");
            }
        }
        return isRaw;
    }

    private static final byte IS_RAW = 0b0000_0001;

    private static final byte HAS_OUTER = 0b0000_0010;

    private static final byte DEFAULT = (byte) 0b1000_0000;

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
