package java.util.ptype.model;


import jdk.internal.vm.annotation.Stable;

import java.util.ptype.SpecializedTypeUtils;
import java.util.ptype.util.ArrayList;
import java.util.ptype.util.Utils;

/// Represents a parameterized type.
public final class ParameterizedType extends ConcreteSpecializedType implements SpecializedType {

    @Stable
    private final Class<?> rawType;

    @Stable
    private final ArrayList<SpecializedType> typeArguments;

    @Stable
    private final boolean isRaw;

    /// Creates a new [ParameterizedType].
    ///
    /// @param rawType the raw type
    /// @param typeArguments the type arguments
    public ParameterizedType(Class<?> rawType, SpecializedType... typeArguments) {
        Utils.requireNonNull(rawType);
        Utils.requireNonNull(typeArguments);
        if (typeArguments.length == 0) {
            throw new IllegalArgumentException("Cannot create a parameterized type without type arguments");
        }
        this.isRaw = isRaw(typeArguments);
        this.rawType = rawType;
        this.typeArguments = ArrayList.of(typeArguments);
    }

    /// Creates a new [ParameterizedType].
    ///
    /// @param rawType the raw type as a string
    /// @param typeArguments the type arguments
    public ParameterizedType(String rawType, SpecializedType... typeArguments) {
        this(Utils.findClassByName(Utils.requireNonNull(rawType)), typeArguments);
    }

    /// Gets the raw type.
    ///
    /// @return the raw type
    public Class<?> rawType() {
        return rawType;
    }

    /// Gets the type arguments.
    ///
    /// @return the type arguments
    public ArrayList<SpecializedType> typeArguments() {
        return typeArguments;
    }

    /// Whether this parameterized type represents a raw type.
    ///
    /// @return true if this parameterized type represents a rawtype; false otherwise.
    public boolean isRaw() {
        return isRaw;
    }

    private static boolean isRaw(SpecializedType[] typeArguments) {
        var isRaw = typeArguments[0] == ErasedType.instance();
        for (var typeArgument : typeArguments) {
            if (typeArgument == ErasedType.instance() != isRaw) {
                throw new IllegalArgumentException("Cannot create a partially erazed parameterized type.");
            }
        }
        return isRaw;
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }


    //    public boolean isAssignable(Arg actual, Variance variance) {
//        Utils.requireNonNull(actual);
//        Utils.requireNonNull(variance);
//        if (variance == Variance.INVARIANT) {
//            // TODO ignore arguments ?
//            return (actual instanceof RawType rt && type.equals(rt.type()))
//                    || (actual instanceof ParameterizedType parameterizedType && type.equals(parameterizedType.rawType())
//                    && compareArguments(parameterizedType));
//        }
//        if (actual instanceof ParameterizedType parameterizedType) {
//            return compareClass(
//                    parameterizedType.rawType(),
//                    variance
//            ) && compareArguments(parameterizedType);
//            // TODO ignore arguments ?
//        } else if (actual instanceof RawType rt) {
//            return compareClass(rt.type(), variance);
//        } else if (actual instanceof Wildcard wildcard) {
//            if (variance == Variance.COVARIANT) {
//                return wildcard.upperBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
//            } else if (variance == Variance.CONTRAVARIANT) {
//                return wildcard.lowerBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
//            }
//            throw new IllegalArgumentException();
//        } else if (actual instanceof Intersection intersection) {
//            return intersection.bounds().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
//        } else if (actual instanceof InnerClassType innerClassType) {
//            return isAssignable(innerClassType.innerType(), variance);
//        } else if (actual instanceof ClassType classType) {
//            return compareClass(classType.type(), variance)
//                    && Internal.staticArgs(classType.type()).anyMatch(Utils.isAssignableLambdaExpected(
//                    this,
//                    Variance.INVARIANT
//            ));
//        } else if (actual instanceof ArrayType) {
//            return false;
//        }
//        throw new IllegalArgumentException();

//    }

}
