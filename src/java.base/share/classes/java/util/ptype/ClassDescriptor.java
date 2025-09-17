package java.util.ptype;


import jdk.internal.vm.annotation.Stable;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
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
    /// 0x0000_0001 raw \
    /// 0x0000_0010 has outer
    @Stable
    private final byte flags;

    @Stable
    private HashMap<Class<?>, ClassDescriptor> superTypes = null;

    @Stable
    private Type javaType;

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type the type
    /// @param isRaw whether the type is a raw type
    /// @param typeArguments the type arguments
    public ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            boolean isRaw,
            SpecializedTypeDescriptor... typeArguments
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(typeArguments);
        var flags = DEFAULT;
        if (outer != null) flags |= HAS_OUTER;
        ArrayList<SpecializedTypeDescriptor> typeArgs = null;
        if (isRaw) {
            if (typeArguments.length != 0) throw new IllegalArgumentException("No type arguments should be present for raw " + type.getSimpleName() + ", but " + Arrays.toString(typeArguments) + " were present.");
            flags |= IS_RAW;
        } else {
            typeArgs = ArrayList.of(typeArguments);
            if (partiallyRawArray(typeArguments)) flags |= PARTIALLY_RAW;
        }
        this.outer = outer;
        this.type = type;
        this.typeArguments = typeArgs;
        this.flags = flags;
    }

    /// Gets the outer type if it exists.
    ///
    /// @return the outer type
    public Optional<ClassDescriptor> outer() {
        return hasOuter() ? Optional.of(outer) : Optional.empty();
    }

    /// Gets the type
    ///
    /// @return the type
    public Class<?> type() {
        return type;
    }

    /// Get the type argument at the n-th position
    ///
    /// @param index the index
    /// @return the type argument
    public SpecializedTypeDescriptor typeArgument(int index) {
        return isRaw() ? ErasedType.instance() : typeArguments.get(index);
    }

    /// Sees the current descriptor as one of its super type.
    ///
    /// @param type the super type
    /// @return the current descriptor as one of its super types
    public ClassDescriptor asSuper(Class<?> type) {
        if (type == this.type) return this;
        if (superTypes == null) {
            superTypes = Internal.generateSuperTypes(this.type, this);
        }
        return superTypes.get(type);
    }

    @Override
    public Type asType() {
        if (javaType != null) return javaType;
        if (isRaw()) {
            javaType = type;
            return javaType;
        }
        var outer = hasOuter() ? this.outer.asType() : null;

        // basic class
        if ((outer == null || outer instanceof Class<?>) && typeArguments.isEmpty()) {
            javaType = type;
            return javaType;
        }

        var arguments = new Type[typeArguments.size()];
        for (var i = 0; i < typeArguments.size(); i++) {
            arguments[i] = typeArguments.get(i).asType();
        }

        javaType = ParameterizedTypeImpl.make(type, arguments, outer);
        return javaType;
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ClassDescriptor that)) return false;
        return type.equals(that.type) && Objects.equals(typeArguments, that.typeArguments) && Objects.equals(outer, that.outer);
    }

    @Override
    public int hashCode() {
        var hash = 1;
        hash = 31 * hash + type.hashCode();
        hash = 31 * hash + (outer != null ? outer.hashCode() : 0);
        hash = 31 * hash + (typeArguments != null ? typeArguments.hashCode() : 0);
        return hash;
    }

    boolean isRaw() {
        return (flags & IS_RAW) != 0;
    }

    boolean partiallyRaw() {
        return (flags & PARTIALLY_RAW) != 0;
    }

    ArrayList<SpecializedTypeDescriptor> typeArguments() {
        return typeArguments;
    }
    /// Gets the outer type if it exists.
    ///
    /// @return the outer type
    public ClassDescriptor $outer() {
        return hasOuter() ? outer : null;
    }

    private boolean hasOuter() {
        return (flags & HAS_OUTER) != 0;
    }

    private static boolean partiallyRawArray(SpecializedTypeDescriptor[] typeArguments) {
        for (var typeArg : typeArguments) {
            if (typeArg == ErasedType.instance() || typeArg instanceof ClassDescriptor cd && (cd.partiallyRaw() || cd.isRaw())) {
                return true;
            }
        }
        return false;
    }

    private static final byte IS_RAW = 1;

    private static final byte HAS_OUTER = 1 << 1;

    private static final byte PARTIALLY_RAW = 1 << 2;

    private static final byte DEFAULT = (byte) 0b1000_0000;

}
