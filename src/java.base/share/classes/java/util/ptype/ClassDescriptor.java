package java.util.ptype;


import jdk.internal.vm.annotation.Stable;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.ptype.util.HashMap;
import java.util.ptype.util.Utils;

/// Represents a class type.
public final class ClassDescriptor implements SpecializedTypeDescriptor, DerivableDescriptor {

    @Stable
    private final ClassDescriptor outer;

    @Stable
    private final Class<?> type;

    @Stable
    private final SpecializedTypeDescriptor[] typeArguments;

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

    private ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            byte flags,
            SpecializedTypeDescriptor[] typeArguments
    ) {
        this.outer = outer;
        this.type = type;
        this.typeArguments = typeArguments;
        this.flags = flags;
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type  the type
    /// @param isRaw whether the type is raw or a regular class
    public ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            boolean isRaw
    ) {
        Utils.requireNonNull(type);
        var flags = DEFAULT;
        if (isRaw) flags |= IS_RAW;
        if (outer != null) {
            flags |= HAS_OUTER;
            if (outer.partiallyRaw()) flags |= PARTIALLY_RAW;
        }
        this(outer, type, flags, EMPTY_ARRAY);
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer         the outer class
    /// @param type          the type
    /// @param typeArguments the type arguments
    public ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            SpecializedTypeDescriptor... typeArguments
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(typeArguments);
        var flags = DEFAULT;
        if (outer != null) flags |= HAS_OUTER;
        var array = new SpecializedTypeDescriptor[typeArguments.length];
        System.arraycopy(typeArguments, 0, array, 0, typeArguments.length);
        if ((outer != null && outer.partiallyRaw()) || partiallyRawArray(typeArguments)) {
            flags |= PARTIALLY_RAW;
        }
        this(outer, type, flags, array);
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type  the type
    /// @param arg1  the first type argument
    public ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            SpecializedTypeDescriptor arg1
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        var flags = DEFAULT;
        if (outer != null) flags |= HAS_OUTER;

        if ((outer != null && outer.partiallyRaw()) || SpecializedTypeUtils.partiallyRaw(arg1)) {
            flags |= PARTIALLY_RAW;
        }
        this(outer, type, flags, new SpecializedTypeDescriptor[]{arg1});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type  the type
    /// @param arg1  the first type argument
    /// @param arg2  the second type argument
    public ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            SpecializedTypeDescriptor arg1,
            SpecializedTypeDescriptor arg2
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        var flags = DEFAULT;
        if (outer != null) flags |= HAS_OUTER;

        if (
                (outer != null && outer.partiallyRaw())
                        || SpecializedTypeUtils.partiallyRaw(arg1)
                        || SpecializedTypeUtils.partiallyRaw(arg2)
        ) {
            flags |= PARTIALLY_RAW;
        }

        this(outer, type, flags, new SpecializedTypeDescriptor[]{arg1, arg2});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param outer the outer class
    /// @param type  the type
    /// @param arg1  the first type argument
    /// @param arg2  the second type argument
    /// @param arg3  the third type argument
    public ClassDescriptor(
            ClassDescriptor outer,
            Class<?> type,
            SpecializedTypeDescriptor arg1,
            SpecializedTypeDescriptor arg2,
            SpecializedTypeDescriptor arg3
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        var flags = DEFAULT;
        if (outer != null) flags |= HAS_OUTER;

        if (
                (outer != null && outer.partiallyRaw())
                        || SpecializedTypeUtils.partiallyRaw(arg1)
                        || SpecializedTypeUtils.partiallyRaw(arg2)
                        || SpecializedTypeUtils.partiallyRaw(arg3)
        ) {
            flags |= PARTIALLY_RAW;
        }

        this(outer, type, flags, new SpecializedTypeDescriptor[]{arg1, arg2, arg3});
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
        if (isRaw()) {
            return ErasedType.instance();
        }
        Objects.checkIndex(index, typeArguments.length);
        if (!hasTypeArguments()) {
            throw new IllegalArgumentException("Type " + type + " is not parameterized.");
        }
        return typeArguments[index];
    }

    @Override
    public Optional<ClassDescriptor> asSuper(Class<?> type) {
        Utils.requireNonNull(type);
        return Optional.ofNullable($asSuper(type));
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
        if ((outer == null || outer instanceof Class<?>) && typeArguments.length == 0) {
            javaType = type;
            return javaType;
        }

        var arguments = new Type[typeArguments.length];
        for (var i = 0; i < typeArguments.length; i++) {
            arguments[i] = typeArguments[i].asType();
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
        return type.equals(that.type) && Arrays.equals(typeArguments, that.typeArguments) && Objects.equals(outer, that.outer);
    }

    @Override
    public int hashCode() {
        var hash = 1;
        hash = 31 * hash + type.hashCode();
        hash = 31 * hash + (outer != null ? outer.hashCode() : 0);
        hash = 31 * hash + Arrays.hashCode(typeArguments);
        return hash;
    }

    boolean isRaw() {
        return (flags & IS_RAW) != 0;
    }

    boolean partiallyRaw() {
        return (flags & (PARTIALLY_RAW | IS_RAW)) != 0;
    }

    boolean hasTypeArguments() {
        return !(isRaw() || typeArguments.length == 0);
    }

    /// Gets the outer type if it exists.
    ///
    /// @return the outer type
    public ClassDescriptor $outer() {
        return hasOuter() ? outer : null;
    }

    /// Sees the current descriptor as one of its super type. If this descriptor hasn't a representation for `type`,
    /// this method will return null.
    ///
    /// @param type the super type
    /// @return the current descriptor as one of its super types or null
    public ClassDescriptor $asSuper(Class<?> type) {
        Utils.requireNonNull(type);
        if (type == this.type) return this;
        if (superTypes == null) {
            superTypes = Internal.generateSuperTypes(this.type, this);
        }
        return superTypes.get(type);
    }

    void joinArguments(StringBuilder builder) {
        SpecializedTypeUtils.joinSpecializedTypeArray(builder, typeArguments);
    }

    private boolean hasOuter() {
        return (flags & HAS_OUTER) != 0;
    }

    private static boolean partiallyRawArray(SpecializedTypeDescriptor[] typeArguments) {
        for (var typeArg : typeArguments) {
            if (SpecializedTypeUtils.partiallyRaw(typeArg)) {
                return true;
            }
        }
        return false;
    }

    private static boolean partiallyRaw(ClassDescriptor descriptor) {
        return descriptor != null && descriptor.partiallyRaw();
    }

    private static final byte IS_RAW = 1;

    private static final byte HAS_OUTER = 1 << 1;

    private static final byte PARTIALLY_RAW = 1 << 2;

    private static final byte DEFAULT = (byte) 0b1000_0000;

    private static final SpecializedTypeDescriptor[] EMPTY_ARRAY = new SpecializedTypeDescriptor[0];

}
