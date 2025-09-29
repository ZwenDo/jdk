package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

/// Descriptor representing a hidden class.
@PrototypeInternal
public final class HiddenClassDescriptor implements DerivedClassDescriptor {

    //region fields

    @Stable
    private final ClassDescriptor[] directSuperTypes;

    @Stable
    HashMap<Class<?>, ClassDescriptor> superTypes;

    //endregion

    //region instantiation

    private HiddenClassDescriptor(ClassDescriptor[] directSuperTypes) {
        this.directSuperTypes = directSuperTypes;
    }

    /// Creates a new [HiddenClassDescriptor].
    ///
    /// @param arg1 the first direct super type
    /// @return the created descriptor
    public static HiddenClassDescriptor of(ClassDescriptor arg1) {
        Utils.requireNonNull(arg1);
        return new HiddenClassDescriptor(new ClassDescriptor[]{arg1});
    }

    /// Creates a new [HiddenClassDescriptor].
    ///
    /// @param arg1 the first direct super type
    /// @param arg2 the second direct super type
    /// @return the created descriptor
    public static HiddenClassDescriptor of(ClassDescriptor arg1, ClassDescriptor arg2) {
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        return new HiddenClassDescriptor(new ClassDescriptor[]{arg1, arg2});
    }

    /// Creates a new [HiddenClassDescriptor].
    ///
    /// @param arg1 the first direct super type
    /// @param arg2 the second direct super type
    /// @param arg3 the third direct super type
    /// @return the created descriptor
    public static HiddenClassDescriptor of(ClassDescriptor arg1, ClassDescriptor arg2, ClassDescriptor arg3) {
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        return new HiddenClassDescriptor(new ClassDescriptor[]{arg1, arg2, arg3});
    }

    /// Creates a new [HiddenClassDescriptor].
    ///
    /// @param directSuperTypes the direct super types
    /// @return the created descriptor
    public static HiddenClassDescriptor of(ClassDescriptor[] directSuperTypes) {
        Utils.requireNonNull(directSuperTypes);
        var copy = new ClassDescriptor[directSuperTypes.length];
        for (var i = 0; i < directSuperTypes.length; i++) {
            copy[i] = Utils.requireNonNull(directSuperTypes[i]);
        }
        return new HiddenClassDescriptor(copy);
    }

    //endregion

    @Override
    public ClassDescriptor viewAsSuper(Class<?> type) {
        Utils.requireNonNull(type);
        if (superTypes == null) {
            for (var directSuperType : directSuperTypes) {
                if (type.equals(directSuperType.type())) {
                    return directSuperType;
                }
            }
            superTypes = SuperDescriptorComputing.buildSuperMap(this);
        }
        return superTypes.get(type);
    }

    void forEachDirectSuperType(Consumer<? super ClassDescriptor> action) {
        Utils.requireNonNull(action);
        for (var directSuperType : directSuperTypes) {
            action.accept(directSuperType);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof HiddenClassDescriptor other)) return false;
        return Utils.arrayEquals(directSuperTypes, other.directSuperTypes);
    }

    @Override
    public int hashCode() {
        return Utils.arrayHashCode(directSuperTypes);
    }

}
