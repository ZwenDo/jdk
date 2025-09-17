package java.util.ptype;

import java.util.ptype.util.Utils;

/// Simple mapping of a super type to its descriptor.
public final class SuperTypeMapping {

    private final Class<?> superType;
    private final ClassDescriptor superTypeDescriptor;

    /// Creates a new super type mapping.
    ///
    /// @param superType the super type
    /// @param superTypeDescriptor the super type descriptor
    public SuperTypeMapping(Class<?> superType, ClassDescriptor superTypeDescriptor) {
        Utils.requireNonNull(superType);
        Utils.requireNonNull(superTypeDescriptor);
        this.superType = superType;
        this.superTypeDescriptor = superTypeDescriptor;
    }

    /// Returns the super type.
    ///
    /// @return the super type
    public Class<?> superType() {
        return superType;
    }

    /// Returns the super type descriptor.
    ///
    /// @return the super type descriptor
    public ClassDescriptor superTypeDescriptor() {
        return superTypeDescriptor;
    }

    @Override
    public int hashCode() {
        return superType.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof SuperTypeMapping other)) {
            return false;
        }
        return superType.equals(other.superType);
    }

}
