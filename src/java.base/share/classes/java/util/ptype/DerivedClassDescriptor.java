package java.util.ptype;

/// Represents a descriptor that derives one or more [ClassDescriptor].
@PrototypeInternal
public interface DerivedClassDescriptor {

    /// Sees the current descriptor as one of its super type. If this descriptor hasn't any representation for `type`,
    /// this method will return null.
    ///
    /// @param type the super type
    /// @return the descriptor viewed as its super type or null
    ClassDescriptor viewAsSuper(Class<?> type);

}
