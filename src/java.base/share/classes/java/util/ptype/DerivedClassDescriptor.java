package java.util.ptype;

/// Represents a descriptor that derives one or more [ClassDescriptor].
@PrototypeInternal
public interface DerivedClassDescriptor {

    /// Sees the current descriptor as one of its super type. If this descriptor hasn't any representation for `type`,
    /// this method should throw.
    ///
    /// @param type the super type
    /// @return the current descriptor
    ClassDescriptor viewAsSuper(Class<?> type);

}
