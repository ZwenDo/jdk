package java.util.ptype;

/// Interface implemented by all types involved in a generic hierarchy.
@PrototypeInternal
public interface ClassDescriptorHolder {

    /// Gets the descriptor of this object.
    ///
    /// @return the descriptor
    DerivedClassDescriptor $descriptor();

}
