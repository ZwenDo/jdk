package java.util.ptype;

/// Represents a type that contains [TypeDescriptor] that can be accessed through an index.
public sealed interface TypeDescriptorAccessor permits ClassDescriptor, MethodDescriptor {

    /// Get the type argument at the n-th position
    ///
    /// @param index the index
    /// @return the type argument
    TypeDescriptor typeArgument(int index);

}
