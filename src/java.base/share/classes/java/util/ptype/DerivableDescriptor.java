package java.util.ptype;

import java.util.Optional;

/// A descriptor that can be involved in a generic type hierarchy.
public sealed interface DerivableDescriptor permits ClassDescriptor, LambdaDescriptor {

    /// Sees the current descriptor as one of its super type. If this descriptor hasn't a representation for `type`,
    /// this method will return [empty][Optional#empty].
    ///
    /// @param type the super type
    /// @return the current descriptor as one of its super types or [empty][Optional#empty]
    Optional<ClassDescriptor> asSuper(Class<?> type);

}
