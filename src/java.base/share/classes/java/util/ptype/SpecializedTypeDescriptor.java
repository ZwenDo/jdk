package java.util.ptype;

import java.lang.annotation.CompilerIntrinsic;
import java.util.Optional;
import java.util.ptype.util.Utils;

/// Supertype for all specialized types.
public sealed interface SpecializedTypeDescriptor permits ArrayDescriptor, ClassDescriptor, ErasedType {

    /// Creates a [SpecializedTypeDescriptor] from the given [T] type.
    ///
    /// @return the corresponding [SpecializedTypeDescriptor]
    /// @param <T> the source type
    @CompilerIntrinsic
    static <T> Optional<SpecializedTypeDescriptor> of() {
        var m = SpecializedTypePassingHandler.methodTypeArguments(null);
        if (m == null) return Optional.empty();
        return m.typeArgument(0);
    }

    /// Gets the [SpecializedTypeDescriptor] from a given object.
    ///
    /// @param holder the object containing the
    /// @return the class descriptor
    @CompilerIntrinsic
    static Optional<ClassDescriptor> from(Object holder) {
        Utils.requireNonNull(holder);
        var internal = Internal.extractInformationField(holder);
        return internal.isPresent() ? Optional.of(internal.get()) : Optional.empty();
    }

}
