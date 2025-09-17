package java.util.ptype;

import java.lang.annotation.CompilerIntrinsic;
import java.lang.reflect.Type;
import java.util.Optional;
import java.util.ptype.util.Utils;

/// Supertype for all specialized types.
public sealed interface SpecializedTypeDescriptor permits ArrayDescriptor, ClassDescriptor, ErasedType {

    /// Returns the [Type] representation of this [SpecializedTypeDescriptor].
    ///
    /// @return the corresponding [Type]
    Type asType();

    /// Creates a [SpecializedTypeDescriptor] from the given [T] type.
    ///
    /// @return the corresponding [SpecializedTypeDescriptor]
    /// @param <T> the source type
    @CompilerIntrinsic
    static <T> Optional<SpecializedTypeDescriptor> of() {
        var m = SpecializedTypePassingHandler.methodTypeArguments(null);
        if (m == null || m.isRaw()) return Optional.empty();
        return Optional.of(m.typeArgument(0));
    }

    /// Gets the [SpecializedTypeDescriptor] from a given object.
    ///
    /// @param holder the object containing the
    /// @return the class descriptor
    @CompilerIntrinsic
    static Optional<ClassDescriptor> from(Object holder) {
        Utils.requireNonNull(holder);
        var internal = Internal.extractInformationField(holder);
        if (internal.isEmpty()) return Optional.empty();
        var value = internal.get();
        if (value.partiallyRaw()) return Optional.empty();
        return Optional.of(value);
    }

}
