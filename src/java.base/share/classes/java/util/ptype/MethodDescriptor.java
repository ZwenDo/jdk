package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.util.Objects;
import java.util.Optional;
import java.util.ptype.util.ArrayList;
import java.util.ptype.util.Utils;

/// Represents the type arguments of a method.
public final class MethodDescriptor {

    @Stable
    private final ArrayList<SpecializedTypeDescriptor> arguments;

    ///  1 -> true \
    /// -1 -> false
    @Stable
    private final byte isRaw;

    /// Creates a new instance.
    ///
    /// @param arguments the arguments of the method
    public MethodDescriptor(SpecializedTypeDescriptor... arguments) {
        Utils.requireNonNull(arguments);
        if (arguments.length == 0) {
            throw new IllegalArgumentException("Cannot create a specialized method type arguments instance without type arguments");
        }
        this.isRaw = -1;
        this.arguments = ArrayList.of(arguments);
    }

    private MethodDescriptor() {
        this.isRaw = 1;
        this.arguments = null;
    }

    /// Gets the n-th specialized type.
    ///
    /// @param index the index of the specialized type.
    /// @return the found type
    public Optional<SpecializedTypeDescriptor> typeArgument(int index) {
        var result = arguments.get(index);
        // TODO we might need to manually push the type arguments for the optional if we want to instrument the jdk
        return result == ErasedType.instance() ? Optional.empty() : Optional.of(result);
    }

    /// Whether this parameterized type represents a raw type.
    ///
    /// @return true if this parameterized type represents a rawtype; false otherwise.
    public boolean isRaw() {
        return isRaw == 1;
    }

    /// Gets the n-th specialized type.
    ///
    /// @param index the index of the specialized type.
    /// @return the found type
    public SpecializedTypeDescriptor $typeArgument(int index) {
        if (isRaw()) return ErasedType.instance();
        Objects.checkIndex(index, arguments.size());
        return arguments.get(index);
    }

    @Override
    public String toString() {
        if (isRaw()) return "<*raw*>";
        var builder = new StringBuilder();
        builder.append("<");
        arguments.joinTo(builder, SpecializedTypeUtils::appendToBuilder, ", ");
        builder.append(">");
        return builder.toString();
    }

    /// Gets the raw method descriptor instance.
    ///
    /// @return the instance
    public static MethodDescriptor rawInstance() {
        return RAW;
    }

    private static final MethodDescriptor RAW = new MethodDescriptor();

}
