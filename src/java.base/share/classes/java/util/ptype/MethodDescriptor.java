package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.util.Objects;
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
    public SpecializedTypeDescriptor typeArgument(int index) {
        return isRaw() ? ErasedType.instance() : arguments.get(index);
    }

    /// Whether this parameterized type represents a raw type.
    ///
    /// @return true if this parameterized type represents a rawtype; false otherwise.
    public boolean isRaw() {
        return isRaw == 1;
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

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof MethodDescriptor that)) return false;
        return isRaw == that.isRaw && Objects.equals(arguments, that.arguments);
    }

    @Override
    public int hashCode() {
        var result = Objects.hashCode(arguments);
        result = 31 * result + isRaw;
        return result;
    }

    /// Gets the raw method descriptor instance.
    ///
    /// @return the instance
    public static MethodDescriptor rawInstance() {
        return RAW;
    }

    private static final MethodDescriptor RAW = new MethodDescriptor();

}
