package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.util.Arrays;
import java.util.Objects;
import java.util.ptype.util.Utils;

/// Represents the type arguments of a method.
public final class MethodDescriptor {

    @Stable
    private final SpecializedTypeDescriptor[] arguments;

    ///  1 -> true \
    /// -1 -> false
    @Stable
    private final byte isRaw;

    /// Creates a new instance.
    ///
    /// @param arguments the arguments of the method
    public MethodDescriptor(SpecializedTypeDescriptor... arguments) {
        Utils.requireNonNull(arguments);
        this.isRaw = -1;
        if (arguments.length == 0) {
            throw new IllegalArgumentException("Cannot create a specialized method type arguments instance without type arguments");
        }
        var array = new SpecializedTypeDescriptor[arguments.length];
        System.arraycopy(arguments, 0, array, 0, arguments.length);
        this.arguments = array;
    }

    /// Creates a new instance.
    ///
    /// @param arg1 the first argument
    public MethodDescriptor(SpecializedTypeDescriptor arg1) {
        Utils.requireNonNull(arg1);
        this.isRaw = -1;
        this.arguments = new SpecializedTypeDescriptor[] {arg1};
    }

    /// Creates a new instance.
    ///
    /// @param arg1 the first argument
    /// @param arg2 the second argument
    public MethodDescriptor(SpecializedTypeDescriptor arg1, SpecializedTypeDescriptor arg2) {
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        this.isRaw = -1;
        this.arguments = new SpecializedTypeDescriptor[] {arg1, arg2};
    }

    /// Creates a new instance.
    ///
    /// @param arg1 the first argument
    /// @param arg2 the second argument
    /// @param arg3 the third argument
    public MethodDescriptor(SpecializedTypeDescriptor arg1, SpecializedTypeDescriptor arg2, SpecializedTypeDescriptor arg3) {
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        this.isRaw = -1;
        this.arguments = new SpecializedTypeDescriptor[] {arg1, arg2, arg3};
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
        Objects.checkIndex(index, arguments.length);
        return isRaw() ? ErasedType.instance() : arguments[index];
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
        SpecializedTypeUtils.joinSpecializedTypeArray(builder, arguments);
        builder.append(">");
        return builder.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof MethodDescriptor that)) return false;
        return isRaw == that.isRaw && Arrays.equals(arguments, that.arguments);
    }

    @Override
    public int hashCode() {
        var result = Arrays.hashCode(arguments);
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
