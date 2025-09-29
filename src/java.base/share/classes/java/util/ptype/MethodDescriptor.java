package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

/// Represents the type arguments of a method.
public final class MethodDescriptor implements TypeDescriptorAccessor {

    @Stable
    private final TypeDescriptor[] arguments;

    private MethodDescriptor(TypeDescriptor[] arguments) {
        Utils.requireNonNull(arguments);
        this.arguments = arguments;
    }

    /// Creates a new instance.
    ///
    /// @return the created descriptor
    @PrototypeInternal
    public static MethodDescriptor of() {
        return RAW;
    }

    /// Creates a new instance.
    ///
    /// @param arg1 the first argument
    /// @return the created descriptor
    @PrototypeInternal
    public static MethodDescriptor of(TypeDescriptor arg1) {
        Utils.requireNonNull(arg1);
        return new MethodDescriptor(new TypeDescriptor[]{arg1});
    }

    /// Creates a new instance.
    ///
    /// @param arg1 the first argument
    /// @param arg2 the second argument
    /// @return the created instance
    @PrototypeInternal
    public static MethodDescriptor of(TypeDescriptor arg1, TypeDescriptor arg2) {
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        return new MethodDescriptor(new TypeDescriptor[] {arg1, arg2});
    }

    /// Creates a new instance.
    ///
    /// @param arg1 the first argument
    /// @param arg2 the second argument
    /// @param arg3 the third argument
    /// @return the created instance
    @PrototypeInternal
    public static MethodDescriptor of(TypeDescriptor arg1, TypeDescriptor arg2, TypeDescriptor arg3) {
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        return new MethodDescriptor(new TypeDescriptor[] {arg1, arg2, arg3});
    }

    /// Creates a new instance.
    ///
    /// @param args the arguments
    /// @return the created instance
    @PrototypeInternal
    public static MethodDescriptor of(TypeDescriptor... args) {
        Utils.requireNonNull(args);
        var copy = new TypeDescriptor[args.length];
        for (int i = 0; i < args.length; i++) {
            copy[i] = Utils.requireNonNull(args[i]);
        }
        return new MethodDescriptor(copy);
    }

    @Override
    public TypeDescriptor typeArgument(int index) {
        if (isRaw()) return ErasedClassDescriptor.instance();
        Utils.checkIndex(index, arguments.length);
        return arguments[index];
    }

    boolean isRaw() {
        return arguments.length == 0;
    }

    @Override
    public String toString() {
        if (isRaw()) return "<*raw*>";
        var builder = new StringBuilder();
        builder.append("<");
        for (var argument : arguments) {
            TypeDescriptorUtils.stringify(builder, argument);
        }
        builder.append(">");
        return builder.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof MethodDescriptor that)) return false;
        return Utils.arrayEquals(arguments, that.arguments);
    }

    @Override
    public int hashCode() {
        return Utils.arrayHashCode(arguments);
    }

    private static final MethodDescriptor RAW = new MethodDescriptor(new TypeDescriptor[0]);

}
