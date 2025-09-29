package java.util.ptype;

/// Class handling type argument propagation through method calls.
public final class TypeDescriptorPassingHandler {

    /// Passed to any regular method call.
    private MethodDescriptor methodDescriptor;

    /// Specialized type passed to constructor.
    private ClassDescriptor constructorDescriptor;

    /// Descriptor passed by hidden classes to their lambda implementation.
    private ClassDescriptor lambdaDescriptor;

    /// Descriptor passed to hidden classes constructors.
    private HiddenClassDescriptor hiddenClassDescriptor;

    /// Pushes the type argument to the stack.
    ///
    /// @param arg    the type argument to push
    public static void pushMethod(MethodDescriptor arg) {
        var instance = instance();
        instance.methodDescriptor = arg;
    }

    /// Returns the type arguments for the current method and resets the associated field.
    ///
    /// @return the type arguments for the current method
    public static MethodDescriptor methodTypeArguments() {
        var instance = instance();
        var args = instance.methodDescriptor;
        instance.methodDescriptor = null;
        return args;
    }

    /// Pushes the type to the stack before a constructor call.
    ///
    /// @param arg the argument to push
    public static void pushConstructor(ClassDescriptor arg) {
        var instance = instance();
        instance.constructorDescriptor = arg;
    }

    /// Returns the argument for the current constructor call and resets the associated field.
    ///
    /// @return the argument for the current constructor call
    public static ClassDescriptor constructorTypeArguments() {
        var instance = instance();
        var args = instance.constructorDescriptor;
        instance.constructorDescriptor = null;
        return args;
    }

    /// Pushes the type to the stack before a lambda calls its impl method.
    /// @param arg the argument to push
    public static void pushLambda(ClassDescriptor arg) {
        var instance = instance();
        instance.lambdaDescriptor = arg;
    }

    /// Returns the argument for the current lambda impl method call and resets the associated field.
    ///
    /// @return the argument for the current lambda impl method call.
    public static ClassDescriptor lambdaTypeArguments() {
        var instance = instance();
        var args = instance.lambdaDescriptor;
        instance.lambdaDescriptor = null;
        return args;
    }

    /// Pushes the type to the stack before a hidden class constructor call.
    ///
    /// @param arg the argument to push
    public static void pushHiddenClass(HiddenClassDescriptor arg) {
        var instance = instance();
        instance.hiddenClassDescriptor = arg;
    }

    /// Returns the argument for the current hidden class constructor call.
    ///
    /// @return the argument for the current hidden class constructor call.
    public static HiddenClassDescriptor hiddenClassTypeArguments() {
        var instance = instance();
        var args = instance.hiddenClassDescriptor;
        instance.hiddenClassDescriptor = null;
        return args;
    }


    private static TypeDescriptorPassingHandler instance() {
        return Thread.currentThread().stpHandler();
    }

    /// Creates a new instance.
    public TypeDescriptorPassingHandler() {
    }

}
