package java.util.ptype;

import java.util.EnumSet;

/// Class handling type argument propagation through method calls.
public final class SpecializedTypePassingHandler {

    /// Passed to any regular method call.
    private MethodDescriptor passedMethodTypeArgs;

    /// Specialized type passed to constructor.
    private SpecializedTypeDescriptor constructorTypeArgs;

    /// Descriptor passed by hidden classes to their lambda implementation.
    private SpecializedTypeDescriptor lambdaPassedDescriptor;

    /// Field used to identify the class who pushed the argument.
    private Class<?> caller;

    private static final class Holder {

        private static final StackWalker WALKER = StackWalker.getInstance(
                EnumSet.of(StackWalker.Option.RETAIN_CLASS_REFERENCE, StackWalker.Option.DROP_METHOD_INFO),
                1
        );

    }

    /// Returns the current expected caller.
    ///
    /// @return the current expected caller
    public static Class<?> methodCaller() {
        var instance = instance();
        return instance.caller;
    }

    /// Returns the type arguments for the current method. This method accepts a parameter representing the actualCaller
    /// of the current method. If the passed `actualCaller` is not null, this method verifies that the actual caller
    /// is the same class as the one stored. If they are different, null is returned instead.
    ///
    /// @param actualCaller the caller of the current method, or null if we don't need to know the caller.
    /// @return the type arguments for the current method
    public static MethodDescriptor methodTypeArguments(Class<?> actualCaller) {
        var instance = instance();
        var args = instance.passedMethodTypeArgs;
        instance.passedMethodTypeArgs = null;
        if (instance.caller == null) {
            return args;
        }
        return instance.caller == actualCaller ? args : null;
    }

    /// Returns the argument for the current constructor call.
    ///
    /// @return the argument for the current constructor call
    public static SpecializedTypeDescriptor constructorTypeArguments() {
        var instance = instance();
        var args = instance.constructorTypeArgs;
        instance.constructorTypeArgs = null;
        return args;
    }

    /// Returns the argument for the current lambda impl method call.
    ///
    /// @return the argument for the current lambda impl method call.
    public static SpecializedTypeDescriptor lambdaTypeArguments() {
        var instance = instance();
        var args = instance.lambdaPassedDescriptor;
        instance.lambdaPassedDescriptor = null;
        return args;
    }

    /// Pushes the type argument to the stack. Also pushes the class that pushed the argument.
    ///
    /// @param arg    the type argument to push
    /// @param caller the expected caller. It will be used when retrieving the arg for comparison
    public static void pushMethod(MethodDescriptor arg, Class<?> caller) {
        var instance = instance();
        instance.passedMethodTypeArgs = arg;
        instance.caller = caller;
    }

    /// Pushes the type to the stack before a constructor call.
    ///
    /// @param arg the argument to push
    public static void pushConstructor(SpecializedTypeDescriptor arg) {
        var instance = instance();
        instance.constructorTypeArgs = arg;
    }

    /// Pushes the type to the stack before a lambda calls its impl method.
    /// @param arg the argument to push
    public static void pushLambda(SpecializedTypeDescriptor arg) {
        var instance = instance();
        instance.lambdaPassedDescriptor = arg;
    }

    /// Pops the method type arguments.
    public static void popMethodTypeArguments() {
        var instance = instance();
        instance.passedMethodTypeArgs = null;
    }

    private static SpecializedTypePassingHandler instance() {
        return Thread.currentThread().stpHandler();
    }

    /// Get the stack walker.
    ///
    /// @return the stack walker
    public static StackWalker walker() {
        return Holder.WALKER;
    }

    /// Creates a new instance.
    public SpecializedTypePassingHandler() {
    }

}
