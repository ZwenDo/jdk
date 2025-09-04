package java.util.ptype;

import java.util.ptype.model.SpecializedMethodTypeArguments;
import java.util.ptype.model.SpecializedType;
import java.util.ptype.util.Utils;

/// Class handling type argument propagation through method calls.
public final class SpecializedTypePassingHandler {

    /// Passed to any regular method call.
    private SpecializedMethodTypeArguments passedMethodTypeArgs;

    /// Specialized type passed to constructor.
    private SpecializedType constructorTypeArgs;

    /// Field used to identify the class who pushed the argument.
    private Class<?> caller;

    private static final StackWalker WALKER = null;
//        StackWalker.getInstance(
//        Set.of(
//            StackWalker.Option.RETAIN_CLASS_REFERENCE,
//            StackWalker.Option.DROP_METHOD_INFO
//        )
//    );

    /// Utility method to simplify AST modification by the compiler when we need to push type arguments before a method
    /// call.
    ///
    /// @param e1       the first expression
    /// @param e2       the second expression
    /// @param baseExpression the base expression that will use the type arguments
    /// @param <E>            the type of the base expression
    /// @return the result of the base expression
    public static <E> E pre(Void e1, Void e2, E baseExpression) {
        return baseExpression;
    }

    /// Utility method to simplify AST modification by the compiler when we need to push type arguments after a method
    /// call.
    ///
    /// @param baseExpression the base expression that will use the type arguments
    /// @param e1       the first expression
    /// @param e2       the second expression
    /// @param <E>            the type of the base expression
    /// @return the result of the base expression
    public static <E> E post(E baseExpression, Void e1, Void e2) {
        return baseExpression;
    }

    /// Returns the type arguments for the current method. This method accepts a parameter representing the actualCaller
    /// of the current method. If the passed `actualCaller` is not null, this method verifies that the actual caller
    /// is the same class as the one stored. If they are different, null is returned instead.
    ///
    /// @param actualCaller the caller of the current method, or null if we don't need to know the caller.
    /// @return the type arguments for the current method
    public static SpecializedMethodTypeArguments methodTypeArguments(Class<?> actualCaller) {
        var instance = instance();
        var args = instance.passedMethodTypeArgs;
        instance.passedMethodTypeArgs = null;
        if (instance.caller == null) {
            return args;
        }
        return instance.caller == actualCaller ? args : null;
    }

    /// Returns the current expected caller.
    ///
    /// @return the current expected caller
    public static Class<?> methodCaller() {
        var instance = instance();
        return instance.caller;
    }

    /// Returns the argument for the current constructor call.
    ///
    /// @return the argument for the current constructor call
    public static SpecializedType constructorTypeArguments() {
        var instance = instance();
        var args = instance.constructorTypeArgs;
        instance.constructorTypeArgs = null;
        return args;
    }

    /// Pushes the type argument to the stack. Also pushes the class that pushed the argument.
    ///
    /// @param arg    the type argument to push
    /// @param caller the expected caller. It will be used when retrieving the arg for comparison
    /// @return null
    public static Void pushMethod(SpecializedMethodTypeArguments arg, Class<?> caller) {
        var instance = instance();
        instance.passedMethodTypeArgs = arg;
        instance.caller = caller;
        return null;
    }

    /// Pops the method type arguments.
    public static void popMethodTypeArguments() {
        var instance = instance();
        instance.passedMethodTypeArgs = null;
    }

    /// Pushes the type to the stack before a constructor call.
    ///
    /// @param arg the argument to push
    /// @return null
    public static Void pushConstructor(SpecializedType arg) {
        var instance = instance();
        instance.constructorTypeArgs = arg;
        return null;
    }

    private static SpecializedTypePassingHandler instance() {
        return Thread.currentThread().stpHandler();
    }

    /// Get the stack walker.
    ///
    /// @return the stack walker
    public static StackWalker walker() {
        return WALKER;
    }

    /// Creates a new instance.
    public SpecializedTypePassingHandler() {
    }

}
