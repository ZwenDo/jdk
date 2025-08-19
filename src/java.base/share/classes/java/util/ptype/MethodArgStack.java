package java.util.ptype;

import jdk.internal.misc.VM;
import jdk.internal.vm.annotation.Stable;

import java.util.function.Supplier;

/// Class handling type argument propagation through method calls.
public final class MethodArgStack {

    private MethodTypeArgs passedMethodTypeArgs;
    /// Field used to identify the class who pushed the argument.
    private Class<?> caller;

    /// This parameter represents the arg passed to the constructor. The constructor will add it itself to its type args
    private Arg passedArg;

    /// The constructor type args used to defer all the insertions.
    private ConstructorTypeArgs passedConstructorTypeArgs;

    private boolean enabled;

    /// This instance is used before the vm is initialized.
    private static final MethodArgStack DEFAULT = new MethodArgStack();

    private static final StackWalker WALKER = null;
//        StackWalker.getInstance(
//        Set.of(
//            StackWalker.Option.RETAIN_CLASS_REFERENCE,
//            StackWalker.Option.DROP_METHOD_INFO
//        )
//    );

    /// Utility method to simplify AST modification by the compiler when we need to push type arguments for a method
    /// call.
    ///
    /// @param prePush1       the code pushing the type arguments
    /// @param prePush2       the other code pushing the type arguments (usually the arg)
    /// @param baseExpression the base expression that will use the type arguments
    /// @param postPush1      the code to push after the call
    /// @param postPush2      the other code to push after the call
    /// @param <E>            the type of the base expression
    /// @return the result of the base expression
    public static <E> E e(Void prePush1, Void prePush2, E baseExpression, Void postPush1, Void postPush2) {
        return baseExpression;
    }

    /// Returns the type arguments for the current method. This method accepts a parameter representing the actualCaller
    /// of the current method. If the passed `actualCaller` is not null, this method verifies that the actual caller
    /// is the same class as the one stored. If they are different, null is returned instead.
    ///
    /// @param actualCaller the caller of the current method, or null if we don't need to know the caller.
    /// @return the type arguments for the current method
    public static MethodTypeArgs methodTypeArgs(Class<?> actualCaller) {
        var instance = instance();
        var args = instance.passedMethodTypeArgs;
        instance.passedMethodTypeArgs = null;
        if (actualCaller == null) {
            return args;
        }
        return instance.caller == actualCaller ? args : null;
    }

    /// Whether the stack is enabled.
    ///
    /// @return true if the stack is enabled, false otherwise
    public static boolean enabled() {
        var instance = instance();
        return instance.enabled;
    }

    /// Enable the stack.
    public static void enable() {
        var instance = instance();
        instance.enabled = true;
    }

    /// Disable the stack.
    public static void disable() {
        var instance = instance();
        instance.enabled = false;
    }

    /// Returns the argument for the current constructor call.
    ///
    /// @return the argument for the current constructor call
    public static Arg typeArgs() {
        var instance = instance();
        var args = instance.passedArg;
        instance.passedArg = null;
        return args;
    }

    /// Returns the constructor current arguments.
    ///
    /// @return the constructor current arguments
    public static ConstructorTypeArgs constructorTypeArgs() {
        var instance = instance();
        var args = instance.passedConstructorTypeArgs;
        instance.passedConstructorTypeArgs = null;
        return args;
    }

    /// Pushes the type argument to the stack. Also pushes the class that pushed the argument.
    ///
    /// @param arg    the type argument to push
    /// @param caller the expected caller. It will be used when retrieving the arg for comparison
    /// @return null
    public static Void push(MethodTypeArgs arg, Class<?> caller) {
        Utils.requireNonNull(arg);
        Utils.requireNonNull(caller);
        var instance = instance();
        instance.passedMethodTypeArgs = arg;
        instance.caller = caller;
        return null;
    }

    /// Pushes the argument to the stack before a constructor call (also pushes a new [ConstructorTypeArgs]).
    ///
    /// @param arg the argument to push
    /// @return null
    public static Void pushArg(Arg arg) {
        return pushArg(arg, new ConstructorTypeArgs());
    }

    /// Pushes the arguments to the stack before a super call.
    ///
    /// @param arg                 the argument to push
    /// @param constructorTypeArgs the constructor type args to push
    /// @return null
    public static Void pushArg(Arg arg, ConstructorTypeArgs constructorTypeArgs) {
        Utils.requireNonNull(arg);
        Utils.requireNonNull(constructorTypeArgs);
        var instance = instance();
        instance.passedArg = arg;
        instance.passedConstructorTypeArgs = constructorTypeArgs;
        return null;
    }

    /// Get the stack walker.
    ///
    /// @return the stack walker
    public static StackWalker walker() {
        return WALKER;
    }

    private static MethodArgStack instance() {
        if (!VM.isBooted() || useDefault) {
            return DEFAULT;
        }
        if (INSTANCES != null) {
            return INSTANCES.get();
        }
        synchronized (LOCK) {
            if (INSTANCES != null) {
                return INSTANCES.get();
            }
            useDefault = true;
            INSTANCES = ThreadLocal.withInitial(
                new Supplier<>() {
                    @Override
                    public MethodArgStack get() {
                        return new MethodArgStack();
                    }
                }
            );
            useDefault = false;
            return INSTANCES.get();
        }
    }

    private static final Object LOCK = new Object();

    private static volatile ThreadLocal<MethodArgStack> INSTANCES;
    private static volatile boolean useDefault = true;


    private MethodArgStack() {
    }

}
