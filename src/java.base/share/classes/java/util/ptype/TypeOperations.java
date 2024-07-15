package java.util.ptype;

import jdk.internal.misc.VM;
import jdk.internal.org.objectweb.asm.Handle;
import sun.reflect.generics.tree.SimpleClassTypeSignature;

import java.lang.invoke.CallSite;
import java.lang.invoke.LambdaMetafactory;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MutableCallSite;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;
import java.util.concurrent.locks.ReentrantLock;

import static java.lang.invoke.MethodType.methodType;

/**
 * Utility methods for working with {@link Arg}s.
 */
public final class TypeOperations {

    private static final StringHashSet REPORTED_CAST_LOCATIONS = new StringHashSet();

    private static final class LockHolder {
        private static final ReentrantLock LOCK = new ReentrantLock();
    }

    /**
     * Performs a cast to the given type. This cast ensures that parameterized types are cast correctly.
     *
     * @param obj      the object to cast
     * @param expected the expected type
     * @param location the location at which the cast has been performed
     * @return the cast object
     */
    public static Object checkCast(Object obj, Arg expected, String location) {
        Utils.requireNonNull(expected);
        Utils.requireNonNull(location);
        if (!VM.isBooted()) return obj;

        if (obj == null) {
            return null;
        }

        if (!isInstance(obj, expected)) {
            try {
                LockHolder.LOCK.lock();
                if (REPORTED_CAST_LOCATIONS.add(location)) {
                    System.out.println(message(obj, expected, location));
                }
            } finally {
                LockHolder.LOCK.unlock();
            }
        }

        return obj;
    }

    private static boolean isInstance(Object obj, Arg expected) {
        // var cast = (A<String>.B<Integer>) obj;
        if (expected instanceof InnerClassType innerClassType) {
            if (!isInstance(obj, innerClassType.innerType())) { // check inner type
                return false;
            }
            var outer = Internal.outerThis(obj);
            if (outer.isPresent()) {
                return isInstance(outer.get(), innerClassType.outerType());
            } else { // by default if no outer type is specified, yield true
                return true;
            }
            // var cast = (String) obj;
        } else if (expected instanceof ClassType classType) {
            return classType.type().isAssignableFrom(obj.getClass());
            // var cast = (List) obj;
        } else if (expected instanceof RawType rawType) {
            return validate(obj, expected, rawType.type());
            // var cast = (List<String>) obj;
        } else if (expected instanceof ParameterizedType parameterizedType) {
            return validate(obj, expected, parameterizedType.rawType());
            // var cast = (Runnable & Serializable) obj;
        } else if (expected instanceof Intersection intersection) {
            return intersection.bounds().allMatch(new ArgList.ArgPredicate() {
                @Override
                public boolean test(Arg bound) {
                    return isInstance(obj, bound);
                }
            });
            // var cast = (List<String>[]) obj;
        } else if (expected instanceof ArrayType arrayType) {
            if (!obj.getClass().isArray()) {
                return false;
            }
            var arg = Internal.arrayType(obj);
            if (arg.isPresent()) {
                return arrayType.isAssignable(arg.get(), Arg.Variance.COVARIANT);
            } else {
                return true;
            }
            // Note that this kind of cast should not be possible
            // var cast = (? extends String) obj;
            // var cast = (? super String) obj;
            // var cast = (?) obj;    equivalent to (? extends Object)
        } else if (expected instanceof Wildcard wildcard) {
            var objClass = obj.getClass();
            var opt = Internal.argHandle(objClass).arg(obj, objClass);
            if (opt.isEmpty()) {
                return true;
            }
            var actualArg = opt.get();
            return wildcard.upperBound().allMatch(Utils.isAssignableLambdaActual(actualArg, Arg.Variance.COVARIANT)) &&
                wildcard.lowerBound().allMatch(Utils.isAssignableLambdaActual(actualArg, Arg.Variance.CONTRAVARIANT));
        }
        throw new IllegalArgumentException();
    }

    private static boolean validate(Object obj, Arg expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var opt = Internal.argHandle(objClass).arg(obj, supertype);
        if (opt.isEmpty()) {
            return supertype.isAssignableFrom(objClass);
        }
        return expected.isAssignable(opt.get(), Arg.Variance.COVARIANT);
    }

    private static String message(Object obj, Arg expected, String location) {
        var objClass = obj.getClass();
        if (objClass.isAnonymousClass()) {
            var interfaces = objClass.getInterfaces();
            objClass = interfaces.length > 0 ? interfaces[0] : objClass.getSuperclass();
        }

        var actualLocation = location.substring(location.indexOf('!') + 1);
        var builder = new StringBuilder(actualLocation)
            .append(": ");

        var arg = Internal.argHandle(objClass)
            .arg(obj, objClass);
        if (arg.isPresent()) {
            arg.get().appendTo(builder);
        } else {
            builder.append(objClass.getName());
        }

        builder.append(" to ");
        expected.appendTo(builder);
        return builder.toString();
    }

    private TypeOperations() {
        throw new AssertionError();
    }

}
