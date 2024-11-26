package java.util.ptype;

import jdk.internal.misc.VM;

import java.util.concurrent.locks.ReentrantLock;

/**
 * Utility methods for working with {@link Arg}s.
 */
public final class TypeOperations {

    private static final StringHashSet REPORTED_CAST_LOCATIONS = new StringHashSet();
    private static Mode mode = null;

    private static String mainId = null;
    private static boolean fromMain = false;

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
    public static Object checkCast(Object obj, Arg expected, String location, String kind, String target) {
        if (!VM.isBooted() || !fromMain) return obj;
        Utils.requireNonNull(expected);
        Utils.requireNonNull(location);
        Utils.requireNonNull(kind);
        Utils.requireNonNull(target);
        if (mode == null) {
            try {
                LockHolder.LOCK.lock();
                if (mode == null) {
                    var prop = System.getProperty("reification.mode");
                    if (prop != null) {
                        mode = Mode.fromString(prop);
                    } else {
                        mode = Mode.DISABLED;
                    }
                }
            } finally {
                LockHolder.LOCK.unlock();
            }
        }

        if (obj == null) return null;
        if (!shouldPerform(kind, target)) return obj;

        if (!isInstance(obj, expected)) {
            try {
                LockHolder.LOCK.lock();
                if (REPORTED_CAST_LOCATIONS.add(location)) {
                    var message = message(obj, expected, location, kind);
                    if (!"java.base/java.util.concurrent.ConcurrentHashMap$Node.<init>(ConcurrentHashMap.java:633): STORAGE jdk.internal.util.SoftReferenceKey to ReferenceKey<BaseLocale>".equals(message)) {
                        System.out.println(message);
                    }
                }
            } finally {
                LockHolder.LOCK.unlock();
            }
        }

        return obj;
    }

    public static void mainStartReached(String id) {
        Utils.requireNonNull(id);
        if (mainId != null) return;
        mainId = id;
        fromMain = true;
    }

    public static void mainEndReached(String id) {
        Utils.requireNonNull(mainId);
        if (mainId == null) throw new AssertionError();
        if (!mainId.equals(id)) return;
        fromMain = false;
    }

    private static boolean shouldPerform(String strKind, String strTarget) {
        var kind = CheckLocationKind.fromString(strKind);
        var target = CheckTarget.fromString(strTarget);
        switch (mode) {
            case Mode.DISABLED:
                return false;
            case Mode.FULL:
                return true;
            case Mode.MINIMAL:
                return kind == CheckLocationKind.STORAGE && target == CheckTarget.TYPE_PARAMETER;
            case Mode.NORMAL:
                return target == CheckTarget.TYPE_PARAMETER && (kind == CheckLocationKind.ENTRY || kind == CheckLocationKind.EXIT || kind == CheckLocationKind.STORAGE);
            default:
                throw new IllegalArgumentException();
        }
    }

    private static boolean isInstance(Object obj, Arg expected) {
        // var cast = (A<String>.B<Integer>) obj;
        if (expected instanceof InnerClassType innerClassType) {
            if (!isInstance(obj, innerClassType.innerType())) { // check inner type
                return false;
            }

            // we need to extract the expected inner class, because the actual object inner class might have an outer
            // this that does not extend the expected outer class (e.g. Attr.ResultInfo & Resolve.MethodResultInfo).
            Class<?> expectedInnerClass;
            var innerClass = innerClassType.innerType();
            if (innerClass instanceof ClassType outerClassType) {
                expectedInnerClass = outerClassType.type();
            } else if (innerClass instanceof ParameterizedType parameterizedType) {
                expectedInnerClass = parameterizedType.rawType();
            } else if (innerClass instanceof RawType rawType) {
                expectedInnerClass = rawType.type();
            } else {
                throw new AssertionError("Unexpected outer type: " + innerClassType.outerType());
            }

            var outer = Internal.outerThis(obj, expectedInnerClass);
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

    private static String message(Object obj, Arg expected, String location, String kind) {
        var objClass = obj.getClass();
        if (objClass.isAnonymousClass()) {
            var interfaces = objClass.getInterfaces();
            objClass = interfaces.length > 0 ? interfaces[0] : objClass.getSuperclass();
        }

        var actualLocation = location.substring(location.indexOf('!') + 1);
        var builder = new StringBuilder(actualLocation)
            .append(": ")
            .append(kind)
            .append(" ");

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
