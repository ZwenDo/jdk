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
     * @param kind     the kind of cast
     * @param target   the target of the cast
     * @return         the cast object
     */
    public static Object checkCast(
        Object obj,
        Arg expected,
        String location,
        CheckLocationKind kind,
        CheckTarget target
    ) {
        if (!VM.isBooted()) return obj;
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
                    System.out.println(message);
                }
            } finally {
                LockHolder.LOCK.unlock();
            }
        }

        return obj;
    }

    /**
     * Called when the main start has been reached.
     *
     * @param id the id of the main method
     */
    public static void mainStartReached(String id) {
        Utils.requireNonNull(id);
        if (mainId != null) return;
        mainId = id;
        fromMain = true;
//        System.out.println("MAIN START");
    }

    /**
     * Called when the main end has been reached.
     *
     * @param id the id of the main method
     */
    public static void mainEndReached(String id) {
        Utils.requireNonNull(mainId);
        if (mainId == null) throw new AssertionError();
        if (!mainId.equals(id)) return;
        fromMain = false;
//        System.out.println("MAIN END");
    }

    private static boolean shouldPerform(CheckLocationKind kind, CheckTarget target) {
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

            // we need to extract the expected outer class, because the actual object inner class might have an outer
            // this that does not extend the expected outer class (e.g. Attr.ResultInfo & Resolve.MethodResultInfo).
            Class<?> expectedOuterClass;
            var outerClassArg = innerClassType.outerType();
            if (outerClassArg instanceof ClassType outerClassType) {
                expectedOuterClass = outerClassType.type();
            } else if (outerClassArg instanceof ParameterizedType parameterizedType) {
                expectedOuterClass = parameterizedType.rawType();
            } else if (outerClassArg instanceof RawType rawType) {
                expectedOuterClass = rawType.type();
            } else {
                throw new AssertionError("Unexpected outer type: " + innerClassType.outerType());
            }

            var outer = Internal.outerThis(obj, expectedOuterClass);
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
        } else if (expected instanceof ArrayType) {
            if (!obj.getClass().isArray()) return false;
            return validate(obj, expected, obj.getClass());
            // Note that this kind of cast should not be possible
            // var cast = (? extends String) obj;
            // var cast = (? super String) obj;
            // var cast = (?) obj; equivalent to (? extends Object)
        } else if (expected instanceof Wildcard wildcard) {
            var objClass = obj.getClass();
            var opt = ArgMap.internalGet(obj, objClass);
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
        var opt = ArgMap.internalGet(obj, supertype);
        if (opt.isEmpty()) {
            return supertype.isAssignableFrom(objClass);
        }
        return expected.isAssignable(opt.get(), Arg.Variance.COVARIANT);
    }

    private static String message(Object obj, Arg expected, String location, CheckLocationKind kind) {
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

        var arg = ArgMap.internalGet(obj, objClass);
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
