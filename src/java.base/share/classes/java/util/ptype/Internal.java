package java.util.ptype;

import jdk.internal.misc.Unsafe;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Objects;
import java.util.WeakHashMap;

final class Internal {

    private static final class Holder {
        private static final MethodHandles.Lookup LOOKUP = MethodHandles.lookup();
    }

    private static final ClassValue<ArgList> STATIC_ARG_CACHE = new ClassValue<>() {
        @Override
        protected ArgList computeValue(Class<?> type) {
            Utils.requireNonNull(type);
            var superclass = type.getGenericSuperclass();
            var genericInterfaces = type.getGenericInterfaces();
            var array = new Arg[genericInterfaces.length + (superclass != null ? 1 : 0)];
            var index = 0;
            if (superclass != null) {
                array[index++] = Arg.fromType(superclass);
            }
            for (var superinterface : genericInterfaces) {
                array[index++] = Arg.fromType(superinterface);
            }
            return ArgList.of(array);
        }
    };

    private static final WeakHashMap<Object, ArrayType> ARRAY_TYPE_STORAGE = new WeakHashMap<>();

    public static MethodHandles.Lookup lookup() {
        return Holder.LOOKUP;
    }

    public static ArgList staticArgs(Class<?> type) {
        Utils.requireNonNull(type);
        synchronized (STATIC_ARG_CACHE) {
            return STATIC_ARG_CACHE.get(type);
        }
    }

    public static RawOptional outerThis(Object obj, Class<?> expectedClass) {
        Utils.requireNonNull(obj);
        Utils.requireNonNull(expectedClass);
        var handle = outerFieldGetter(expectedClass);
        if (handle == null) {
            return RawOptional.empty();
        }
        try {
            return RawOptional.of(handle.invoke(obj));
        } catch (Throwable e) {
            throw new AssertionError(e);
        }
    }

    private static MethodHandle outerFieldGetter(Class<?> type) {
        var enclosingMethod = type.getEnclosingMethod();  // local method
        var enclosingClass = enclosingMethod != null ? enclosingMethod.getDeclaringClass() : type.getEnclosingClass();

        if (enclosingClass == null || Modifier.isStatic(type.getModifiers())) { // not nested or static
            throw new IllegalArgumentException("Not an inner class: " + type);
        }
        var index = computeIndex(enclosingClass);

        try {
            return Holder.LOOKUP.findGetter(type, "this$" + index, enclosingClass);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            return null;
        }
    }

    private static int computeIndex(Class<?> type) {
        var current = type;
        var index = -1;
        while (current != null) {
            index++;
            var enclosingMethod = current.getEnclosingMethod();
            if (enclosingMethod != null) {
                current = outerFromMethod(enclosingMethod);
            } else {
                current = outerFromClass(current);
            }
        }
        return index;
    }

    private static Class<?> outerFromMethod(Method method) {
        if (Modifier.isStatic(method.getModifiers())) {
            return null;
        }
        return method.getDeclaringClass();
    }

    private static Class<?> outerFromClass(Class<?> type) {
        var next = type.getEnclosingClass();
        if (next == null || Modifier.isStatic(type.getModifiers())) {
            return null;
        }
        return next;
    }

    public static Class<?> findClass(String internalName) {
        Objects.requireNonNull(internalName);
        try {
            return Class.forName(internalName, false, ClassLoader.getPlatformClassLoader());
        } catch (ClassNotFoundException e) {
            throw new AssertionError("Could not load: " + internalName, e);
        }
    }

    static {
        try {
            class LookupMock {
                private Class<?> lookupClass;
                private Class<?> prevLookupClass;
                private int allowedModes;
            }

            var unsafe = Unsafe.getUnsafe();
            var allowedModesOffset = unsafe.objectFieldOffset(LookupMock.class.getDeclaredField("allowedModes"));
            unsafe.getAndSetInt(Holder.LOOKUP, allowedModesOffset, -1);
        } catch (NoSuchFieldException e) {
            throw new AssertionError(e);
        }

    }

    private Internal() {
        throw new AssertionError();
    }

}
