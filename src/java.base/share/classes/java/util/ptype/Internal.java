package java.util.ptype;

import jdk.internal.misc.Unsafe;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.WeakHashMap;

final class Internal {

    private static final MethodHandles.Lookup LOOKUP = MethodHandles.lookup();

    private static final ClassValue<ArgHandle> ARG_HANDLE_CACHE = new ClassValue<>() {
        @Override
        protected ArgHandle computeValue(Class<?> type) {
            Objects.requireNonNull(type);
            return ArgHandle.of(type);
        }
    };

    private static final ClassValue<List<Arg>> STATIC_ARG_CACHE = new ClassValue<>() {
        @Override
        protected List<Arg> computeValue(Class<?> type) {
            Objects.requireNonNull(type);
            var superclass = type.getGenericSuperclass();
            var genericInterfaces = type.getGenericInterfaces();
            var list = new ArrayList<Arg>(genericInterfaces.length + (superclass != null ? 1 : 0));

            if (superclass != null) {
                list.add(Arg.fromType(superclass));
            }
            for (var superinterface : genericInterfaces) {
                list.add(Arg.fromType(superinterface));
            }
            return List.copyOf(list);
        }
    };

    private static final WeakHashMap<Object, ArrayType> ARRAY_TYPE_STORAGE = new WeakHashMap<>();

    public static MethodHandles.Lookup lookup() {
        return LOOKUP;
    }

    public static ArgHandle argHandle(Class<?> type) {
        Objects.requireNonNull(type);
        synchronized (ARG_HANDLE_CACHE) {
            return ARG_HANDLE_CACHE.get(type);
        }
    }

    public static List<Arg> staticArgs(Class<?> type) {
        Objects.requireNonNull(type);
        synchronized (STATIC_ARG_CACHE) {
            return STATIC_ARG_CACHE.get(type);
        }
    }

    public static ArrayType arrayType(Object array) {
        Objects.requireNonNull(array);
        if (!array.getClass().isArray()) {
            throw new IllegalArgumentException("Object " + array + " is not an array");
        }
        ArrayType value;
        synchronized (ARRAY_TYPE_STORAGE) {
            value = ARRAY_TYPE_STORAGE.get(array);
        }
        if (value == null) {
            throw new IllegalArgumentException("Array " + array + " not cached");
        }
        return value;
    }

    public static Optional<Object> outerThis(Object obj) {
        Objects.requireNonNull(obj);
        var handle = outerFieldGetter(obj.getClass());
        if (handle == null) {
            return Optional.empty();
        }
        try {
            return Optional.of(handle.invoke(obj));
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
            return LOOKUP.findGetter(type, "this$" + index, enclosingClass);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new AssertionError(e);
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

    public static void addArrayTypeArg(Object array, ArrayType arrayType) {
        Objects.requireNonNull(array);
        Objects.requireNonNull(arrayType);
        if (!array.getClass().isArray()) {
            throw new IllegalArgumentException("Object " + array + " is not an array");
        }
        synchronized (ARRAY_TYPE_STORAGE) {
            ARRAY_TYPE_STORAGE.compute(array, (key, old) -> {
                if (old != null) {
                    throw new IllegalArgumentException("Array " + key + " already cached");
                }
                return arrayType;
            });
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
            unsafe.getAndSetInt(LOOKUP, allowedModesOffset, -1);
        } catch (NoSuchFieldException e) {
            throw new AssertionError(e);
        }

    }

    private Internal() {
        throw new AssertionError();
    }

}
