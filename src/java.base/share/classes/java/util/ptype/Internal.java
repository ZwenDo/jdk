package java.util.ptype;

import jdk.internal.misc.Unsafe;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ptype.model.SpecializedTypeDescriptor;
import java.util.ptype.util.HashMap;
import java.util.ptype.util.Optional;
import java.util.ptype.util.Utils;

/// Utility class that provides logic used internally.
public final class Internal {

    private static final ClassValue<Optional<MethodHandle>> FIELD_CACHE = new ClassValue<>() {
        @Override
        protected Optional<MethodHandle> computeValue(Class<?> type) {
            try {
                var getter = Holder.LOOKUP.findGetter(type, "$typeArguments", SpecializedTypeDescriptor.class);
                return Optional.of(getter);
            } catch (NoSuchFieldException e) {
                return Optional.empty();
            } catch (IllegalAccessException e) {
                throw new AssertionError(e);
            }
        }
    };

    private static final ClassValue<MethodHandle> SUPER_GENERATION_CACHE = new ClassValue<>() {
        @Override
        protected MethodHandle computeValue(Class<?> type) {
            try {
                return Holder.LOOKUP.findStatic(
                        type,
                        "$computeSuper",
                        MethodType.methodType(HashMap.class, SpecializedTypeDescriptor.class)
                );
            } catch (NoSuchMethodException | IllegalAccessException e) {
                throw new AssertionError(e);
            }
        }
    };

    static Optional<SpecializedTypeDescriptor> extractInformationField(Object obj) {
        var getter = FIELD_CACHE.get(obj.getClass());
        if (getter.isEmpty()) return Optional.empty();
        try {
            return Optional.of((SpecializedTypeDescriptor) getter.get().invoke(obj));
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /// Generates the map containing the super types of a given specialized type.
    ///
    /// @param type     the type represented by this specialized type
    /// @param concrete the concrete specialized type
    /// @return the map associating all the supertypes to their value
    @SuppressWarnings("unchecked")
    public static HashMap<Class<?>, SpecializedTypeDescriptor> generateSuperTypes(Class<?> type, SpecializedTypeDescriptor concrete) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(concrete);
        try {
            var method = SUPER_GENERATION_CACHE.get(type);
            return (HashMap<Class<?>, SpecializedTypeDescriptor>) method.invokeExact(concrete);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    static Optional<Object> outerThis(Object obj, Class<?> expectedClass) {
        Utils.requireNonNull(obj);
        Utils.requireNonNull(expectedClass);
        var handle = outerFieldGetter(expectedClass);
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

    private static final class Holder {
        private static final MethodHandles.Lookup LOOKUP;

        static {
            try {
                class LookupMock {
                    private Class<?> lookupClass;
                    private Class<?> prevLookupClass;
                    private int allowedModes;
                }

                var lookup = MethodHandles.lookup();
                var unsafe = Unsafe.getUnsafe();
                var allowedModesOffset = unsafe.objectFieldOffset(LookupMock.class.getDeclaredField("allowedModes"));
                unsafe.getAndSetInt(lookup, allowedModesOffset, -1);

                LOOKUP = lookup;
            } catch (NoSuchFieldException e) {
                throw new AssertionError(e);
            }
        }
    }

    private Internal() {
        throw new AssertionError();
    }

}
