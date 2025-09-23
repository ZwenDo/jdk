package java.util.ptype;

import jdk.internal.misc.Unsafe;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.ptype.util.HashMap;
import java.util.ptype.util.HashSet;
import java.util.ptype.util.Utils;

/// Utility class that provides logic used internally.
public final class Internal {

    private static final ClassValue<MethodHandle> FIELD_CACHE = new ClassValue<>() {
        @Override
        protected MethodHandle computeValue(Class<?> type) {
            try {
                return Holder.LOOKUP.findStatic(type, "$getDescriptorStatic", MethodType.methodType(ClassDescriptor.class, type));
            } catch (IllegalAccessException e) {
                throw new AssertionError(e);
            } catch (NoSuchMethodException e) {
                return null;
            }
        }
    };

    private static final ClassValue<MethodHandle> SUPER_GENERATION_CACHE = new ClassValue<>() {
        @Override
        protected MethodHandle computeValue(Class<?> type) {
            try {
                return Holder.LOOKUP.findStatic(
                        type,
                        "$populateSuperSet",
                        MethodType.methodType(void.class, ClassDescriptor.class, HashSet.class)
                );
            } catch (NoSuchMethodException | IllegalAccessException e) {
                throw new AssertionError(e);
            }
        }
    };

    static ClassDescriptor extractInformationField(Object obj) {
        var getter = FIELD_CACHE.get(obj.getClass());
        if (getter == null) return null;
        try {
            return (ClassDescriptor) getter.invoke(obj);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /// Generates the map containing the super types of a given specialized type.
    ///
    /// @param type     the type represented by this specialized type
    /// @param concreteDescriptor the descriptor
    /// @return the map associating all the supertypes to their value
    public static HashMap<Class<?>, ClassDescriptor> generateSuperTypes(
            Class<?> type,
            ClassDescriptor concreteDescriptor
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(concreteDescriptor);
        try {
            var method = SUPER_GENERATION_CACHE.get(type);
            var set = new HashSet<SuperTypeMapping>();
            method.invokeExact(concreteDescriptor, set);
            return HashMap.superTypeMap(set);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
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
