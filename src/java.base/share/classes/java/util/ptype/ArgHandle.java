package java.util.ptype;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.WeakHashMap;

/**
 * A handle to get the {@link Arg} of a type.
 */
public interface ArgHandle {

    /**
     * Gets the {@link Arg} of the given type.
     *
     * @param holder    the holder of the field
     * @param supertype the supertype represented by the field
     * @return an Optional containing the {@link Arg} if the field exists
     */
    ArgOptional arg(Object holder, Class<?> supertype);

    /**
     * Creates an {@link ArgHandle} for the given type.
     *
     * @param type the type
     * @return the {@link ArgHandle}
     */
    static ArgHandle of(Class<?> type) {
        Utils.requireNonNull(type);
        return new ArgHandleImpl(type);
    }

    /**
     * Implementation of {@link ArgHandle}.
     */
    class ArgHandleImpl implements ArgHandle {
        private final WeakHashMap<Class<?>, MethodHandle> cache;
        private static final MethodHandle SENTINEL = MethodHandles.constant(Arg.class, null);
        private final Class<?> type;

        private ArgHandleImpl(Class<?> type) {
            this.type = type;
            cache = new WeakHashMap<>();
        }

        @Override
        public ArgOptional arg(Object holder, Class<?> supertype) {
            Utils.requireNonNull(holder);
            Utils.requireNonNull(supertype);
            MethodHandle result;
            synchronized (cache) {
                result = cache.get(supertype);
                if (result == null) {
                    result = computeValue(supertype);
                    cache.put(supertype, result);
                }
            }
            if (result == SENTINEL) {
                return ArgOptional.empty();
            }
            try {
                return ArgOptional.of((Arg) result.invoke(holder));
            } catch (Throwable throwable) {
                throw new AssertionError(throwable);
            }

        }

        private MethodHandle computeValue(Class<?> supertype) {
            Utils.requireNonNull(supertype);
            try {
                var name = TypeArgUtils.typeArgsFieldName(supertype);
                var getter = Internal.lookup().findGetter(type, name, Arg.class);
                Utils.requireNonNull(getter);
                return getter;
            } catch (NoSuchFieldException e) {
                return staticArg(supertype);
            } catch (Throwable throwable) {
                if (throwable instanceof RuntimeException)
                    throw (RuntimeException) throwable;
                throw new AssertionError(throwable);
            }
        }

        private MethodHandle staticArg(Class<?> superType) {
            var type = findType(superType);
            if (type == null) return SENTINEL;
            return MethodHandles.dropArguments(
                MethodHandles.constant(Arg.class, Arg.fromType(type)),
                0,
                Object.class
            );
        }

        private Type findType(Class<?> superType) {
            var isInterface = superType.isInterface();
            var current = type;
            while (current != null) {
                if (isInterface) {
                    for (var i : current.getGenericInterfaces()) {
                        if (i instanceof Class) {
                            if (i == superType) return i;
                        } else if (i instanceof ParameterizedType ti) {
                            if (ti.getRawType() == superType) return ti;
                        }
                    }
                } else {
                    var ts = current.getGenericSuperclass();
                    if (ts instanceof Class) {
                        if (ts == superType) return ts;
                    } else if (ts instanceof ParameterizedType tspt) {
                        if (tspt.getRawType() == superType) return tspt;
                    }
                }
                current = current.getSuperclass();
            }

            return null;
        }

    }
}
