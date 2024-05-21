package java.util.ptype;

import java.lang.invoke.MethodHandle;

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
        return new ArgHandle() {
            private final ClassValue<MethodHandle> cache = new ClassValue<>() {

                @Override
                protected MethodHandle computeValue(Class<?> supertype) {
                    Utils.requireNonNull(supertype);
                    try {
                        var name = TypeArgUtils.typeArgsFieldName(supertype);
                        var getter = Internal.lookup().findGetter(type, name, Arg.class);
                        Utils.requireNonNull(getter);
                        return getter;
                    } catch (NoSuchFieldException e) {
                        return null;
                    } catch (Throwable throwable) {
                        throw new AssertionError(throwable);
                    }
                }

            };

            @Override
            public ArgOptional arg(Object holder, Class<?> supertype) {
                Utils.requireNonNull(holder);
                Utils.requireNonNull(supertype);
                var result = cache.get(supertype);
                if (result == null) {
                    return null;
                }
                try {
                    return ArgOptional.of((Arg) result.invoke(holder));
                } catch (Throwable throwable) {
                    throw new AssertionError(throwable);
                }

            }

        };
    }

}
