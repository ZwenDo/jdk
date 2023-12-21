package java.util.ptype;

import java.lang.invoke.MethodHandle;
import java.util.Objects;
import java.util.Optional;

/**
 * A handle to get the {@link Arg} of a type.
 */
public interface ArgHandle {

    /**
     * Gets the {@link Arg} of the given type.
     *
     * @param holder the holder of the field
     * @param supertype the supertype represented by the field
     * @return an {@link Optional} containing the {@link Arg} if the field exists
     */
    Optional<Arg> arg(Object holder, Class<?> supertype);

    /**
     * Creates an {@link ArgHandle} for the given type.
     *
     * @param type the type
     * @return the {@link ArgHandle}
     */
    static ArgHandle of(Class<?> type) {
        Objects.requireNonNull(type);
        return new ArgHandle() {
            private final ClassValue<Optional<MethodHandle>> cache = new ClassValue<>() {

                @Override
                protected Optional<MethodHandle> computeValue(Class<?> supertype) {
                    Objects.requireNonNull(supertype);
                    try {
                        var name = typeArgsFieldName(supertype);
                        var getter = Internal.lookup().findGetter(type, name, Arg.class);
                        return Optional.of(getter);
                    } catch (NoSuchFieldException e) {
                        return Optional.empty();
                    } catch (Throwable throwable) {
                        throw new AssertionError(throwable);
                    }
                }

            };

            @Override
            public Optional<Arg> arg(Object holder, Class<?> supertype) {
                Objects.requireNonNull(holder);
                Objects.requireNonNull(supertype);
                return cache.get(supertype).map(getter -> {
                    try {
                        return (Arg) getter.invoke(holder);
                    } catch (Throwable throwable) {
                        throw new AssertionError(throwable);
                    }
                });
            }

        };
    }

    private static String typeArgsFieldName(Class<?> clazz) {
        var pkg = clazz.getPackageName();
        var name = clazz.getName().substring(pkg.length() + 1).replace('.', '$');
        return "typeArgs$" + pkg.replace('.', '$') + "$$" + name ;
    }

}
