package java.util.ptype;

import java.util.Objects;

/**
 * Utility methods for working with {@link Arg}s.
 */
public final class TypeArgUtils {

    /**
     * Creates an {@link Arg} representing a super type from the given concrete {@link Arg}.
     *
     * @param concrete  the concrete type
     * @param superType the super type
     * @param indices   the indices of the type args to use
     * @return the {@link Arg}
     */
    public static Arg toSuper(Arg concrete, Class<?> superType, int... indices) {
        Utils.requireNonNull(concrete);
        Utils.requireNonNull(superType);
        if (indices.length == 0) {
            throw new IllegalArgumentException("indices.length == 0");
        }
        return switch (concrete) {
            case RawType ignored -> RawType.of(superType);
            case ParameterizedType p -> {
                var args = new Arg[indices.length];
                for (var arg : indices) {
                    args[arg] = p.typeArgs().get(arg);
                }
                yield ParameterizedType.of(superType, args);
            }
            default -> throw new AssertionError("Unexpected value: " + concrete);
        };
    }

    /**
     * Gets the {@link Arg} stored in an object representing a given super type.
     *
     * @param concrete  the concrete type
     * @param supertype the super type
     * @return the {@link Arg}
     */
    public static Arg getArg(Object concrete, Class<?> supertype) {
        Utils.requireNonNull(concrete);
        Utils.requireNonNull(supertype);
        var arg = ArgHandle.of(concrete.getClass())
            .arg(concrete, supertype);
        if (arg.isEmpty()) {
            throw new IllegalArgumentException("Object " + concrete + " is not an instance of " + supertype);
        }
        return arg.get();
    }

    /**
     * Gets the {@link Arg} stored in an object representing a given super type.
     *
     * @param concrete the concrete type
     * @param indices  the indices of the type args to use
     * @return the {@link Arg}
     */
    public static Arg getArg(Arg concrete, int... indices) {
        Utils.requireNonNull(concrete);
        if (indices.length == 0) {
            throw new IllegalArgumentException("indices.length == 0");
        }
        return getArgInternal(concrete, 0, indices);
    }

    /**
     * Gets all the super interfaces {@link Arg}s of the given type.
     *
     * @param type the type
     * @return the {@link Arg}s
     */
    public static ArgList getGenericSupertypes(Class<?> type) {
        Utils.requireNonNull(type);
        return Internal.staticArgs(type);
    }

    /**
     * Adds the given {@link Arg} representing the given array to the cache of array types.
     *
     * @param array     the array
     * @param arrayType the {@link Arg} representing the array
     * @return the array
     */
    public static Object addArrayTypeArg(Object array, ArrayType arrayType) {
        Utils.requireNonNull(array);
        Utils.requireNonNull(arrayType);
        Internal.addArrayTypeArg(array, arrayType);
        return array;
    }

    /**
     * Gets the {@link Arg} representing the given array from the cache of array types.
     *
     * @param array the array
     * @return the {@link Arg}
     */
    public static ArgOptional arrayType(Object array) {
        Utils.requireNonNull(array);
        return Internal.arrayType(array);
    }

    /**
     * Gets the name of the field storing the type args of the given class.
     *
     * @param clazz the class
     * @return the name of the field
     */
    public static String typeArgsFieldName(Class<?> clazz) {
        Utils.requireNonNull(clazz);
        var pkg = clazz.getPackageName();
        var substringSize = pkg.isEmpty() ? 0 : pkg.length() + 1;
        var name = clazz.getName().substring(substringSize).replace('.', '$');
        return "0$typeArgs$" + pkg.replace('.', '$') + "$$" + name;
    }

    private static Arg getArgInternal(Arg concrete, int currentIndex, int... indices) {
        if (currentIndex == indices.length) { // base case
            return concrete;
        }
        return switch (concrete) {
            case RawType ignored -> throw new IllegalArgumentException("RawType has no type args");
            case InnerClassType ict -> getArgInternal(ict.innerType(), currentIndex, indices);
            case ArrayType a -> getArgInternal(a.componentType(), currentIndex, indices);
            case ParameterizedType p -> {
                var list = p.typeArgs();
                Objects.checkIndex(indices[currentIndex], list.size());
                yield getArgInternal(list.get(indices[currentIndex]), currentIndex + 1, indices);
            }
            // wildcard and intersection are not supported because we are in the context of a concrete type
            case Wildcard ignored -> throw new UnsupportedOperationException("Wildcard not supported");
            case Intersection ignored -> throw new UnsupportedOperationException("Intersection not supported");
            case ClassType ignored -> throw new IllegalArgumentException("ClassType has no type args");
        };
    }

    private TypeArgUtils() {
        throw new AssertionError();
    }

}
