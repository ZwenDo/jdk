package java.util.ptype;

import java.util.List;
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
        Objects.requireNonNull(concrete);
        Objects.requireNonNull(superType);
        if (indices.length == 0) {
            throw new IllegalArgumentException("indices.length == 0");
        }
        return switch (concrete) {
            case RawType r -> toSuper(r.rawArg(), superType, indices);
            case ParameterizedType p -> {
                var list = IterableUtils.map(indices, p.typeArgs()::get);
                yield ParameterizedType.of(superType, list);
            }
            default -> throw new AssertionError("Unexpected value: " + concrete);
//            case ArrayType _, ClassType _, InnerClassType _, Intersection _, Wildcard _ ->
//                throw new AssertionError(STR."Unexpected value: \{concrete}");
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
        Objects.requireNonNull(concrete);
        Objects.requireNonNull(supertype);
        return ArgHandle.of(concrete.getClass())
            .arg(concrete, supertype)
            .orElseThrow(() -> new IllegalArgumentException("Object " + concrete + " is not an instance of " + supertype));
    }

    /**
     * Gets the {@link Arg} stored in an object representing a given super type.
     *
     * @param concrete the concrete type
     * @param indices  the indices of the type args to use
     * @return the {@link Arg}
     */
    public static Arg getArg(Arg concrete, int... indices) {
        Objects.requireNonNull(concrete);
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
    public static List<Arg> getGenericSupertypes(Class<?> type) {
        Objects.requireNonNull(type);
        return Internal.staticArgs(type);
    }

    /**
     * Adds the given {@link Arg} representing the given array to the cache of array types.
     *
     * @param array     the array
     * @param arrayType the {@link Arg} representing the array
     */
    public static void addArrayTypeArg(Object array, ArrayType arrayType) {
        Objects.requireNonNull(array);
        Objects.requireNonNull(arrayType);
        Internal.addArrayTypeArg(array, arrayType);
    }

    /**
     * Gets the {@link Arg} representing the given array from the cache of array types.
     *
     * @param array the array
     * @return the {@link Arg}
     */
    public static ArrayType arrayType(Object array) {
        Objects.requireNonNull(array);
        return Internal.arrayType(array);
    }

    private static Arg getArgInternal(Arg concrete, int currentIndex, int... indices) {
        if (currentIndex == indices.length) { // base case
            return concrete;
        }
        return switch (concrete) {
            case RawType r -> getArgInternal(r.rawArg(), currentIndex, indices);
            case InnerClassType ict -> getArgInternal(ict.innerType(), currentIndex, indices);
            case ArrayType a -> getArgInternal(a.componentType(), currentIndex, indices);
            case ParameterizedType p -> {
                var list = p.typeArgs();
                Objects.checkIndex(indices[currentIndex], list.size());
                yield getArgInternal(list.get(indices[currentIndex]), currentIndex + 1, indices);
            }
            // wildcard and intersection are not supported because we are in the context of a concrete type
            case Wildcard wc ->
                throw new UnsupportedOperationException("Wildcard not supported"); //getArgInternal(w.upperBound(), currentIndex, indices);
            case Intersection is -> throw new UnsupportedOperationException("Intersection not supported");
            case ClassType ct -> throw new IllegalArgumentException("ClassType has no type args");
        };
    }

    private TypeArgUtils() {
        throw new AssertionError();
    }

}
