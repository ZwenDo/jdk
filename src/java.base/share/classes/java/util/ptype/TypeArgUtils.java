package java.util.ptype;

import jdk.internal.misc.VM;

import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
        if (concrete instanceof RawType) {
            return RawType.of(superType);
        } else if (concrete instanceof ParameterizedType p) {
            var args = new Arg[indices.length];
            for (var arg : indices) {
                args[arg] = p.typeArgs().get(arg);
            }
            return ParameterizedType.of(superType, args);
        }
        throw new AssertionError("Unexpected value: " + concrete);
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
        if (!VM.isBooted()) return ArgList.of();
        return Internal.staticArgs(type);
    }

    private static Arg getArgInternal(Arg concrete, int currentIndex, int... indices) {
        if (currentIndex == indices.length) { // base case
            return concrete;
        }
        if (concrete instanceof RawType rt) {
            var arg = rawTypeArg(rt.type());
            return getArgInternal(arg, currentIndex, indices);
        } else if (concrete instanceof ArrayType a) {
            return getArgInternal(a.componentType(), currentIndex, indices);
        } else if (concrete instanceof ParameterizedType p) {
            var list = p.typeArgs();
            Objects.checkIndex(indices[currentIndex], list.size());
            return getArgInternal(list.get(indices[currentIndex]), currentIndex + 1, indices);
        } else if (concrete instanceof InnerClassType i) {
            return getArgInternal(i.innerType(), currentIndex, indices);
            // wildcard and intersection are not supported because we are in the context of a concrete type
        } else if (concrete instanceof Wildcard) {
            throw new UnsupportedOperationException("Wildcard not supported");
        } else if (concrete instanceof Intersection) {
            throw new UnsupportedOperationException("Intersection not supported");
        } else if (concrete instanceof ClassType) {
            throw new IllegalArgumentException("ClassType has no type args");
        }
        throw new IllegalArgumentException(concrete.getClass().getName());
    }

    private static Arg rawTypeArg(Class<?> clazz) {
        if (clazz == Map.class || clazz == HashMap.class || clazz == AbstractMap.class) {
            return mapLike(clazz);
        } else if (clazz == List.class || clazz == ArrayList.class || clazz == AbstractList.class || clazz == Collection.class || clazz == AbstractCollection.class || clazz == Iterable.class) {
            return collectionLike(clazz);
        }
        return ParameterizedType.of(clazz, Arg.fromTypes(clazz.getTypeParameters()));
    }

    private static Arg mapLike(Class<?> clazz) {
        return ParameterizedType.of(clazz, objectClassType(), objectClassType());
    }

    private static Arg collectionLike(Class<?> clazz) {
        return ParameterizedType.of(clazz, objectClassType());
    }

    private static Arg objectClassType() {
        return ClassType.of(Object.class);
    }


    private TypeArgUtils() {
        throw new AssertionError();
    }
}
