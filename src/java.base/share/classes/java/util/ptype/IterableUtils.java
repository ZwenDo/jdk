package java.util.ptype;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;

final class IterableUtils {

    public static <T> boolean anyMatch(Iterable<T> list, Predicate<T> predicate) {
        Objects.requireNonNull(list);
        Objects.requireNonNull(predicate);
        for (var element : list) {
            if (predicate.test(element)) {
                return true;
            }
        }
        return false;
    }

    public static <T> boolean allMatch(Iterable<T> list, Predicate<T> predicate) {
        Objects.requireNonNull(list);
        Objects.requireNonNull(predicate);
        for (var element : list) {
            if (!predicate.test(element)) {
                return false;
            }
        }
        return true;
    }

    public static <T, R> List<R> map(T[] array, Function<T, R> mapper) {
        Objects.requireNonNull(array);
        Objects.requireNonNull(mapper);
        var result = new ArrayList<R>(array.length);
        for (var element : array) {
            Objects.requireNonNull(element);
            result.add(mapper.apply(element));
        }
        return result;
    }

    public static <R> List<R> map(int[] array, IntFunction<R> mapper) {
        Objects.requireNonNull(array);
        Objects.requireNonNull(mapper);
        var result = new ArrayList<R>(array.length);
        for (var element : array) {
            result.add(mapper.apply(element));
        }
        return result;
    }

    private IterableUtils() {
        throw new AssertionError();
    }

}
