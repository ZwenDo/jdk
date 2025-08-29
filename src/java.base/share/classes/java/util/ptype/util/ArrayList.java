package java.util.ptype.util;

import jdk.internal.vm.annotation.Stable;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.RandomAccess;

/// Immutable array list.
///
/// @param <E> the type of the elements
public final class ArrayList<E> implements RandomAccess {

    @Stable
    private final E[] array;

    private static final ArrayList<?> EMPTY = new ArrayList<>(new Object[0]);

    private ArrayList(E[] array) {
        this.array = array;
    }

    /// Creates a new empty list.
    ///
    /// @return the created list
    /// @param <E> the type of elements of the list
    @SuppressWarnings("unchecked")
    public static <E> ArrayList<E> of() {
        return (ArrayList<E>) EMPTY;
    }

    /// Creates a new list from an array
    ///
    /// @param array the array
    /// @return the created list
    /// @param <E> the type of elements of the list
    public static <E> ArrayList<E> of(E[] array) {
        Utils.requireNonNull(array);
        if (array.length == 0) {
            return of();
        }

        @SuppressWarnings("unchecked")
        var dest = (E[]) new Object[array.length];

        System.arraycopy(array, 0, dest, 0, array.length);
        return new ArrayList<>(dest);
    }

    /// Gets the element at the given index.
    ///
    /// @param index the index
    /// @return the element at the index
    public E get(int index) {
        Objects.checkIndex(index, array.length);
        return array[index];
    }

    /// Gets the size of this list
    ///
    /// @return the size of the list
    public int size() {
        return array.length;
    }

    /// Returns whether the list is empty
    ///
    /// @return true if the list is empty; false otherwise.
    public boolean isEmpty() {
        return array.length == 0;
    }

    /// Joins the given list into a [StringBuilder].
    ///
    /// @param builder the string builder where to accumulate the elements
    /// @param consumer the action to add an element to the builder
    /// @param separator the separator between each element
    public void joinTo(StringBuilder builder, BiConsumer<StringBuilder, E> consumer, String separator) {
        switch (array.length) {
            case 0: break;
            case 1:
                builder.append(array[0]);
                break;
            default:
                for (var i = 0; i < array.length - 1; i++) {
                    consumer.accept(builder, array[i]);
                    builder.append(separator);
                }
                builder.append(array[array.length - 1]);
        }
    }

    /// Tests whether all the elements of this list match a given predicate.
    ///
    /// @param predicate the predicate to match
    /// @return true if all elements matches the predicate or if the list is empty; false otherwise
    public boolean allMatch(Predicate<E> predicate) {
        Utils.requireNonNull(predicate);
        for (var e : array) {
            if (!predicate.test(e)) {
                return false;
            }
        }
        return true;
    }

    /// Tests whether any of the elements of this list matches a given predicate.
    ///
    /// @param predicate the predicate to match
    /// @return true if at least one element matches the predicate; false otherwise
    public boolean anyMatch(Predicate<E> predicate) {
        Utils.requireNonNull(predicate);
        for (var e : array) {
            if (predicate.test(e)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        var builder = new StringBuilder();
        builder.append('[');
        for (var i = 0; i < array.length; i++) {
            builder.append(array[i]);
            if (i != array.length - 1) {
                builder.append(", ");
            }
        }
        builder.append(']');
        return builder.toString();
    }

}
