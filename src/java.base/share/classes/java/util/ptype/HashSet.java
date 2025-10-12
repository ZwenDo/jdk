package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.util.NoSuchElementException;

/// HashSet.
/// @param <E> the type of elements maintained by this set
@PrototypeInternal
final class HashSet<E> {

    @SuppressWarnings({"unchecked"})
    private Node<E>[] content = (Node<E>[]) SENTINEL;

    private int size;

    private int modCount;

    @Stable
    private final Equivalence<? super E> equivalence;

    /// Creates a new, empty set.
    public HashSet() {
        this(Equivalence.natural());
    }

    public HashSet(Equivalence<? super E> equivalence) {
        Utils.requireNonNull(equivalence);
        this.equivalence = equivalence;
    }

    /// Adds an element to the set if it is not already present. If the element is already present, the existing element
    /// is returned.
    ///
    /// @param element the element to add
    /// @return the existing element if it was already present, otherwise null
    public E add(E element) {
        Utils.requireNonNull(element);
        if (content.length <= size * 2) {
            resize();
            modCount++;
        }

        var hash = equivalence.hash(element) & (content.length - 1);
        var bucket = content[hash];

        if (bucket == null) {
            content[hash] = new Node<>(element);
            size++;
            modCount++;
            return null;
        }

        var current = bucket;
        while (true) {
            if (equivalence.equals(element, current.value)) {
                return current.value;
            }
            if (current.next == null) {
                current.next = new Node<>(element);
                size++;
                modCount++;
                return null;
            }
            current = current.next;
        }
    }

    /// Safety: Called must ensure that for any `x` of type `E` the invariant
    /// `tEquivalence.equal(element, x) == equivalence.equal(mapper.apply(element), x)` is respected.
    <T> E computeIfAbsent(T element, Equivalence<T> tEquivalence, Function<? super T, ? extends E> mapper) {
        Utils.requireNonNull(element);
        Utils.requireNonNull(mapper);
        if (content.length <= size * 2) {
            resize();
            modCount++;
        }

        var hash = tEquivalence.hash(element) & (content.length - 1);
        var bucket = content[hash];

        if (bucket == null) {
            var value = mapper.apply(element);
            content[hash] = new Node<>(value);
            size++;
            modCount++;
            return value;
        }

        var current = bucket;
        while (true) {
            if (equivalence.equals(current.value, element)) {
                return current.value;
            }
            if (current.next == null) {
                var value = mapper.apply(element);
                current.next = new Node<>(value);
                size++;
                modCount++;
                return value;
            }
            current = current.next;
        }
    }

    void addAll(HashSet<E> other) {
        Utils.requireNonNull(other);
        for (var it = other.iterator(); it.hasNext();) {
            add(it.next());
        }
    }

    /// Returns the number of elements in the set.
    ///
    /// @return the number of elements in the set
    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public String toString() {
        var builder = new StringBuilder();
        builder.append("{");
        for (var it = iterator(); it.hasNext();) {
            builder.append(it.next());
            if (it.hasNext()) {
                builder.append(", ");
            }
        }
        builder.append("}");
        return builder.toString();
    }

    /// Returns an iterator over the elements in the set. The elements are returned in no particular order.
    ///
    /// @return an iterator over the elements in the set
    Iterator<E> iterator() {
        return new Iterator<>() {
            private final int expectedModCount = modCount;
            private int index;
            private Node<E> current = null;

            {
                advance();
            }

            @Override
            public boolean hasNext() {
                return current != null;
            }

            @Override
            public E next() {
                if (!hasNext()) throw new NoSuchElementException("no more elements");
                var value = current.value;
                advance();
                return value;
            }

            private void advance() {
                if (expectedModCount != modCount) {
                    throw new IllegalStateException("concurrent modification");
                }
                if (current != null && current.next != null) {
                    current = current.next;
                    return;
                }
                while (index < content.length) {
                    var bucket = content[index++];
                    if (bucket != null) {
                        current = bucket;
                        return;
                    }
                }
                current = null;
            }
        };
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void resize() {
        if (content == SENTINEL) {
            content = (Node<E>[]) new Node[16];
            return;
        }

        var newArray = (Node<E>[]) new Node[content.length * 2];
        for (var node : content) { // iterate through all buckets
            if (node == null) continue;
            var current = node;


            do { // iterate through all nodes from a single bucket
                var toAdd = current;
                current = current.next;
                toAdd.next = null;

                var index = equivalence.hash(toAdd.value) & (newArray.length - 1);
                var bucketNode = newArray[index];
                if (bucketNode == null) { // there is no bucket, just add the node at the index
                    newArray[index] = toAdd;
                } else { // there is at least 1 node in the bucket, add the new node last
                    while (bucketNode.next != null) {
                        bucketNode = bucketNode.next;
                    }
                    bucketNode.next = toAdd;
                }
            } while (current != null);
        }
        content = newArray;
    }

    private static final class Node<E> {
        private final E value;
        private Node<E> next;

        public Node(E value) {
            this.value = value;
        }

        @Override
        public String toString() {
            var current = this;
            var builder = new StringBuilder();
            builder.append('[');
            while (current != null) {
                builder.append(current.value);
                if (current.next != null) {
                    builder.append(", ");
                }
                current = current.next;
            }
            builder.append(']');
            return builder.toString();
        }

    }

    @SuppressWarnings({"rawtypes"})
    private static final Node<?>[] SENTINEL = (Node<?>[]) new Node[0];

}
