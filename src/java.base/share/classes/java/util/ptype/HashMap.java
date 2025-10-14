package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.util.NoSuchElementException;

/// HashSet.
/// @param <K> the type of elements maintained by this set
@PrototypeInternal
final class HashMap<K, V> {

    @SuppressWarnings({"unchecked"})
    private Node<K, V>[] content = (Node<K, V>[]) SENTINEL;

    private int size;

    private int modCount;

    @Stable
    private final Equivalence<? super K> equivalence;

    /// Creates a new, empty set.
    public HashMap() {
        this(Equivalence.natural());
    }

    HashMap(Equivalence<? super K> equivalence) {
        Utils.requireNonNull(equivalence);
        this.equivalence = equivalence;
    }

    V computeIfAbsent(K key, Function<? super K, ? extends V> mapper) {
        Utils.requireNonNull(key);
        Utils.requireNonNull(mapper);
        if (content.length <= size * 2) {
            resize();
            modCount++;
        }

        var hash = equivalence.hash(key) & (content.length - 1);
        var bucket = content[hash];

        if (bucket == null) {
            var value = mapper.apply(key);
            content[hash] = new Node<>(key, value);
            size++;
            modCount++;
            return value;
        }

        var current = bucket;
        while (true) {
            if (equivalence.equals(key, current.key)) {
                return current.value;
            }
            if (current.next == null) {
                var value = mapper.apply(key);
                current.next = new Node<>(key, value);
                size++;
                modCount++;
                return value;
            }
            current = current.next;
        }
    }

    int size() {
        return size;
    }

    Iterator<V> iterator() {
        return new Iterator<>() {
            private final int expectedModCount = modCount;
            private int index;
            private Node<K, V> current = null;

            {
                advance();
            }

            @Override
            public boolean hasNext() {
                return current != null;
            }

            @Override
            public V next() {
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
            content = (Node<K, V>[]) new Node[16];
            return;
        }

        var newArray = (Node<K, V>[]) new Node[content.length * 2];
        for (var node : content) { // iterate through all buckets
            if (node == null) continue;
            var current = node;


            do { // iterate through all nodes from a single bucket
                var toAdd = current;
                current = current.next;
                toAdd.next = null;

                var index = equivalence.hash(toAdd.key) & (newArray.length - 1);
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

    private static final class Node<K, V> {
        private final K key;
        private final V value;
        private Node<K, V> next;

        public Node(K key, V value) {
            this.key = key;
            this.value = value;
        }


    }

    @SuppressWarnings({"rawtypes"})
    private static final Node<?, ?>[] SENTINEL = (Node<?, ?>[]) new Node[0];

}
