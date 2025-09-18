package java.util.ptype.util;

import java.util.NoSuchElementException;

/// HashSet.
/// @param <E> the type of elements maintained by this set
public final class HashSet<E> {

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Node<E>[] content = (Node<E>[]) new Node[64];

    private int size;

    private int modCount;

    /// Creates a new, empty set.
    public HashSet() {
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

        var hash = element.hashCode() & (content.length - 1);
        var bucket = content[hash];

        if (bucket == null) {
            content[hash] = new Node<>(element);
            size++;
            modCount++;
            return null;
        }

        var current = bucket;
        while (true) {
            if (element.equals(current.value)) {
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

    /// Returns the number of elements in the set.
    ///
    /// @return the number of elements in the set
    public int size() {
        return size;
    }

    /// Returns an iterator over the elements in the set. The elements are returned in no particular order.
    ///
    /// @return an iterator over the elements in the set
    public Iterator<E> iterator() {
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

    private void resize() {
        @SuppressWarnings({"unchecked", "rawtypes"})
        var newArray = (Node<E>[]) new Node[content.length * 2];
        for (var node : content) { // iterate through all buckets
            if (node == null) continue;
            var current = node;


            do { // iterate through all nodes from a single bucket
                var toAdd = current;
                current = current.next;
                toAdd.next = null;

                var index = toAdd.value.hashCode() & (newArray.length - 1);
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
    }

}
