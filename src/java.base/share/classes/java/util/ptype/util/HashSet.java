package java.util.ptype.util;

final class HashSet<E> {

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Node<E>[] content = (Node<E>[]) new Node[64];

    private int size;

    public boolean add(E element) {
        Utils.requireNonNull(element);
        if (content.length <= size * 2) {
            resize();
        }

        var hash = element.hashCode() & (content.length - 1);
        var bucket = content[hash];

        if (bucket == null) {
            content[hash] = new Node<>(element);
            size++;
            return true;
        }

        var current = bucket;
        while (true) {
            if (element.equals(current.value)) {
                return false;
            }
            if (current.next == null) {
                current.next = new Node<>(element);
                size++;
                return true;
            }
            current = current.next;
        }
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
