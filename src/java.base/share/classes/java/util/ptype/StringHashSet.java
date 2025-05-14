package java.util.ptype;

final class StringHashSet {

    private Node[] content = new Node[64];

    private int size;

    public boolean add(String string) {
        Utils.requireNonNull(string);
        if (content.length <= size * 2) {
            resize();
        }

        var hash = string.hashCode() & (content.length - 1);
        var bucket = content[hash];

        if (bucket == null) {
            content[hash] = new Node(string);
            size++;
            return true;
        }

        var current = bucket;
        while (true) {
            if (string.equals(current.value)) {
                return false;
            }
            if (current.next == null) {
                current.next = new Node(string);
                size++;
                return true;
            }
            current = current.next;
        }
    }

    private void resize() {
        var newArray = new Node[content.length * 2];
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

    private static final class Node {
        private final String value;
        private Node next;

        public Node(String value) {
            this.value = value;
        }
    }

}
