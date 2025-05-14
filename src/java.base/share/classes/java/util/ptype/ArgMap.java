package java.util.ptype;

import jdk.internal.misc.VM;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The global map used in the reification prototype
 */
public final class ArgMap {

    private static final double LOAD_FACTOR = 0.75;
    private static final int CAPACITY = 256;
    private static final int MAXIMUM_CAPACITY = 1 << 30;

    private static final class Holder {

        private static final ArgMap INSTANCE = new ArgMap(CAPACITY, LOAD_FACTOR);

    }


    private final double loadFactor;
    private final ReentrantLock lock = new ReentrantLock();
    private final ReferenceQueue<Object> queue = new ReferenceQueue<>();
    private Entry[] table;
    private int threshold;
    private int size;

    private ArgMap(final int initialCapacity, double loadFactor) {
        if (initialCapacity < 0) {
            throw new IllegalArgumentException("Illegal Initial Capacity: " + initialCapacity);
        }
        if (loadFactor <= 0 || Double.isNaN(loadFactor)) {
            throw new IllegalArgumentException("Illegal Load factor: " + loadFactor);
        }

        var capacity = tableSizeFor(Math.min(initialCapacity, MAXIMUM_CAPACITY));
        this.table = new Entry[capacity];
        this.loadFactor = LOAD_FACTOR;
        this.threshold = (int) loadFactor * capacity;
    }

    static void put(Object key, Class<?> type, Arg value) {
        if (!VM.isBooted()) return;
        getInstance().put_(key, type, value);
    }

    /**
     * Gets an ArgOptional from an object.
     *
     * @param key       the object
     * @param superType the superType of the object
     * @return an optional containing the arg or null.
     */
    public static Arg get(Object key, Class<?> superType) {
        if (!VM.isBooted()) return null;
        return getInstance().get_(key, superType);
    }

    static ArgOptional internalGet(Object key, Class<?> superType) {
        return ArgOptional.ofNullable(get(key, superType));
    }

    private static ArgMap getInstance() {
        Holder.INSTANCE.lock.lock();
        try {
            return Holder.INSTANCE;
        } finally {
            Holder.INSTANCE.lock.unlock();
        }
    }

    private static int tableSizeFor(int cap) {
        int n = -1 >>> Integer.numberOfLeadingZeros(cap - 1);
        return (n < 0) ? 1 : (n >= MAXIMUM_CAPACITY) ? MAXIMUM_CAPACITY : n + 1;
    }

    private static int bucketIndex(int h, int length) {
        return h & (length - 1);
    }


    private void put_(Object key, Class<?> type, Arg value) {
        Utils.requireNonNull(key);
        Utils.requireNonNull(type);
        Utils.requireNonNull(value);

        var hash = hash(key);
        var table = getTable();
        var bucketIndex = bucketIndex(hash, table.length);
        var head = table[bucketIndex];
        var entry = new Entry(key, head, queue);
        entry.value.addArg(type, value);
        table[bucketIndex] = entry;
        size++;
        if (size > threshold) {
            resize(table.length * 2);
        }
    }

    private void resize(int newSize) {
        if (table.length == MAXIMUM_CAPACITY) {
            threshold = Integer.MAX_VALUE;
            return;
        }

        var newTable = new Entry[newSize];
        transfer(table, newTable);
        table = newTable;
        threshold = (int) (newSize * loadFactor);
    }

    private void transfer(Entry[] oldTable, Entry[] newTable) {
        for (var i = 0; i < oldTable.length; i++) {
            var entry = oldTable[i];
            oldTable[i] = null;
            while (entry != null) {
                var next = entry.next;
                if (entry.refersTo(null)) {
                    entry.next = null;
                    entry.value = null;
                    size--;
                } else {
                    var index = bucketIndex(hash(entry.get()), newTable.length);
                    entry.next = newTable[index];
                    newTable[index] = entry;
                }
                entry = next;
            }
        }
    }

    private Arg get_(Object key, Class<?> superType) {
        Utils.requireNonNull(key);
        Utils.requireNonNull(superType);
        var hash = hash(key);
        var table = getTable();
        var bucketIndex = bucketIndex(hash, table.length);
        var head = table[bucketIndex];
        for (var current = head; current != null; current = current.next) {
            if (current.get() == key) {
                return current.value.arg(superType);
            }
        }
        return null;
    }

    private Entry[] getTable() {
        purgeEntries();
        return table;
    }

    private void purgeEntries() {
        for (var o = queue.poll(); o != null; o = queue.poll()) {
            var entry = (Entry) o;
            var bucketIndex = bucketIndex(hash(entry.get()), table.length);

            var prev = table[bucketIndex];
            var current = prev;
            while (current != null) {
                var next = current.next;
                if (current != entry) {
                    prev = current;
                    current = next;
                    continue;
                }

                if (prev == entry) {
                    table[bucketIndex] = next;
                } else {
                    prev.next = next;
                }
                entry.value = null;
                size--;
                break;
            }
        }
    }

    private int hash(Object key) {
        int h = System.identityHashCode(key);

        h ^= (h >>> 20) ^ (h >>> 12);
        return h ^ (h >>> 7) ^ (h >>> 4);
    }

    private static final class Entry extends WeakReference<Object> {

        private ArgPerType value = new ArgPerType();

        private Entry next;

        Entry(Object key, Entry next, ReferenceQueue<Object> queue) {
            super(key, queue);
            Utils.requireNonNull(value);
            this.next = next;
        }


    }

    private static final class ArgPerType {

        private ArgEntry[] entries = new ArgEntry[4];
        private byte size;

        public Arg arg(Class<?> type) {
            for (var i = 0; i < size; i++) {
                var entry = entries[i];
                if (entry.type() == type) {
                    return entry.value();
                }
            }
            return null;
        }

        public void addArg(Class<?> type, Arg arg) {
            if (size == entries.length) {
                grow();
            }
            entries[size++] = new ArgEntry(type, arg);
        }

        private void grow() {
            if (size == 127) {
                throw new AssertionError("Too many entries");
            }
            var newLength = entries.length == 64 ? 127 : entries.length * 2;
            var newArray = new ArgEntry[newLength];
            System.arraycopy(entries, 0, newArray, 0, size);
            entries = newArray;
        }

        private static final class ArgEntry {
            private final Class<?> type;
            private final Arg value;

            private ArgEntry(Class<?> type, Arg value) {
                this.type = type;
                this.value = value;
            }

            public Class<?> type() {
                return type;
            }

            public Arg value() {
                return value;
            }

        }

    }

}
