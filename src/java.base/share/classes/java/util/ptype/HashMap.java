package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

/// Immutable hashmap implementation.
///
/// @param <K> the type of the keys
/// @param <V> the type of the values
final class HashMap<K,V>  {

    @Stable
    private final Object[] table;

    @Stable
    private final int size;

    /// Creates a new hashmap from the given entries.
    ///
    /// @param input the entries to populate the map
    private HashMap(Object... input) {
        if ((input.length & 1) != 0) {
            throw new IllegalArgumentException("length is odd");
        }
        size = input.length >> 1;

        int len = 2 * input.length;
        len = (len + 1) & ~1; // ensure table is even length
        table = new Object[len];

        for (int i = 0; i < input.length; i += 2) {
            @SuppressWarnings("unchecked")
            var k = java.util.ptype.Utils.requireNonNull((K) input[i]);
            @SuppressWarnings("unchecked")
            var v = java.util.ptype.Utils.requireNonNull((V) input[i + 1]);
            int idx = probe(k);
            if (idx >= 0) {
                var message = Utils.join("duplicate key: ", k);
                throw new IllegalArgumentException(message);
            } else {
                int dest = -(idx + 1);
                table[dest] = k;
                table[dest + 1] = v;
            }
        }
    }

    private HashMap(int size, Object[] table) {
        this.size = size;
        this.table = table;
    }

    /// Creates a new hashmap from the given super type mappings.
    ///
    /// @param input the super type mappings
    /// @return the created hashmap
    public static HashMap<Class<?>, ClassDescriptor> superTypeMap(HashSet<ClassDescriptor> input) {
        int len = 4 * input.size();
        len = (len + 1) & ~1; // ensure table is even length
        var table = new Object[len];

        for (var it = input.iterator(); it.hasNext();) {
            var descriptor = it.next();
            int idx = probeStatic(descriptor.type(), table);
            if (idx >= 0) {
                var message = Utils.join("duplicate key: ", descriptor.type());
                throw new IllegalArgumentException(message);
            } else {
                int dest = -(idx + 1);
                table[dest] = descriptor.type();
                table[dest + 1] = descriptor;
            }
        }

        return new HashMap<>(input.size(), table);
    }

    public static <I, K, V> HashMap<K, V> of(I[] input, Function<I, K> keyMapper, Function<I, V> valueMapper) {
        if ((input.length & 1) != 0) {
            throw new IllegalArgumentException("length is odd");
        }
        var size = input.length >> 1;

        int len = 2 * input.length;
        len = (len + 1) & ~1; // ensure table is even length
        var table = new Object[len];

        for (int i = 0; i < input.length; i++) {
            var k = keyMapper.apply(Utils.requireNonNull(input[i]));
            var v = valueMapper.apply(Utils.requireNonNull(input[i + 1]));
            int idx = probeStatic(k, table);
            if (idx >= 0) {
                var message = Utils.join("duplicate key: ", k);
                throw new IllegalArgumentException(message);
            } else {
                int dest = -(idx + 1);
                table[dest] = k;
                table[dest + 1] = v;
            }
        }

        return new HashMap<>(size, table);
    }

    @SuppressWarnings("unchecked")
    static <K, V> HashMap<K, V> empty() {
        return (HashMap<K, V>) EMPTY;
    }

    /// Gets the value associated to a given key
    ///
    /// @param o the key
    /// @return the associated value or null if the key does not exist
    @SuppressWarnings("unchecked")
    public V get(Object o) {
        if (size == 0) {
            Utils.requireNonNull(o);
            return null;
        }
        int i = probe(o);
        if (i >= 0) {
            return (V)table[i+1];
        } else {
            return null;
        }
    }

    /// Tests whether the map is empty.
    ///
    /// @return true if the map is empty; false otherwise.
    public boolean isEmpty() {
        return size == 0;
    }

    private int probe(Object pk) {
        return probeStatic(pk, table);
    }

    private static <K> int probeStatic(Object pk, Object[] table) {
        int idx = Math.floorMod(pk.hashCode(), table.length >> 1) << 1;
        while (true) {
            @SuppressWarnings("unchecked")
            K ek = (K)table[idx];
            if (ek == null) {
                return -idx - 1;
            } else if (pk.equals(ek)) {
                return idx;
            } else if ((idx += 2) == table.length) {
                idx = 0;
            }
        }
    }

    private static final HashMap<?, ?> EMPTY = new HashMap<>();

}