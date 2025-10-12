package java.util.ptype;


final class Utils {

    public static String join(Object... args) {
        var builder = new StringBuilder();
        for (var arg : args) {
            builder.append(arg.toString());
        }
        return builder.toString();
    }

    public static <T> T requireNonNull(T o) {
        if (o == null) {
            throw new IllegalArgumentException("Argument is null.");
        }
        return o;
    }

    public static void checkIndex(int index, int length) {
        if (index < 0 || index >= length) {
            var message = Utils.join("Index ", index, " is out of bounds for length ", length);
            throw new IllegalArgumentException(message);
        }
    }

    public static int hashCode(Object o) {
        return o != null ? o.hashCode() : 0;
    }

    public static boolean equals(Object a, Object b) {
        return (a == b) || (a != null && a.equals(b));
    }

    public static boolean arrayEquals(Object[] a, Object[] a2) {
        return arrayEquals(a, a2, DEFAULT);
    }

    public static int arrayHashCode(Object[] array) {
        return arrayHashCode(array, DEFAULT);
    }

    public static <T> boolean arrayEquals(T[] array, T[] array2, Equivalence<? super T> equivalence) {
        Utils.requireNonNull(equivalence);
        if (array == array2)
            return true;
        if (array == null || array2 == null)
            return false;

        int length = array.length;
        if (array2.length != length)
            return false;

        for (int i = 0; i < length; i++) {
            if (!equivalence.equals(array[i], array2[i]))
                return false;
        }

        return true;
    }

    public static <T> int arrayHashCode(T[] array, Equivalence<? super T> equivalence) {
        Utils.requireNonNull(equivalence);
        if (array == null) return 0;

        var result = 1;
        for (var element : array) {
            result = 31 * result + equivalence.hash(element);
        }

        return result;
    }

    private static final Equivalence<Object> DEFAULT = new Equivalence<>() {
        @Override
        public int hash(Object obj) {
            return Utils.hashCode(obj);
        }

        @Override
        public boolean equals(Object obj, Object other) {
            return Utils.equals(obj, obj);
        }
    };

    private Utils() {
        throw new AssertionError();
    }

}
