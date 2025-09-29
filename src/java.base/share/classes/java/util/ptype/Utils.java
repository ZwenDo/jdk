package java.util.ptype;


final class Utils {

    public static <T> T requireNonNull(T o) {
        if (o == null) {
            throw new IllegalArgumentException("Argument is null.");
        }
        return o;
    }

    public static void checkIndex(int index, int length) {
        if (index < 0 || index >= length) {
            throw new IllegalArgumentException("Index " + index + " is out of bounds for length " + length);
        }
    }

    public static int hashCode(Object o) {
        return o != null ? o.hashCode() : 0;
    }

    public static boolean equals(Object a, Object b) {
        return (a == b) || (a != null && a.equals(b));
    }

    public static boolean arrayEquals(Object[] a, Object[] a2) {
        if (a==a2)
            return true;
        if (a==null || a2==null)
            return false;

        int length = a.length;
        if (a2.length != length)
            return false;

        for (int i=0; i<length; i++) {
            if (!Utils.equals(a[i], a2[i]))
                return false;
        }

        return true;
    }

    public static int arrayHashCode(Object[] array) {
        if (array == null) return 0;

        var result = 1;
        for (var element : array) {
            result = 31 * result + Utils.hashCode(element);
        }

        return result;
    }

    private Utils() {
        throw new AssertionError();
    }

}
