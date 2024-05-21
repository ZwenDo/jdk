package java.util.ptype;

final class Utils {

    static void requireNonNull(Object o) {
        if (o == null) {
            throw new NullPointerException();
        }
    }

    private Utils() {
        throw new AssertionError();
    }


}
