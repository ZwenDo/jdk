package java.util.ptype.util;

/// Utility class.
public final class Utils {

    /// Checks whether a given value is null. If it is null, throws an exception.
    ///
    /// @param o the value to check
    /// @return the value is it is not null
    /// @param <T> the type of the value
    public static <T> T requireNonNull(T o) {
        if (o == null) {
            throw new NullPointerException();
        }
        return o;
    }

    private Utils() {
        throw new AssertionError();
    }

}
