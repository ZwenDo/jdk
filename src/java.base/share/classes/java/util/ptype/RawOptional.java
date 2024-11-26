package java.util.ptype;

import java.util.NoSuchElementException;

final class RawOptional {

    private final Object value;

    private static final RawOptional EMPTY = new RawOptional(null);

    private RawOptional(Object value) {
        this.value = value;
    }

    public Object get() {
        if (value == null) {
            throw new NoSuchElementException();
        }
        return value;
    }

    public boolean isPresent() {
        return value != null;
    }

    static RawOptional of(Object value) {
        Utils.requireNonNull(value);
        return new RawOptional(value);
    }

    static RawOptional empty() {
        return EMPTY;
    }

    @Override
    public String toString() {
        return value != null
            ? "RawOptional(" + value + ")"
            : "RawOptional.empty";
    }
}
