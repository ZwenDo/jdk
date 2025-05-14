package java.util.ptype;

import java.util.NoSuchElementException;


final class ArgOptional {

    private final Arg arg;

    private static final ArgOptional EMPTY = new ArgOptional(null);

    private ArgOptional(Arg arg) {
        this.arg = arg;
    }

    static ArgOptional of(Arg arg) {
        Utils.requireNonNull(arg);
        return new ArgOptional(arg);
    }

    static ArgOptional ofNullable(Arg arg) {
        return arg == null ? EMPTY : new ArgOptional(arg);
    }

    static ArgOptional empty() {
        return EMPTY;
    }

    Arg get() {
        if (arg == null) {
            throw new NoSuchElementException();
        }
        return arg;
    }

    boolean isPresent() {
        return arg != null;
    }

    boolean isEmpty() {
        return arg == null;
    }

}
