package java.util.ptype;

import java.util.NoSuchElementException;

public final class ArgOptional {

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
        return new ArgOptional(arg);
    }

    public static ArgOptional empty() {
        return EMPTY;
    }

    public Arg get() {
        if (arg == null) {
            throw new NoSuchElementException();
        }
        return arg;
    }


    public boolean isPresent() {
        return arg != null;
    }

    public boolean isEmpty() {
        return arg == null;
    }

}
