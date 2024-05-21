package java.util.ptype;

import java.util.NoSuchElementException;

public final class ArgOptional {

    private final Arg arg;

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
