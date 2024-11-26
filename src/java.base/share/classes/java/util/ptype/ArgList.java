package java.util.ptype;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.RandomAccess;

/**
 * Immutable array list of {@link Arg}s.
 */
public final class ArgList implements RandomAccess {

    private final Arg[] args;

    private static final ArgList EMPTY = new ArgList(new Arg[0]);

    private ArgList(Arg[] args) {
        this.args = args;
    }

    static ArgList of(Arg... args) {
        Utils.requireNonNull(args);
        if (args.length == 0) {
            return EMPTY;
        }
        var dest = new Arg[args.length];
        var i = 0;
        for (Arg arg : args) {
//            Utils.requireNonNull(arg);
            dest[i++] = arg;
        }
        return new ArgList(dest);
    }

    Arg get(int index) {
        Objects.checkIndex(index, args.length);
        return args[index];
    }

    int size() {
        return args.length;
    }

    boolean isEmpty() {
        return args.length == 0;
    }

    void forEachIndexed(IntArgBiConsumer consumer) {
        Utils.requireNonNull(consumer);
        for (var i = 0; i < args.length; i++) {
            consumer.accept(i, args[i]);
        }
    }

    boolean allMatch(ArgPredicate predicate) {
        Utils.requireNonNull(predicate);
        for (var arg : args) {
            if (!predicate.test(arg)) {
                return false;
            }
        }
        return true;
    }

    boolean anyMatch(ArgPredicate predicate) {
        Utils.requireNonNull(predicate);
        for (var arg : args) {
            if (predicate.test(arg)) {
                return true;
            }
        }
        return false;
    }

    ArgIterator iterator() {
        return new ArgIterator() {
            private int index = 0;
            @Override
            public boolean hasNext() {
                return index < args.length;
            }
            @Override
            public Arg next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                return args[index++];
            }
        };
    }

    @Override
    public String toString() {
        var builder = new StringBuilder();
        builder.append('[');
        for (var i = 0; i < args.length; i++) {
            args[i].appendTo(builder);
            if (i != args.length - 1) {
                builder.append(", ");
            }
        }
        builder.append(']');
        return builder.toString();
    }

    interface ArgIterator {

        boolean hasNext();

        Arg next();

    }


    @FunctionalInterface
    interface ArgConsumer {

        void accept(Arg arg);

    }

    @FunctionalInterface
    interface IntArgBiConsumer {

        void accept(int index, Arg arg);

    }

    @FunctionalInterface
    interface ArgPredicate {

        boolean test(Arg arg);

    }

}
