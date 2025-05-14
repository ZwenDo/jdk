package java.util.ptype;

/**
 * Class used to defer pushing the arg information of an object after it has been initialized.
 */
public final class ConstructorTypeArgs {

    private Pair[] args = new Pair[INITIAL_CAPACITY];
    private int size;

    /**
     * Creates a new instance.
     */
    public ConstructorTypeArgs() {
    }

    /**
     * Flushes the current object to add all its args to the map. This method should be called right after the super
     * call of the object.
     *
     * @param source the object owning the args.
     */
    public void flush(Object source) {
        Utils.requireNonNull(source);
        if (args == null) return;

        var args = this.args;
        this.args = null;
        for (var arg : args) {
            if (arg == null) break;
            ArgMap.put(source, arg.type(), arg.arg());
        }
    }

    /**
     * Adds an arg in the list of args that will be pushed.
     *
     * @param arg the arg to push
     * @param type the type represented by the arg
     */
    public void add(Arg arg, Class<?> type) {
        Utils.requireNonNull(arg);
        Utils.requireNonNull(type);
        if (args == null) throw new AssertionError("already flushed");

        if (size == args.length) {
            var newArgs = new Pair[args.length * 2];
            System.arraycopy(args, 0, newArgs, 0, size);
            args = newArgs;
        }
        var pair = new Pair(arg, type);
        args[size] = pair;
        size++;
    }

    private static final class Pair {
        private final Arg arg;
        private final Class<?> type;

        private Pair(Arg arg, Class<?> type) {
            this.arg = arg;
            this.type = type;
        }

        public Arg arg() {
            return arg;
        }

        public Class<?> type() {
            return type;
        }
    }

    private static final int INITIAL_CAPACITY = 16;

}
