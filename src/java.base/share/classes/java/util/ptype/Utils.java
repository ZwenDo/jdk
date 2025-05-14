package java.util.ptype;

final class Utils {

    static void requireNonNull(Object o) {
        if (o == null) {
            throw new NullPointerException();
        }
    }

    static ArgList.ArgPredicate isAssignableLambdaExpected(Arg expected, Arg.Variance variance) {
        requireNonNull(expected);
        requireNonNull(variance);
        return new ArgList.ArgPredicate() {
            @Override
            public boolean test(Arg arg) {
                requireNonNull(arg);
                return expected.isAssignable(arg, variance);
            }
        };
    }

    static ArgList.ArgPredicate isAssignableLambdaActual(Arg actual, Arg.Variance variance) {
        requireNonNull(actual);
        requireNonNull(variance);
        return new ArgList.ArgPredicate() {
            @Override
            public boolean test(Arg arg) {
                requireNonNull(arg);
                return arg.isAssignable(actual, variance);
            }
        };
    }

    static ArgList.IntArgBiConsumer appendListLambda(StringBuilder builder, int size, String separator) {
        requireNonNull(builder);
        if (size < 0) {
            throw new IllegalArgumentException("size < 0");
        }
        requireNonNull(separator);
        return new ArgList.IntArgBiConsumer() {
            @Override
            public void accept(int index, Arg arg) {
                requireNonNull(arg);
                arg.appendTo(builder);
                if (index < size - 1) {
                    builder.append(separator);
                }
            }
        };
    }

    private Utils() {
        throw new AssertionError();
    }


}
