package java.util.ptype;

interface Equivalence<T> {

    int hash(T obj);

    boolean equals(T obj, Object other);

    @SuppressWarnings("unchecked")
    static <T> Equivalence<T> natural() {
        final class Holder {
            private static final Equivalence<?> INSTANCE = new Equivalence<>() {
                @Override
                public int hash(Object obj) {
                    Utils.requireNonNull(obj);
                    return obj.hashCode();
                }

                @Override
                public boolean equals(Object obj, Object other) {
                    Utils.requireNonNull(obj);
                    return obj.equals(other);
                }
            };
        }

        return (Equivalence<T>) Holder.INSTANCE;
    }

}