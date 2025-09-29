package java.util.ptype;

final class ArrayList<E> {

    @SuppressWarnings("unchecked")
    private E[] elements = (E[]) new Object[16];

    private int size;

    public ArrayList() {
    }

    private ArrayList(E[] elements) {
        this.elements = elements;
        this.size = elements.length;
    }

    public void add(E e) {
        Utils.requireNonNull(e);
        if (size == elements.length) {
            grow();
        }
        elements[size++] = e;
    }

    public E get(int index) {
        Utils.checkIndex(index, size);
        return elements[index];
    }

    public int size() {
        return size;
    }

    public E[] toArray(Function<Integer, E[]> arrayFactory) {
        var array = arrayFactory.apply(size);
        System.arraycopy(elements, 0, array, 0, size);
        return array;
    }

    public static <E> ArrayList<E> from(E[] elements) {
        Utils.requireNonNull(elements);
        for (var element : elements) {
            Utils.requireNonNull(element);
        }
        return new ArrayList<>(elements);
    }

    private void grow() {
        var elements = this.elements;
        @SuppressWarnings("unchecked")
        var newArray = (E[]) new Object[elements.length * 2];
        System.arraycopy(elements, 0, newArray, 0, elements.length);
        this.elements = newArray;
    }

    @Override
    public String toString() {
        var builder = new StringBuilder();
        builder.append('[');
        for (int i = 0; i < size; i++) {
            builder.append(elements[i]);
            if (i + 1 < size) {
                builder.append(", ");
            }
        }
        builder.append(']');
        return builder.toString();
    }
}
