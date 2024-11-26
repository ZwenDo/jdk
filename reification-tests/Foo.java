public class Foo {

    public static final class Box<E> {
        private E value;
    }

    public static void foo(Box<? super Object> box, Object value) {
        box.value = value;
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        var box = new Box<Integer>();
        var box2 = (Box<? super Object>) (Box<?>) box;
        foo(box2, "aaaaa");
        System.out.println("Hello, World!");
    }

}
