package java.util.ptype;

final class TypeDescriptorCaching {

    private static final HashSet<ClassDescriptor> CLASS_DESCRIPTOR_CACHE = new HashSet<>();
    private static final HashSet<MethodDescriptor> METHOD_DESCRIPTOR_CACHE = new HashSet<>();
    private static final HashSet<ArrayDescriptor> ARRAY_DESCRIPTOR_CACHE = new HashSet<>();
    private static final HashSet<HiddenClassDescriptor> HIDDEN_CLASS_CACHE = new HashSet<>();

    static ClassDescriptor cache(ClassDescriptor descriptor) {
        Utils.requireNonNull(descriptor);
        return cache(CLASS_DESCRIPTOR_CACHE, descriptor);
    }

    static HiddenClassDescriptor cache(HiddenClassDescriptor descriptor) {
        Utils.requireNonNull(descriptor);
        return cache(HIDDEN_CLASS_CACHE, descriptor);
    }

    static ArrayDescriptor cache(ArrayDescriptor descriptor) {
        Utils.requireNonNull(descriptor);
        return cache(ARRAY_DESCRIPTOR_CACHE, descriptor);
    }

    static MethodDescriptor cache(MethodDescriptor descriptor) {
        Utils.requireNonNull(descriptor);
        return cache(METHOD_DESCRIPTOR_CACHE, descriptor);
    }

    private static <T> T cache(HashSet<T> cache, T element) {
        T old;
        synchronized (cache) {
            old = cache.add(element);
        }
        if (old != null) {
            return old;
        }
        return element;
    }

    private TypeDescriptorCaching() {
        throw new AssertionError();
    }

}
