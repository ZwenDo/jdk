package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

/// A basic cache for classes with a single type parameter.
public final class SimpleDescriptorCache {

    @Stable
    private final HashMap<TypeDescriptor, ClassDescriptor> inner;
    @Stable
    private final Function<TypeDescriptor, ClassDescriptor> mapper;

    /// Creates a new instance for the given type.
    ///
    /// @param rawType the raw class
    public SimpleDescriptorCache(Class<?> rawType) {
        Utils.requireNonNull(rawType);
        this.inner = new HashMap<>();
        this.mapper = new Function<>() {
            @Stable
            private final Class<?> type = rawType;

            @Override
            public ClassDescriptor apply(TypeDescriptor input) {
                Utils.requireNonNull(input);
                var result = ClassDescriptor.ofInternal(type, 0, false, new TypeDescriptor[] {input});
                Analytics.reportUsage(result);
                return result;
            }
        };
    }

    /// Gets the [ClassDescriptor] for the given `argument`.
    ///
    /// @param argument the argument
    /// @return the corresponding descriptor
    public ClassDescriptor get(TypeDescriptor argument) {
        Utils.requireNonNull(argument);
        return inner.computeIfAbsent(argument, mapper);
    }

}
