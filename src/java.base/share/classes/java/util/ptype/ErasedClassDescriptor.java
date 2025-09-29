package java.util.ptype;


import jdk.internal.vm.annotation.Stable;

import java.lang.reflect.Type;

/// Represent the erased type, used in to represent rawtypes.
@PrototypeInternal
public final class ErasedClassDescriptor implements TypeDescriptor {

    private static final ErasedClassDescriptor INSTANCE = new ErasedClassDescriptor();

    @Stable
    private final Properties properties = new Properties(false, true);

    /// Gets the instance of erased type.
    ///
    /// @return the instance
    public static ErasedClassDescriptor instance() {
        return INSTANCE;
    }

    @Override
    public String toString() {
        return TypeDescriptorUtils.stringify(this);
    }

    @Override
    public Type asType() {
        throw new AssertionError("Should never be called.");
    }

    @Override
    public Properties properties() {
        return properties;
    }

    private ErasedClassDescriptor() {
        if (INSTANCE != null) throw new AssertionError();
    }

}
