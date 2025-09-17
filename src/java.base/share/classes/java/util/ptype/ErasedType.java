package java.util.ptype;


import java.lang.reflect.Type;

/// Represent the erased type, used in to represent rawtypes.
public final class ErasedType implements SpecializedTypeDescriptor {

    private static final ErasedType INSTANCE = new ErasedType();

    /// Gets the instance of erased type.
    ///
    /// @return the instance
    public static ErasedType instance() {
        return INSTANCE;
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

    @Override
    public Type asType() {
        throw new AssertionError("Should never be called.");
    }

    private ErasedType() {
        if (INSTANCE != null) throw new AssertionError();
    }
}
