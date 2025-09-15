package java.util.ptype;


/// Represent the erased type, used in to represent rawtypes.
public final class ErasedType implements SpecializedTypeDescriptor {

    private static final ErasedType INSTANCE = new ErasedType();

    /// Gets the instance of erased type.
    ///
    /// @return the instance
    public static ErasedType instance() {
        return INSTANCE;
    }

    private ErasedType() {
        if (INSTANCE != null) throw new AssertionError();
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }
}
