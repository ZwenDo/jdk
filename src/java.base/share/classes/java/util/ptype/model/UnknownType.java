package java.util.ptype.model;


import java.util.ptype.SpecializedTypeUtils;

/// The unknown type
public final class UnknownType implements SpecializedTypeDescriptor {

    private static final UnknownType INSTANCE = new UnknownType();

    /// Gets the instance of unknown type.
    ///
    /// @return the instance
    public static UnknownType instance() {
        return INSTANCE;
    }

    private UnknownType() {
        if (INSTANCE != null) throw new AssertionError();
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

}
