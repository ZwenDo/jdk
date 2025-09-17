package java.util.ptype;

import jdk.internal.vm.annotation.Stable;
import sun.reflect.generics.reflectiveObjects.GenericArrayTypeImpl;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ptype.util.Utils;

/// Represents an array type.
public final class ArrayDescriptor implements SpecializedTypeDescriptor {

    @Stable
    private final SpecializedTypeDescriptor componentType;

    @Stable
    private Type javaType;


    private ArrayDescriptor(SpecializedTypeDescriptor componentType) {
        Utils.requireNonNull(componentType);
        this.componentType = componentType;
    }

    /// Creates a new array type.
    ///
    /// @param componentType the component type
    /// @return the created type or [ErasedType] if the component type is erased
    public static SpecializedTypeDescriptor of(SpecializedTypeDescriptor componentType) {
        return componentType == ErasedType.instance() ? ErasedType.instance() : new ArrayDescriptor(componentType);
    }

    /// Gets the component type of this array type.
    ///
    /// @return the component type
    public SpecializedTypeDescriptor componentType() {
        return componentType;
    }

    @Override
    public String toString() {
        return SpecializedTypeUtils.stringify(this);
    }

    @Override
    public Type asType() {
        if (javaType != null) return javaType;
        var component = componentType.asType();
        switch (component) {
            case Class<?> cls:
                javaType = cls.arrayType();
                break;
            case ParameterizedType ptype:
                javaType = GenericArrayTypeImpl.make(ptype);
                break;
            case GenericArrayType gatype:
                javaType = GenericArrayTypeImpl.make(gatype);
                break;
            default:
                throw new AssertionError("Unknown component type: " + component);
        }
        return javaType;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof ArrayDescriptor that)) return false;

        return componentType.equals(that.componentType);
    }

    @Override
    public int hashCode() {
        return componentType.hashCode();
    }

}
