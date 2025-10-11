package java.util.ptype;

import jdk.internal.vm.annotation.Stable;
import sun.reflect.generics.reflectiveObjects.GenericArrayTypeImpl;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/// Represents an array type.
public final class ArrayDescriptor implements TypeDescriptor {

    @Stable
    private final TypeDescriptor componentType;

    @Stable
    private Type javaType;

    @Stable
    private Properties properties;

    private ArrayDescriptor(TypeDescriptor componentType) {
        Utils.requireNonNull(componentType);
        this.componentType = componentType;
        Analytics.log(this);
    }

    /// Creates a new array type.
    ///
    /// @param componentType the component type
    /// @return the created type or [ErasedClassDescriptor] if the component type is erased
    @PrototypeInternal
    public static TypeDescriptor of(TypeDescriptor componentType) {
        return componentType == ErasedClassDescriptor.instance() ? ErasedClassDescriptor.instance() : new ArrayDescriptor(componentType);
    }

    /// Gets the component type of this array type.
    ///
    /// @return the component type
    public TypeDescriptor componentType() {
        return componentType;
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
                var message = Utils.join("Unknown component type: ", component.toString());
                throw new AssertionError(message);
        }
        return javaType;
    }

    @Override
    public Properties properties() {
        if (properties == null) {
            this.properties = componentType.properties();
        }
        return properties;
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

    @Override
    public String toString() {
        return TypeDescriptorUtils.stringify(this);
    }

}
