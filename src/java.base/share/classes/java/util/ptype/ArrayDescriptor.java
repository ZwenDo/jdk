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
        Analytics.report(this, Analytics.Kind.CREATED);
    }

    /// Creates a new array type.
    ///
    /// @param componentType the component type
    /// @return the created type or [ErasedClassDescriptor] if the component type is erased
    @PrototypeInternal
    public static TypeDescriptor of(TypeDescriptor componentType) {
        Utils.requireNonNull(componentType);
        if (componentType == ErasedClassDescriptor.instance()) { // TODO log about the raw array
            return ErasedClassDescriptor.instance();
        }
        var created = new ArrayDescriptor(componentType);
        Analytics.report(created, Analytics.Kind.USED);
        return created;
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

    static TypeDescriptor ofConstant(TypeDescriptor componentType) {
        if (componentType == ErasedClassDescriptor.instance()) { // TODO log about the raw array
            return ErasedClassDescriptor.instance();
        }
        return new ArrayDescriptor(componentType);
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
