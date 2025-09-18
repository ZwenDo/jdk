package java.util.ptype;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.ptype.util.HashSet;
import java.util.ptype.util.Utils;

/// Utility class for creating constant specialized type descriptors.
public final class ConstantSpecializedTypes {

    private static final HashSet<Object> CACHE = new HashSet<>();

    /// Creates a new constant parameterized type descriptor.
    ///
    /// @param lookup        the lookup context (unused)
    /// @param variableName  the name of the variable (unused)
    /// @param variableType  the type of the variable (unused)
    /// @param outer         the outer class descriptor
    /// @param rawType       the raw type
    /// @param typeArguments the type arguments
    /// @return the created type descriptor
    public static SpecializedTypeDescriptor constantClassDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<SpecializedTypeDescriptor> variableType,
            Object outer,
            String rawType,
            Object... typeArguments
    ) {
        Utils.requireNonNull(outer);
        Utils.requireNonNull(rawType);
        return newClassDescriptor((ClassDescriptor) outer, classFromName(lookup, rawType), false, typeArguments);
    }

    /// Creates a new constant parameterized type descriptor.
    ///
    /// @param lookup        the lookup context (unused)
    /// @param variableName  the name of the variable (unused)
    /// @param variableType  the type of the variable (unused)
    /// @param rawType       the raw type
    /// @param typeArguments the type arguments
    /// @return the created type descriptor
    public static SpecializedTypeDescriptor constantClassDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<SpecializedTypeDescriptor> variableType,
            String rawType,
            Object... typeArguments
    ) {
        Utils.requireNonNull(rawType);
        return newClassDescriptor(null, classFromName(lookup, rawType), false, typeArguments);
    }

    /// Creates a new raw parameterized type descriptor.
    ///
    /// @param lookup       the lookup context (unused)
    /// @param variableName the name of the variable (unused)
    /// @param variableType the type of the variable (unused)
    /// @param outer        the outer class descriptor
    /// @param rawType      the raw type
    /// @return the created type descriptor
    public static SpecializedTypeDescriptor rawTypeDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<SpecializedTypeDescriptor> variableType,
            Object outer,
            Class<?> rawType
    ) {
        Utils.requireNonNull(outer);
        Utils.requireNonNull(rawType);
        return newClassDescriptor((ClassDescriptor) outer, rawType, true);
    }

    /// Creates a new raw parameterized type descriptor.
    ///
    /// @param lookup       the lookup context (unused)
    /// @param variableName the name of the variable (unused)
    /// @param variableType the type of the variable (unused)
    /// @param rawType      the raw type
    /// @return the created type descriptor
    public static SpecializedTypeDescriptor rawTypeDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<SpecializedTypeDescriptor> variableType,
            Class<?> rawType
    ) {
        Utils.requireNonNull(rawType);
        return newClassDescriptor(null, rawType, true);
    }

    /// Creates a new constant parameterized type descriptor.
    ///
    /// @param lookup        the lookup context (unused)
    /// @param variableName  the name of the variable (unused)
    /// @param variableType  the type of the variable (unused)
    /// @param componentType the component type
    /// @return the created type descriptor
    public static SpecializedTypeDescriptor constantArrayTypeDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<SpecializedTypeDescriptor> variableType,
            Object componentType
    ) {
        Utils.requireNonNull(componentType);
        var componentDesc = (SpecializedTypeDescriptor) componentType;
        var instance = ArrayDescriptor.of(componentDesc);
        var old = CACHE.add(instance);
        if (old != null) {
            return (SpecializedTypeDescriptor) old;
        }
        return instance;
    }

    /// Returns the erased type descriptor.
    ///
    /// @param lookup       the lookup context (unused)
    /// @param variableName the name of the variable (unused)
    /// @param variableType the type of the variable (unused)
    /// @return the erased type descriptor
    public static SpecializedTypeDescriptor erasedTypeDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<SpecializedTypeDescriptor> variableType
    ) {
        return ErasedType.instance();
    }

    /// Creates a new constant method descriptor.
    ///
    /// @param lookup        the lookup context (unused)
    /// @param variableName  the name of the variable (unused)
    /// @param variableType  the type of the variable (unused)
    /// @param typeArguments the type arguments
    /// @return the created type descriptor
    public static MethodDescriptor constantMethodDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<MethodDescriptor> variableType,
            Object... typeArguments
    ) {
        if (typeArguments.length == 0) {
            throw new IllegalArgumentException("Should have at least one argument");
        }
        var args = new SpecializedTypeDescriptor[typeArguments.length];
        System.arraycopy(typeArguments, 0, args, 0, typeArguments.length);
        var instance = new MethodDescriptor(args);
        var old = CACHE.add(instance);
        if (old != null) {
            return (MethodDescriptor) old;
        }
        return instance;
    }

    private static SpecializedTypeDescriptor newClassDescriptor(
            ClassDescriptor outer,
            Class<?> rawType,
            boolean isRaw,
            Object... typeArguments
    ) {
        SpecializedTypeDescriptor[] args;
        if (typeArguments.length != 0) {
            args = new SpecializedTypeDescriptor[typeArguments.length];
            System.arraycopy(typeArguments, 0, args, 0, typeArguments.length);
        } else {
            args = new SpecializedTypeDescriptor[0];
        }
        var instance = new ClassDescriptor(outer, rawType, isRaw, args);
        var old = CACHE.add(instance);
        if (old != null) {
            return (SpecializedTypeDescriptor) old;
        }
        return instance;
    }

    private static Class<?> classFromName(MethodHandles.Lookup lookup, String name) {
        switch (name) {
            case "I":
                return int.class;
            case "B":
                return byte.class;
            case "J":
                return long.class;
            case "S":
                return short.class;
            case "Z":
                return boolean.class;
            case "F":
                return float.class;
            case "D":
                return double.class;
            case "C":
                return char.class;
            case "V":
                throw new AssertionError("void should not be here");
            default:
                return MethodType.fromMethodDescriptorString(name, lookup.lookupClass().getClassLoader())
                        .returnType();
        }
    }

    private ConstantSpecializedTypes() {
        throw new AssertionError();
    }

}
