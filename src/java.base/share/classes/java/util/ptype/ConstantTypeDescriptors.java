package java.util.ptype;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

/// Utility class for creating constant specialized type descriptors.
@PrototypeInternal
public final class ConstantTypeDescriptors {

    /// The flag that indicates that the queried descriptor is raw.
    public static final int FLAG_RAW = 1;

    /// Creates a new constant [ClassDescriptor].
    ///
    /// @param lookup       the lookup context (unused)
    /// @param variableName the name of the variable (unused)
    /// @param variableType the type of the variable (unused)
    /// @param rawType      the raw type
    /// @param flags        the flags describing the descriptor to create
    /// @param arguments    the extra arguments
    /// @return the created descriptor
    public static ClassDescriptor classDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<ClassDescriptor> variableType,
            String rawType,
            int flags,
            Object... arguments
    ) {
        Utils.requireNonNull(rawType);
        Utils.requireNonNull(arguments);

        var type = classFromName(lookup, rawType);

        if (arguments.length == 0) {
            var isRaw = (flags & FLAG_RAW) != 0;
            var instance = isRaw ? ClassDescriptor.ofRaw(type) : ClassDescriptor.of(type);
            return TypeDescriptorCaching.cache(instance);
        }

        var flattenedTypeArguments = new TypeDescriptor[arguments.length];
        System.arraycopy(arguments, 0, flattenedTypeArguments, 0, flattenedTypeArguments.length);

        var instance = ClassDescriptor.ofConstant(
                type,
                flattenedTypeArguments.length,
                flattenedTypeArguments
        );
        if (!instance.properties().is(TypeDescriptor.Properties.Property.CONSTANT)) {
            var message = Utils.join(instance, " should be constant.");
            throw new AssertionError(message);
        }
        return TypeDescriptorCaching.cache(instance);
    }

    /// Creates a new constant [HiddenClassDescriptor].
    ///
    /// @param lookup       the lookup context (unused)
    /// @param variableName the name of the variable (unused)
    /// @param variableType the type of the variable (unused)
    /// @param directSuperTypes    the extra directSuperTypes
    /// @return the created descriptor
    public static HiddenClassDescriptor hiddenClassDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<HiddenClassDescriptor> variableType,
            Object... directSuperTypes
    ) {
        Utils.requireNonNull(directSuperTypes);

        var descriptors = new ClassDescriptor[directSuperTypes.length];
        System.arraycopy(directSuperTypes, 0, descriptors, 0, descriptors.length);

        for (var descriptor : descriptors) {
            if (!descriptor.properties().is(TypeDescriptor.Properties.Property.CONSTANT)) {
                throw new AssertionError("The descriptor is not constant.");
            }
        }

        var instance = HiddenClassDescriptor.of(descriptors);
        return TypeDescriptorCaching.cache(instance);
    }

    /// Creates a new constant parameterized type descriptor.
    ///
    /// @param lookup        the lookup context (unused)
    /// @param variableName  the name of the variable (unused)
    /// @param variableType  the type of the variable (unused)
    /// @param componentType the component type
    /// @return the created type descriptor
    public static TypeDescriptor arrayDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<? extends TypeDescriptor> variableType,
            Object componentType
    ) {
        Utils.requireNonNull(componentType);
        var componentDesc = (TypeDescriptor) componentType;
        // we can safely cast, as to return erased, the component must have been extract, but here it is constant.
        return TypeDescriptorCaching.cache((ArrayDescriptor) ArrayDescriptor.of(componentDesc));
    }

    /// Returns the erased type descriptor.
    ///
    /// @param lookup       the lookup context (unused)
    /// @param variableName the name of the variable (unused)
    /// @param variableType the type of the variable (unused)
    /// @return the erased type descriptor
    public static ErasedClassDescriptor erasedClassDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<ErasedClassDescriptor> variableType
    ) {
        return ErasedClassDescriptor.instance();
    }

    /// Creates a new constant method descriptor.
    ///
    /// @param lookup        the lookup context (unused)
    /// @param variableName  the name of the variable (unused)
    /// @param variableType  the type of the variable (unused)
    /// @param typeArguments the type arguments
    /// @return the created type descriptor
    public static MethodDescriptor methodDescriptor(
            MethodHandles.Lookup lookup,
            String variableName,
            Class<MethodDescriptor> variableType,
            Object... typeArguments
    ) {
        if (typeArguments.length == 0) {
            return MethodDescriptor.of();
        }
        var args = new TypeDescriptor[typeArguments.length];
        System.arraycopy(typeArguments, 0, args, 0, typeArguments.length);
        return TypeDescriptorCaching.cache(MethodDescriptor.of(args));
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

    private static <T> T extractArg(Object[] args, int index, Class<T> type) {
        if (index >= args.length) {
            throw new IllegalArgumentException("missing argument");
        }
        Object result = Utils.requireNonNull(args[index]);
        if (!type.isInstance(result)) {
            throw new IllegalArgumentException("argument has wrong type");
        }
        return type.cast(result);
    }

    private ConstantTypeDescriptors() {
        throw new AssertionError();
    }

}
