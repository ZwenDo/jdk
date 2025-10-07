package java.util.ptype;


import jdk.internal.vm.annotation.Stable;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

import java.lang.reflect.Type;
import java.util.Optional;

/// Represents a class type.
public final class ClassDescriptor implements TypeDescriptor, TypeDescriptorAccessor, DerivedClassDescriptor {

    //region fields

    @Stable
    private final Class<?> type;

    @Stable
    private final TypeDescriptor[] arguments;

    @Stable
    private final int capturedTypeArgumentsStartIndex;

    @Stable
    HashMap<Class<?>, ClassDescriptor> superTypes;

    @Stable
    private Type javaType;

    @Stable
    private Properties properties;

    //endregion

    //region instantiation

    private ClassDescriptor(
            Class<?> type,
            int capturedTypeArgumentsStartIndex,
            TypeDescriptor[] arguments
    ) {
        Utils.requireNonNull(arguments);
        Utils.requireNonNull(arguments);
        Utils.checkIndex(capturedTypeArgumentsStartIndex, arguments.length + 1);

        this.type = type;
        this.capturedTypeArgumentsStartIndex = capturedTypeArgumentsStartIndex;
        this.arguments = arguments;
    }

    /// Creates a new raw [ClassDescriptor].
    ///
    /// @param type the type
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor ofRaw(Class<?> type) {
        Utils.requireNonNull(type);
        return new ClassDescriptor(type, 0, RAW_TYPE_ARGUMENTS);
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type the type
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(Class<?> type) {
        Utils.requireNonNull(type);
        return new ClassDescriptor(type, 0, EMPTY_ARRAY);
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type         the type
    /// @param captureStart the start index of the captured types
    /// @param arg1         the first type argument
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(
            Class<?> type,
            int captureStart,
            TypeDescriptor arg1
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.checkIndex(captureStart, 2);
        return new ClassDescriptor(type, captureStart, new TypeDescriptor[]{arg1});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type         the type
    /// @param captureStart the start index of the captured types
    /// @param arg1         the first type argument
    /// @param arg2         the second type argument
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(
            Class<?> type,
            int captureStart,
            TypeDescriptor arg1,
            TypeDescriptor arg2
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.checkIndex(captureStart, 3);
        return new ClassDescriptor(type, captureStart, new TypeDescriptor[]{arg1, arg2});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type         the type
    /// @param captureStart the start index of the captured types
    /// @param arg1         the first type argument
    /// @param arg2         the second type argument
    /// @param arg3         the third type argument
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(
            Class<?> type,
            int captureStart,
            TypeDescriptor arg1,
            TypeDescriptor arg2,
            TypeDescriptor arg3
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        Utils.checkIndex(captureStart, 4);
        return new ClassDescriptor(type, captureStart, new TypeDescriptor[]{arg1, arg2, arg3});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type         the type
    /// @param captureStart the start index of the captured types
    /// @param arg1         the first type argument
    /// @param arg2         the second type argument
    /// @param arg3         the third type argument
    /// @param arg4         the fourth type argument
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(
            Class<?> type,
            int captureStart,
            TypeDescriptor arg1,
            TypeDescriptor arg2,
            TypeDescriptor arg3,
            TypeDescriptor arg4
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        Utils.requireNonNull(arg4);
        Utils.checkIndex(captureStart, 5);
        return new ClassDescriptor(type, captureStart, new TypeDescriptor[]{arg1, arg2, arg3, arg4});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type         the type
    /// @param captureStart the start index of the captured types
    /// @param arg1         the first type argument
    /// @param arg2         the second type argument
    /// @param arg3         the third type argument
    /// @param arg4         the fourth type argument
    /// @param arg5         the fifth type argument
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(
            Class<?> type,
            int captureStart,
            TypeDescriptor arg1,
            TypeDescriptor arg2,
            TypeDescriptor arg3,
            TypeDescriptor arg4,
            TypeDescriptor arg5
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        Utils.requireNonNull(arg4);
        Utils.requireNonNull(arg5);
        Utils.checkIndex(captureStart, 6);
        return new ClassDescriptor(type, captureStart, new TypeDescriptor[]{arg1, arg2, arg3, arg4, arg5});
    }

    /// Creates a new [ClassDescriptor].
    ///
    /// @param type         the type
    /// @param captureStart the start index of the captured types
    /// @param arguments    the type arguments
    /// @return the created descriptor
    @PrototypeInternal
    public static ClassDescriptor of(
            Class<?> type,
            int captureStart,
            TypeDescriptor... arguments
    ) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(arguments);
        Utils.checkIndex(captureStart, arguments.length + 1);

        if (arguments.length == 0) {
            return of(type);
        }

        var array = new TypeDescriptor[arguments.length];
        for (int i = 0; i < arguments.length; i++) {
            array[i] = Utils.requireNonNull(arguments[i]);
        }

        return new ClassDescriptor(type, captureStart, array);
    }

    //endregion

    //region public api

    /// Gets the type
    ///
    /// @return the type
    public Class<?> type() {
        return type;
    }

    @Override
    public TypeDescriptor typeArgument(int index) {
        if (isRaw()) {
            return ErasedClassDescriptor.instance();
        }
        Utils.checkIndex(index, capturedTypeArgumentsStartIndex);
        if (!hasTypeArguments()) {
            var message = Utils.join("Type ", type, " is not parameterized.");
            throw new IllegalArgumentException(message);
        }
        return arguments[index];
    }

    /// Views this descriptor as one of its super types.
    ///
    /// @param type the super type
    /// @return this descriptor as one of its super types
    public Optional<ClassDescriptor> asSuper(Class<?> type) {
        Utils.requireNonNull(type);
        return Optional.ofNullable(superDescriptor(type));
    }

    @Override
    public Type asType() {
        if (javaType != null) return javaType;

        // raw and plain classes
        if (isRaw() || capturedTypeArgumentsStartIndex == 0) {
            javaType = type;
            return javaType;
        }

        javaType = makeType(type, 0);
        return javaType;
    }

    @Override
    public Properties properties() {
        if (properties == null) {
            var props = new Properties(
                    arguments != RAW_TYPE_ARGUMENTS,
                    true
            );
            this.properties = Properties.computeTransitiveFlags(arguments, props);
        }
        return properties;
    }

    @Override
    public String toString() {
        return TypeDescriptorUtils.stringify(this);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ClassDescriptor other)) return false;
        return type.equals(other.type)
                && capturedTypeArgumentsStartIndex == other.capturedTypeArgumentsStartIndex
                && Utils.arrayEquals(arguments, other.arguments);
    }

    @Override
    public int hashCode() {
        var hash = 1;
        hash = 31 * hash + type.hashCode();
        hash = 31 * hash + capturedTypeArgumentsStartIndex;
        hash = 31 * hash + Utils.arrayHashCode(arguments);
        return hash;
    }
    //endregion

    //region internal methods
    @Override
    public ClassDescriptor viewAsSuper(Class<?> type) {
        Utils.requireNonNull(type);
        return superDescriptor(type);
    }

    /// Gets the argument at the given index
    ///
    /// @param index the index
    /// @return the argument at the index
    @PrototypeInternal
    public TypeDescriptor argument(int index) {
        if (isRaw()) return ErasedClassDescriptor.instance();
        Utils.checkIndex(index, arguments.length);
        return arguments[index];
    }

    boolean isRaw() {
        return arguments == RAW_TYPE_ARGUMENTS;
    }

    boolean hasTypeArguments() {
        return !isRaw() && capturedTypeArgumentsStartIndex > 0;
    }

    boolean hasArgument() {
        return arguments.length > 0;
    }

    private ClassDescriptor superDescriptor(Class<?> type) {
        if (type == this.type) return this;
        if (superTypes == null) {
            superTypes = SuperDescriptorComputing.buildSuperMap(this);
        }
        return superTypes.get(type);
    }

    private Type makeType(Class<?> current, int offset) {
        var typeParametersCount = current.getTypeParameters().length;
        Type outer = null;
        if (current.getEnclosingMethod() == null && current.getEnclosingMethod() == null) {
            var enclosingClass = current.getEnclosingClass();
            if (enclosingClass != null) {
                outer = makeType(enclosingClass, offset + typeParametersCount);
            }
        }


        var typeArguments = new Type[typeParametersCount];
        for (var i = 0; i < typeParametersCount; i++) {
            typeArguments[i] = arguments[offset + i].asType();
        }

        return ParameterizedTypeImpl.make(current, typeArguments, outer);
    }

    private static final TypeDescriptor[] EMPTY_ARRAY = new TypeDescriptor[0];

    private static final TypeDescriptor[] RAW_TYPE_ARGUMENTS = new TypeDescriptor[0];

    //endregion

}
