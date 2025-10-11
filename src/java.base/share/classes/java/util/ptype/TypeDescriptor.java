package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.lang.annotation.CompilerIntrinsic;
import java.lang.reflect.Type;
import java.util.Optional;

/// Supertype for all specialized types.
public sealed interface TypeDescriptor permits ArrayDescriptor, ClassDescriptor, ErasedClassDescriptor {

    //region public API

    /// Returns the [Type] representation of this [TypeDescriptor].
    ///
    /// @return the corresponding [Type]
    Type asType();

    /// Creates a [TypeDescriptor] from the given [T] type.
    ///
    /// @return the corresponding [TypeDescriptor]
    /// @param <T> the source type
    @CompilerIntrinsic
    static <T> Optional<TypeDescriptor> of() {
        var methodDescriptor = TypeDescriptorPassingHandler.methodTypeArguments();
        if (methodDescriptor == null) return Optional.empty();
        return filterPartialDescriptor(methodDescriptor.typeArgument(0));
    }

    /// Gets the [TypeDescriptor] from a given object.
    ///
    /// This method returns an empty optional if the `holder` is not specialized or if it is not a subtype of `type`, or
    /// if the `holder` is a hidden class instance.
    ///
    /// @param holder the object containing the descriptor
    /// @param type the type we want the returned descriptor to represent
    /// @return the class descriptor or an empty optional
    static Optional<ClassDescriptor> from(Object holder, Class<?> type) {
        Utils.requireNonNull(type);
        if (!(holder instanceof ClassDescriptorHolder h) || type.isHidden()) return Optional.empty();
        var descriptor = h.$descriptor().viewAsSuper(type);
        return filterPartialDescriptor(descriptor);
    }

    /// [TypeDescriptor] should implement a proper toString method.
    ///
    /// @return the string representation
    @Override
    String toString();

    /// [TypeDescriptor] should implement a proper hashCode method.
    ///
    /// @return the hash code
    @Override
    int hashCode();

    /// [TypeDescriptor] should implement a proper equals method.
    ///
    /// @param obj the other object
    /// @return true if equal
    @Override
    boolean equals(Object obj);

    //endregion

    //region internal methods

    /// Class representing the properties of a [TypeDescriptor].
    @PrototypeInternal
    final class Properties {

        @Stable
        private final int props;

        Properties(Property... props) {
            var value = Property.DEFAULT.value();
            for (var prop : props) {
                value |= prop.value();
            }
            this.props = value;
        }

        private Properties(int props) {
            this.props = props;
        }

        Properties with(Property property) {
            Utils.requireNonNull(property);
            return new Properties(props | property.value());
        }

        Properties merge(Properties other) {
            Utils.requireNonNull(other);
            return new Properties(merge(props, other));
        }

        Properties merge(TypeDescriptor[] arguments) {
            Utils.requireNonNull(arguments);
            if (arguments.length == 0) return this;
            var props = this.props;
            for (var argument : arguments) {
                props = merge(props, argument.properties());
            }
            return new Properties(props);
        }

        boolean is(Property property) {
            return hasProperty(props, property);
        }

        @Override
        public String toString() {
            var builder = new StringBuilder();
            builder.append("{");

            var addedOne = false;
            for (int i = 1; i < Property.ALL.length; i++) {
                var property = Property.ALL[i];
                if (is(property)) {
                    if (addedOne) {
                        builder.append(", ");
                    }
                    addedOne = true;
                    builder.append(property);
                }
            }

            builder.append('}');
            return builder.toString();
        }

        private static int merge(int self, Properties other) {
            var props = Property.DEFAULT.value();
            for (int i = 1; i < Property.ALL.length; i++) {
                var property = Property.ALL[i];
                if (hasProperty(self, property) && hasProperty(other.props, property)) {
                    props |= property.value();
                }
            }
            return props;
        }

        private static boolean hasProperty(int props, Property property) {
            return (property.value() & props) != 0;
        }

        enum Property {
            DEFAULT,
            FULL,
            CONSTANT,
            ;

            @Stable
            private static final Property[] ALL = Property.values();

            private int value() {
                return 1 << ordinal();
            }

        }

    }

    /// Returns the [Properties] of this [TypeDescriptor].
    ///
    /// @return the properties
    @PrototypeInternal
    Properties properties();

    /// Gets the [TypeDescriptor] from a given object.
    ///
    /// This method returns null if the `holder` is not specialized or if it is not a subtype of `type`, or
    /// if the `holder` is a hidden class instance.
    ///
    /// @param holder the object containing the descriptor
    /// @param type the type we want the returned descriptor to represent
    /// @return the class descriptor or null
    @PrototypeInternal
    static ClassDescriptor $from(Object holder, Class<?> type) {
        Utils.requireNonNull(type);
        if (!(holder instanceof ClassDescriptorHolder h) || type.isHidden()) return null;
        var descriptor = h.$descriptor().viewAsSuper(type);
        return (descriptor != null && descriptor.properties().is(Properties.Property.FULL))
                ? descriptor
                : null;
    }

    /// Filters the partially raw descriptors
    ///
    /// @param descriptor the descriptor to filter
    /// @return the descriptor or an empty optional
    /// @param <T> the type of the descriptor
    @PrototypeInternal
    static <T extends TypeDescriptor> Optional<T> filterPartialDescriptor(T descriptor) {
        return (descriptor != null && descriptor.properties().is(Properties.Property.FULL))
                ? Optional.of(descriptor)
                : Optional.empty();
    }

    //endregion

}
