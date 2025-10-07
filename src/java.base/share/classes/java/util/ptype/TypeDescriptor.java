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

    //endregion

    //region internal methods

    /// Class representing the properties of a [TypeDescriptor].
    @PrototypeInternal
    final class Properties {

        @Stable
        private final int props;

        Properties(
                boolean isFull,
                boolean isConstant
        ) {
            var props = Properties.DEFAULT;
            if (isFull) {
                props |= Properties.FULL;
            }
            if (isConstant) {
                props |= Properties.CONSTANT;
            }
            this.props = props;
        }

        private Properties(int props) {
            this.props = props;
        }

        static Properties computeTransitiveFlags(TypeDescriptor[] arguments, Properties base) {
            var props = base;
            for (var argument : arguments) {
                props = argument.properties().merge(props);
            }
            return props;
        }

        Properties merge(Properties other) {
            Utils.requireNonNull(other);
            var flags = DEFAULT;
            if (isConstant() && other.isConstant()) {
                flags |= CONSTANT;
            }
            if (isFull() && other.isFull()) {
                flags |= FULL;
            }
            return new Properties(flags);
        }

        boolean isFull() {
            return hasProperty(FULL);
        }

        boolean isConstant() {
            return hasProperty(CONSTANT);
        }

        private boolean hasProperty(int property) {
            return (property & props) != 0;
        }

        @Override
        public String toString() {
            var builder = new StringBuilder();
            builder.append("{");

            if (hasProperty(FULL)) {
                builder.append("FULL, ");
            }

            if (hasProperty(CONSTANT)) {
                builder.append("CONSTANT, ");
            }

            builder.append('}');
            return builder.toString();
        }

        private static final int DEFAULT = 1;

        private static final int FULL = 1 << 1;

        private static final int CONSTANT = 1 << 2;

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
        return descriptor == null || !descriptor.properties().isFull() ? null : descriptor;
    }

    /// Filters the partially raw descriptors
    ///
    /// @param descriptor the descriptor to filter
    /// @return the descriptor or an empty optional
    /// @param <T> the type of the descriptor
    @PrototypeInternal
    static <T extends TypeDescriptor> Optional<T> filterPartialDescriptor(T descriptor) {
        return descriptor == null || !descriptor.properties().isFull()
                ? Optional.empty()
                : Optional.of(descriptor);
    }

    //endregion

}
