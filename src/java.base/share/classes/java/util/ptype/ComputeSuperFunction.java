package java.util.ptype;

/// Function that computes the super types
@FunctionalInterface
public interface ComputeSuperFunction {

    /// Computes the super types for this descriptor
    ///
    //// @param self the base descriptor
    //// @param mapping the current mappings
    void computeSuper(/*ClassDescriptor self, HashSet<Mapping> mapping*/);

    /// Simple mapping of a super type to its descriptor.
    final class Mapping {

        private final Class<?> superType;
        private final ClassDescriptor superTypeDescriptor;

        /// Creates a new super type mapping.
        ///
        /// @param superType the super type
        /// @param superTypeDescriptor the super type descriptor
        public Mapping(Class<?> superType, ClassDescriptor superTypeDescriptor) {
            Utils.requireNonNull(superType);
            Utils.requireNonNull(superTypeDescriptor);
            this.superType = superType;
            this.superTypeDescriptor = superTypeDescriptor;
        }

        /// Returns the super type.
        ///
        /// @return the super type
        public Class<?> superType() {
            return superType;
        }

        /// Returns the super type descriptor.
        ///
        /// @return the super type descriptor
        public ClassDescriptor superTypeDescriptor() {
            return superTypeDescriptor;
        }

        @Override
        public int hashCode() {
            return superType.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof Mapping other)) {
                return false;
            }
            return superType.equals(other.superType);
        }

    }

}
