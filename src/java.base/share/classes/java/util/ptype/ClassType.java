package java.util.ptype;


/**
 * Represents a class type.
 */
public non-sealed interface ClassType extends Arg {

    /**
     * Gets the {@link Class} of this type.
     *
     * @return the {@link Class}
     */
    Class<?> type();

    /**
     * Creates a {@link ClassType} from the given {@link Class}.
     *
     * @param type the {@link Class}
     * @return the {@link ClassType}
     */
    static ClassType of(Class<?> type) {
        Utils.requireNonNull(type);
        return new ClassType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                builder.append(type.getSimpleName());
            }

            @Override
            public boolean isAssignable(Arg actual, Variance variance) {
                Utils.requireNonNull(actual);
                Utils.requireNonNull(variance);
                if (variance == Variance.INVARIANT) { // invariant
                    return actual instanceof ClassType classType && type.equals(classType.type());
                }

                if (actual instanceof ClassType classType) {
                    return compareClass(classType.type(), variance);
                } else if (actual instanceof Intersection intersection) {
                    return intersection.bounds().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                } else if (actual instanceof Wildcard wildcard) {
                    if (variance == Variance.COVARIANT) {
                        return wildcard.upperBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                    } else if (variance == Variance.CONTRAVARIANT) {
                        return wildcard.lowerBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                    }
                    throw new IllegalArgumentException();
                } else if (actual instanceof RawType rawType) {
                    return compareClass(rawType.type(), variance);
                } else if (actual instanceof ArrayType) {
                    return false;
                } else if (actual instanceof InnerClassType innerClassType) {
                    return isAssignable(innerClassType.innerType(), variance);
                } else if (actual instanceof ParameterizedType parameterizedType) {
                    return compareClass(parameterizedType.rawType(), variance);
                }
                throw new IllegalArgumentException();
            }

            private boolean compareClass(Class<?> clazz, Variance variance) {
                if (variance == Variance.INVARIANT) {
                    throw new AssertionError("Should not reach here");
                } else if (variance == Variance.COVARIANT) {
                    return type.isAssignableFrom(clazz);
                } else if (variance == Variance.CONTRAVARIANT) {
                    return clazz.isAssignableFrom(type);
                }
                throw new IllegalArgumentException();
            }

            @Override
            public Class<?> type() {
                return type;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

    /**
     * Creates a {@link ClassType} from the given class name. This method is used in cases where the class is not
     * accessible through the class literal syntax due to visibility constraints.
     *
     * @param className the name of the class
     * @return the {@link ClassType}
     */
    static ClassType of(String className) {
        Utils.requireNonNull(className);
        return of(Internal.findClass(className));
    }

}
