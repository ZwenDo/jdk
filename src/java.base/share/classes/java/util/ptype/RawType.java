package java.util.ptype;

/**
 * Represents a raw type.
 */
public non-sealed interface RawType extends Arg {

    /**
     * Gets the raw type of this raw type.
     *
     * @return the raw type
     */
    Class<?> type();

    /**
     * Creates a {@link RawType} from the given raw type.
     * <p>
     * This method is only used for testing purposes. RawType should be created statically by the compiler, so this
     * dynamic method should never be called.
     *
     * @param type the raw type
     * @return the {@link RawType}
     */
    static RawType of(Class<?> type) {
        Utils.requireNonNull(type);
        return new RawType() {
            @Override
            public Class<?> type() {
                return type;
            }

            @Override
            public boolean isAssignable(Arg actual, Variance variance) {
                Utils.requireNonNull(actual);
                if (variance == Variance.INVARIANT) {
                    return (actual instanceof RawType rawType && type.equals(rawType.type()))
                           || (actual instanceof ParameterizedType parameterizedType && type.equals(parameterizedType.rawType()));
                }
                if (actual instanceof ParameterizedType parameterizedType) {
                    return compareClass(parameterizedType.rawType(), variance);
                } else if (actual instanceof RawType rawType) {
                    return compareClass(rawType.type(), variance);
                } else if (actual instanceof Intersection intersection) {
                    return intersection.bounds()
                        .anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                } else if (actual instanceof Wildcard wildcard) {
                    if (variance == Variance.COVARIANT) {
                        return wildcard.upperBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                    } else if (variance == Variance.CONTRAVARIANT) {
                        return wildcard.lowerBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                    }
                    throw new IllegalArgumentException();
                } else if (actual instanceof InnerClassType innerClassType) {
                    return isAssignable(innerClassType.innerType(), variance);
                } else if (actual instanceof ArrayType) {
                    return false;
                } else if (actual instanceof ClassType classType) {
                    return compareClass(classType.type(), variance);
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
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                builder.append(type.getSimpleName());
                builder.append("<raw type>");
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }
        };
    }

    /**
     * Creates a {@link RawType} from the given name of the raw type. This method is used in cases where the class is
     * accessible through the class literal syntax due to visibility constraints.
     *
     * @param name the name of the class
     * @return the {@link RawType}
     */
    static RawType of(String name) {
        Utils.requireNonNull(name);
        return of(Internal.findClass(name));
    }

}
