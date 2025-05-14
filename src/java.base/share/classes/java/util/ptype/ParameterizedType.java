package java.util.ptype;


/**
 * Represents a parameterized type.
 */
public non-sealed interface ParameterizedType extends Arg {

    /**
     * Gets the raw type of this parameterized type.
     *
     * @return the raw type
     */
    Class<?> rawType();

    /**
     * Gets the type arguments of this parameterized type.
     *
     * @return the type arguments
     */
    ArgList typeArgs();

    /**
     * Creates a {@link ParameterizedType} from the given raw type and type arguments.
     *
     * @param type the raw type
     * @param args the type arguments
     * @return the {@link ParameterizedType}
     */
    static ParameterizedType of(Class<?> type, Arg... args) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(args);
        if (args.length == 0) {
            throw new IllegalArgumentException("args is empty");
        }

        var typeArgs = ArgList.of(args);
        return new ParameterizedType() {

            @Override
            public boolean isAssignable(Arg actual, Variance variance) {
                Utils.requireNonNull(actual);
                Utils.requireNonNull(variance);
                if (variance == Variance.INVARIANT) {
                    // TODO ignore arguments ?
                    return (actual instanceof RawType rt && type.equals(rt.type()))
                        || (actual instanceof ParameterizedType parameterizedType && type.equals(parameterizedType.rawType())
                        && compareArguments(parameterizedType));
                }
                if (actual instanceof ParameterizedType parameterizedType) {
                    return compareClass(
                        parameterizedType.rawType(),
                        variance
                    ) && compareArguments(parameterizedType);
                    // TODO ignore arguments ?
                } else if (actual instanceof RawType rt) {
                    return compareClass(rt.type(), variance);
                } else if (actual instanceof Wildcard wildcard) {
                    if (variance == Variance.COVARIANT) {
                        return wildcard.upperBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                    } else if (variance == Variance.CONTRAVARIANT) {
                        return wildcard.lowerBound().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                    }
                    throw new IllegalArgumentException();
                } else if (actual instanceof Intersection intersection) {
                    return intersection.bounds().anyMatch(Utils.isAssignableLambdaExpected(this, variance));
                } else if (actual instanceof InnerClassType innerClassType) {
                    return isAssignable(innerClassType.innerType(), variance);
                } else if (actual instanceof ClassType classType) {
                    return compareClass(classType.type(), variance)
                        && Internal.staticArgs(classType.type()).anyMatch(Utils.isAssignableLambdaExpected(
                        this,
                        Variance.INVARIANT
                    ));
                } else if (actual instanceof ArrayType) {
                    return false;
                }
                throw new IllegalArgumentException();
            }

            private boolean compareArguments(ParameterizedType actual) {
                if (typeArgs.size() != actual.typeArgs().size()) {
                    return false;
                }
                var expectedIt = typeArgs.iterator();
                var actualIt = actual.typeArgs().iterator();
                while (expectedIt.hasNext()) {
                    var exNext = expectedIt.next();
                    var acNext = actualIt.next();
                    if (!exNext.isAssignable(acNext, Variance.INVARIANT)) { // always invariant
                        return false;
                    }
                }
                return true;
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
                builder.append(rawType().getSimpleName());
                builder.append("<");
                typeArgs().forEachIndexed(Utils.appendListLambda(builder, typeArgs().size(), ", "));
                builder.append(">");
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

            @Override
            public Class<?> rawType() {
                return type;
            }

            @Override
            public ArgList typeArgs() {
                return typeArgs;
            }

        };
    }

    /**
     * Creates a {@link ParameterizedType} from the name of the type and type arguments. This method is used in cases
     * where the class is not accessible through a class literal due to visibility constraints.
     *
     * @param name the name of the type
     * @param args the type arguments
     * @return the {@link ParameterizedType}
     */
    static ParameterizedType of(String name, Arg... args) {
        Utils.requireNonNull(name);
        Utils.requireNonNull(args);
        return of(Internal.findClass(name), args);
    }

}
