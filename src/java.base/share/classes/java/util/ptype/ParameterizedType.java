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
     * @param type  the raw type
     * @param args  the type arguments
     * @return the {@link ParameterizedType}
     */
    static ParameterizedType of(Class<?> type, Arg... args) {
        Utils.requireNonNull(type);
        Utils.requireNonNull(args);
        if (args.length == 0) {
            throw new IllegalArgumentException("args is empty");
        }
        var argsCopy = ArgList.of(args);
        return new ParameterizedType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                builder.append(rawType().getSimpleName());
                builder.append("<");
                typeArgs().forEachIndexed((index, arg) -> {
                    arg.appendTo(builder);
                    if (index < argsCopy.size() - 1) {
                        builder.append(", ");
                    }
                });
                builder.append(">");
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Utils.requireNonNull(actual);
                return switch (actual) {
                    case ParameterizedType parameterizedType -> {
                        if ( // return false if the raw types are not assignable or if the number of type args is different
                            !type.equals(parameterizedType.rawType())
                            || argsCopy.size() != parameterizedType.typeArgs().size()
                        ) {
                            yield false;
                        }

                        var expectedIt = argsCopy.iterator();
                        var actualIt = parameterizedType.typeArgs().iterator();
                        while (expectedIt.hasNext()) {
                            var exNext = expectedIt.next();
                            var acNext = actualIt.next();
                            if (!exNext.isAssignable(acNext)) {
                                yield false;
                            }
                        }

                        yield true;
                    }
                    case RawType rawType -> type.equals(rawType.type());
                    case Wildcard wildcard -> wildcard.upperBound().anyMatch(this::isAssignable);
                    case Intersection intersection -> intersection.bounds().allMatch(this::isAssignable);
                    case InnerClassType ignored -> false;
                    case ClassType ignored -> false;
                    case ArrayType ignored -> false;
                };
            }

            @Override
            public Class<?> rawType() {
                return type;
            }

            @Override
            public ArgList typeArgs() {
                return argsCopy;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
