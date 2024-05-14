package java.util.ptype;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

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
    List<Arg> typeArgs();

    /**
     * Creates a {@link ParameterizedType} from the given raw type and list of type arguments.
     *
     * @param type the raw type
     * @param args the type arguments
     * @return the {@link ParameterizedType}
     */
    static ParameterizedType of(Class<?> type, List<Arg> args) {
        Objects.requireNonNull(type);
        Objects.requireNonNull(args);
        if (args.isEmpty()) {
            throw new IllegalArgumentException("args is empty");
        }
        var argsCopy = List.copyOf(args);
        return new ParameterizedType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                builder.append(rawType().getSimpleName());
                builder.append("<");
                var index = 0;
                for (var arg : typeArgs()) {
                    arg.appendTo(builder);
                    index++;
                    if (index < typeArgs().size()) {
                        builder.append(", ");
                    }
                }
                builder.append(">");
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
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
                    case Wildcard wildcard -> wildcard.upperBound().stream().anyMatch(this::isAssignable);
                    case Intersection intersection -> intersection.bounds().stream().allMatch(this::isAssignable);
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
            public List<Arg> typeArgs() {
                return argsCopy;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

    /**
     * Creates a {@link ParameterizedType} from the given raw type and type arguments.
     *
     * @param type  the raw type
     * @param args  the type arguments
     * @return the {@link ParameterizedType}
     */
    static ParameterizedType of(Class<?> type, Arg... args) {
        Objects.requireNonNull(type);
        if (args.length == 0) {
            throw new IllegalArgumentException("args is empty");
        }
        // even if asList is not copying the array, it's fine, the overload makes a defensive copy of the list
        return of(type, Arrays.asList(args));
    }

}
