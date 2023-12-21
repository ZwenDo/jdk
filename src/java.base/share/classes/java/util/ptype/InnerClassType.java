package java.util.ptype;

import java.util.Objects;

/**
 * Represents an inner class type.
 */
public non-sealed interface InnerClassType extends Arg {

    /**
     * Gets the outer type of this inner class type.
     *
     * @return the outer type
     */
    Arg outerType();

    /**
     * Gets the inner type of this inner class type.
     *
     * @return the inner type
     */
    Arg innerType();

    /**
     * Creates an {@link InnerClassType} from the given outer and inner types.
     *
     * @param outerType the outer type
     * @param innerType the inner type
     * @return the {@link InnerClassType}
     */
    static InnerClassType of(Arg outerType, Arg innerType) {
        Objects.requireNonNull(outerType);
        Objects.requireNonNull(innerType);
        return new InnerClassType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Objects.requireNonNull(builder);
                outerType.appendTo(builder);
                builder.append(".");
                innerType.appendTo(builder);
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Objects.requireNonNull(actual);
                return switch (actual) {
                    case InnerClassType innerClassType -> outerType.isAssignable(innerClassType.outerType()) &&
                        innerType.isAssignable(innerClassType.innerType());
                    default -> false;
//                    case ArrayType _, ClassType _, Intersection _, ParameterizedType _, RawType _, Wildcard _ -> false;
                };
            }

            @Override
            public Arg outerType() {
                return outerType;
            }

            @Override
            public Arg innerType() {
                return innerType;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
