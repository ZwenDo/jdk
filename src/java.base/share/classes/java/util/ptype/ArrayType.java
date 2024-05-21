package java.util.ptype;

/**
 * Represents an array type.
 */
public non-sealed interface ArrayType extends Arg {

    /**
     * Gets the component type of this array type.
     *
     * @return the component type
     */
    Arg componentType();

    /**
     * Creates an {@link ArrayType} from the given component type.
     *
     * @param componentTypeArgs the component type
     * @return the {@link ArrayType}
     */
    static ArrayType of(Arg componentTypeArgs) {
        return new ArrayType() {

            @Override
            public void appendTo(StringBuilder builder) {
                Utils.requireNonNull(builder);
                componentTypeArgs.appendTo(builder);
                builder.append("[]");
            }

            @Override
            public boolean isAssignable(Arg actual) {
                Utils.requireNonNull(actual);
                return switch (actual) {
                    case ArrayType arrayType -> componentTypeArgs.isAssignable(arrayType.componentType());
                    case ClassType ignored -> false;
                    case InnerClassType ignored -> false;
                    case Intersection ignored -> false;
                    case ParameterizedType ignored -> false;
                    case RawType ignored -> false;
                    case Wildcard ignored -> false;
                };
            }

            @Override
            public Arg componentType() {
                return componentTypeArgs;
            }

            @Override
            public String toString() {
                return Arg.toString(this);
            }

        };
    }

}
