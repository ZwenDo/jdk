package java.util.ptype.model;

import java.util.Objects;
import java.util.ptype.SpecializedTypeUtils;
import java.util.ptype.util.ArrayList;
import java.util.ptype.util.Utils;

/// Represents the type arguments of a method.
public final class SpecializedMethodTypeArguments implements SpecializedTypeContainer {

    private final ArrayList<SpecializedType> arguments;

    /// Creates a new instance.
    ///
    /// @param arguments the arguments of the method
    public SpecializedMethodTypeArguments(SpecializedType... arguments) {
        if (arguments.length == 0) {
            throw new IllegalArgumentException("Cannot create a specialized method type arguments instance without type arguments");
        }
        checkRaw(arguments);
        this.arguments = ArrayList.of(Utils.requireNonNull(arguments));
    }

    /// Gets the n-th specialized type.
    ///
    /// @param index the index at which get the specialized type.
    /// @return the found index
    public SpecializedType typeArgument(int index) {
        Objects.checkIndex(arguments.size(), index);
        return arguments.get(index);
    }

    @Override
    public String toString() {
        var builder = new StringBuilder();
        builder.append("<");
        arguments.joinTo(builder, SpecializedTypeUtils::appendToBuilder, ", ");
        builder.append(">");
        return builder.toString();
    }

    private static void checkRaw(SpecializedType[] arguments) {
        var isRaw = arguments[0] == ErasedType.instance();
        for (var argument : arguments) {
            if (argument == ErasedType.instance() != isRaw) {
                throw new IllegalArgumentException("Cannot create a partially erazed method type arguments instance.");
            }
        }
    }

}
