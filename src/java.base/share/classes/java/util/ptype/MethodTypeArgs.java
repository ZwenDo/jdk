package java.util.ptype;

import java.util.Objects;

/**
 * Represents the type arguments of a method.
 */
public interface MethodTypeArgs {

    /**
     * Gets the type argument at the given index.
     * 
     * @param index the index of the type argument
     * @return the type argument at the given index
     */
    Arg arg(int index);

    /**
     * Creates a new instance of {@code MethodTypeArgs} with the given type arguments.
     * 
     * @param args the type arguments
     * @return a new instance of {@code MethodTypeArgs} with the given type arguments
     */
    static MethodTypeArgs of(Arg... args) {
        if (args.length == 0) {
            throw new IllegalArgumentException("args.length == 0");
        }
        return new MethodTypeArgs() {
            
            @Override
            public Arg arg(int index) {
                Objects.checkIndex(index, args.length);
                return args[index];
            }

            @Override
            public String toString() {
                var builder = new StringBuilder();
                builder.append("<");
                var index = 0;
                for (var arg : args) {
                    builder.append(arg.toString());
                    index++;
                    if (index < args.length) {
                        builder.append(", ");
                    }
                }
                builder.append(">");
                return builder.toString();
            }

        };
    }
    
}
