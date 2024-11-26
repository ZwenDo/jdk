package java.util.ptype;

/**
 * The kind of object that is the target of a cast operation.
 */
public enum CheckTarget {
    /**
     * The check is performed on a parameterized type.
     */
    PARAMETERIZED_TYPE,
    /**
     * The check is performed on a type parameter.
     */
    TYPE_PARAMETER,

    /*
     * The check is performed on a generic array.
     */
    // arrays are actually unwrapped, so we fall back on the two above
    // GENERIC_ARRAY,
    ;

    static CheckTarget fromString(String s) {
        switch (s) {
            case "PARAMETERIZED_TYPE": return PARAMETERIZED_TYPE;
            case "TYPE_PARAMETER": return TYPE_PARAMETER;
            default: throw new IllegalArgumentException("Unknown CheckTarget: " + s);
        }
    }
}
