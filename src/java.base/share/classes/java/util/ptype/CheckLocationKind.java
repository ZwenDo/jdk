package java.util.ptype;

/**
 * The kind of location where a {@link TypeOperations#checkCast checkCast} is performed.
 */
public enum CheckLocationKind {
    /**
     * The check is performed at the entry of a method.
     */
    ENTRY,
    /**
     * The check is performed at the exit of a method.
     */
    EXIT,
    /**
     * The check is performed at a cast operation.
     */
    CAST,

    /**
     * The check is performed at a storage operation.
     */
    STORAGE,
    ;

    static CheckLocationKind fromString(String s) {
        switch (s) {
            case "ENTRY": return ENTRY;
            case "EXIT": return EXIT;
            case "CAST": return CAST;
            case "STORAGE": return STORAGE;
            default: throw new IllegalArgumentException("Unknown CheckLocationKind: " + s);
        }
    }

}
