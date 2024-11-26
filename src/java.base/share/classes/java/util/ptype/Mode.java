package java.util.ptype;

enum Mode {
        /**
         * All casts are generated.
         */
        FULL,
        /**
         * Only casts for type parameters at method entry and exit, and cast are generated.
         */
        NORMAL,
        /**
         * Only storage casts are generated.
         */
        MINIMAL,
        /**
         * No cast is generated.
         */
        DISABLED,
        ;

        static Mode fromString(String s) {
            switch (s) {
                case "FULL": return FULL;
                case "NORMAL": return NORMAL;
                case "MINIMAL": return MINIMAL;
                case "DISABLED": return DISABLED;
                default: throw new IllegalArgumentException("Unknown Mode: " + s);
            }
        }

}
