package java.util.ptype;

import java.lang.reflect.Executable;

final class ReflectionUtils {

    public static Executable enclosingMethod(Class<?> current) {
        Utils.requireNonNull(current);
        var method = current.getEnclosingMethod();
        if (method != null) return method;
        return current.getEnclosingConstructor();
    }

    private ReflectionUtils() {
        throw new AssertionError();
    }

}
