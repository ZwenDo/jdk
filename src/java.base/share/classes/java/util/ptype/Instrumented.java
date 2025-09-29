package java.util.ptype;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/// Marker for classes that have been instrumented by the prototype.
@PrototypeInternal
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Instrumented {
}
