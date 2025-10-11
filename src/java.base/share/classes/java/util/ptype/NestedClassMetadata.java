package java.util.ptype;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/// An annotation added to nested classes for which reflection does not provide enough information about whether the
/// class is static or not.
///
/// The classes that require this annotation are:
/// - Anonymous classes used as field initializers
/// - Local classes declared in static and instance initializers
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface NestedClassMetadata {

    /// Returns true if the annotated class is static, false otherwise.
    ///
    /// @return true if the class is static, false otherwise
    boolean isStatic();

}
