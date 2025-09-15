package java.lang.annotation;

/// Annotation marking methods that are compiler intrinsics.
@Documented
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
public @interface CompilerIntrinsic {
}
