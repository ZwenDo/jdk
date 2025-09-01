package java.util.ptype.model;

/// Supertype for all specialized types.
public sealed interface SpecializedType
        extends SpecializedTypeContainer
        permits ArrayType, ClassType, ErasedType, InnerClassType, IntersectionType, ParameterizedType, WildcardType {

}
