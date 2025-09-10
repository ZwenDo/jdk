package java.util.ptype.model;

/// Supertype for all specialized types.
public sealed interface SpecializedTypeDescriptor permits ArrayDescriptor, ClassDescriptor, ErasedType, UnknownType {

}
