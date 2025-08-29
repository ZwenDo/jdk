package java.util.ptype;

import java.util.ptype.model.SpecializedType;

/// The kind of location where a [checkCast][SpecializedTypeUtils#checkCast(Object, SpecializedType)] is performed.
public enum CheckLocationKind {
    /// The check is performed at the entry of a method.
    ENTRY,
    /// The check is performed at the exit of a method.
    EXIT,
    /// The check is performed at a cast operation.
    CAST,
    /// The check is performed at a storage operation.
    STORAGE,
    ;

}
