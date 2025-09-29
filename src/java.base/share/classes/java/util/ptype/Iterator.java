package java.util.ptype;

/// An iterator
///
/// @param <E> the type iterated over
interface Iterator<E> {

    /// Whether there is still items to iterate.
    ///
    /// @return true if there is still items to iterate over.
    boolean hasNext();

    /// Returns the next item or throws if the iterator is empty.
    ///
    /// @return the next element
    E next();

}
