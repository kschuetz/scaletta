package software.kes.scaletta.internal.types

trait TypeHierarchy[T] {
  def relationshipFor(lhs: Type[T], rhs: Type[T]): TypeRelationship[T]

  /**
   * Returns the immediate supertypes of a type.
   * For BFS traversal, the caller can queue these results to visit the
   * hierarchy level-by-level.
   */
  def immediateSupertypes(t: Type[T]): Iterable[Type[T]]
}
