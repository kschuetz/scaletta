package software.kes.scaletta.types

trait TypeHierarchy[T] {
  def relationshipFor(lhs: T, rhs: T): TypeRelationship[T]

  /**
   * Returns the immediate supertypes of a type.
   * For BFS traversal, the caller can queue these results to visit the
   * hierarchy level-by-level.
   */
  def immediateSupertypes(t: T): Iterable[T]
}
