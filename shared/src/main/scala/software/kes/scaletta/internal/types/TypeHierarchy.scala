package software.kes.scaletta.internal.types

import software.kes.scaletta.api.Type

trait TypeHierarchy[T] {
  def relationshipFor(lhs: Type[T], rhs: Type[T]): TypeRelationship[T]

  /**
   * Returns the immediate supertypes of a type.
   * For BFS traversal, the caller can queue these results to visit the
   * hierarchy level-by-level.
   */
  def immediateSupertypes(t: Type[T]): Iterable[Type[T]]

  /**
   * Returns true if the first type is a subtype of the second type.
   */
  def isSubtype(lhs: Type[T], rhs: Type[T]): Boolean
}
