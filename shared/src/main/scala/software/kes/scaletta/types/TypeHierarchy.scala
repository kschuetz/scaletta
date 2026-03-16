package software.kes.scaletta.types

trait TypeHierarchy[T] {
  def relationshipFor(lhs: T, rhs: T): TypeRelationship[T]
}
