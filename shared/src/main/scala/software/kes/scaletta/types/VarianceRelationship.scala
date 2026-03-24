package software.kes.scaletta.types

sealed trait VarianceRelationship

object VarianceRelationship {
  case object Same extends VarianceRelationship

  case object Subtype extends VarianceRelationship

  case object Supertype extends VarianceRelationship

  case object Unrelated extends VarianceRelationship
}
