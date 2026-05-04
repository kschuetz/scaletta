package software.kes.scaletta.internal.types

sealed trait Variance

object Variance {
  def invariant: Variance = Invariant

  def covariant: Variance = Covariant

  def contravariant: Variance = Contravariant

  case object Invariant extends Variance

  case object Covariant extends Variance

  case object Contravariant extends Variance
}
