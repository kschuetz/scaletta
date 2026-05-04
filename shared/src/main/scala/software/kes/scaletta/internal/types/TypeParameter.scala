package software.kes.scaletta.internal.types

object TypeParameter {
  def invariant[T]: TypeParameter[T] = TypeParameter(Variance.invariant)

  def covariant[T]: TypeParameter[T] = TypeParameter(Variance.covariant)

  def contravariant[T]: TypeParameter[T] = TypeParameter(Variance.contravariant)
}

final case class TypeParameter[T](variance: Variance = Variance.invariant)
