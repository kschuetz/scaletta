package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

object Identifier {
  implicit object identifierFunctorK extends FunctorK[Identifier] {
    def mapK[F[_], G[_]](tf: Identifier[F])(nt: F ~> G)(implicit F: Functor[F]): Identifier[G] =
      Identifier(tf.name)
  }
}

case class Identifier[F[_]](name: String) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Identifier[G] =
    Identifier(name)
}
