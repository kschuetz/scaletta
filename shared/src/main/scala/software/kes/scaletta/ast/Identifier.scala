package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

case class Identifier[F[_]](name: String) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Identifier[G] =
    Identifier(name)
}
