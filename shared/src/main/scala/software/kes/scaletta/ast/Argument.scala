package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

case class Argument[F[_]](value: F[Expression[F]],
                          name: Option[F[Identifier[F]]] = None) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Argument[G] =
    Argument(phi(F.map(value)(_.mapK(phi))), name.map(n => phi(F.map(n)(_.mapK(phi)))))
}

case class ArgumentGroup[F[_]](arguments: Vector[F[Argument[F]]],
                               splat: Option[F[Argument[F]]] = None) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): ArgumentGroup[G] =
    ArgumentGroup(
      arguments.map(a => phi(F.map(a)(_.mapK(phi)))),
      splat.map(s => phi(F.map(s)(_.mapK(phi))))
    )
}
