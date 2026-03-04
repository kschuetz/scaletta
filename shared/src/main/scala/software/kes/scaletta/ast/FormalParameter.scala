package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

case class FormalParameter[F[_]](name: F[Identifier],
                                 typ: F[TypeIdentifier],
                                 default: Option[F[Expression[F]]] = None) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): FormalParameter[G] =
    FormalParameter(
      phi(name),
      phi(typ),
      default.map(d => phi(F.map(d)(_.mapK(phi))))
    )
}

case class FormalParameterGroup[F[_]](parameters: Vector[F[FormalParameter[F]]],
                                      variadic: Option[F[FormalParameter[F]]] = None) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): FormalParameterGroup[G] =
    FormalParameterGroup(
      parameters.map(p => phi(F.map(p)(_.mapK(phi)))),
      variadic.map(v => phi(F.map(v)(_.mapK(phi))))
    )
}
