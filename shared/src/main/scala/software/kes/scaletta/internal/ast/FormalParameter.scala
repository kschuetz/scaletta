package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

case class FormalParameter[F[_]](name: F[Identifier[F]],
                                 typ: F[TypeIdentifier[F]],
                                 default: Option[F[Expression[F]]] = None) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): FormalParameter[G] =
    FormalParameter(
      phi(F.map(name)(_.mapK(phi))),
      phi(F.map(typ)(_.mapK(phi))),
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
