package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

case class LambdaParameter[F[_]](name: F[Identifier[F]], typ: Option[F[TypeIdentifier[F]]]) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): LambdaParameter[G] =
    LambdaParameter(phi(F.map(name)(_.mapK(phi))), typ.map(t => phi(F.map(t)(_.mapK(phi)))))
}
