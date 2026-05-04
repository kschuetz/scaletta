package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

case class TypeArgument[F[_]](typ: F[TypeIdentifier[F]]) {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): TypeArgument[G] =
    TypeArgument(phi(F.map(typ)(_.mapK(phi))))
}
