package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.~>

case class TypeArgument[F[_]](typ: F[TypeIdentifier]) {
  def mapK[G[_]](phi: F ~> G): TypeArgument[G] =
    TypeArgument(phi(typ))
}
