package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.~>

case class LambdaParameter[F[_]](name: F[Identifier], typ: Option[F[TypeIdentifier]]) {
  def mapK[G[_]](phi: F ~> G): LambdaParameter[G] =
    LambdaParameter(phi(name), typ.map(phi.apply))
}
