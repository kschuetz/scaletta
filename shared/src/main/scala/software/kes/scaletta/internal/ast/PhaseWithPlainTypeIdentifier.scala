package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.FunctorK

trait PhaseWithPlainTypeIdentifier extends Phase {
  type TypeIdent[F[_]] = TypeIdentifier[F]

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentifier.typeIdentifierFunctorK
}
