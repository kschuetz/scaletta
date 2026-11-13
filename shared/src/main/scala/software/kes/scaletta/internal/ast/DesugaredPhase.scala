package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.FunctorK

object DesugaredPhase extends CorePhase {
  type Ident = Identifier
  type TypeIdent[F[_]] = TypeIdentifier[F]

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentifier.typeIdentifierFunctorK
}
