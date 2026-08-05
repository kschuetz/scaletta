package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.FunctorK

object ParsingPhase extends Phase {
  type Ident[F[_]] = Identifier[F]
  type TypeIdent[F[_]] = TypeIdentifier[F]

  protected implicit def identFunctorK: FunctorK[Ident] = Identifier.identifierFunctorK

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentifier.typeIdentifierFunctorK
}
