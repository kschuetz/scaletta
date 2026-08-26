package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.FunctorK

object ParsingPhase extends PhaseWithPlainTypeIdentifier {
  type Ident[F[_]] = Identifier[F]

  protected implicit def identFunctorK: FunctorK[Ident] = Identifier.identifierFunctorK
}
