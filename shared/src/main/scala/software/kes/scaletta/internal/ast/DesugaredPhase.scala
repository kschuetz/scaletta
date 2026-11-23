package software.kes.scaletta.internal.ast

import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.util.functional.FunctorK

object DesugaredPhase extends CorePhase {
  type Ident = Identifier
  type TypeIdent[F[_]] = TypeIdentifier[F]
  type InterpolatorRef = Interpolator

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentifier.typeIdentifierFunctorK
}
