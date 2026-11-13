package software.kes.scaletta.internal.ast

import software.kes.scaletta.api.{ProperType, TypeId}
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.internal.types.TypeResolutionError
import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

object TypedPhase extends CorePhase {
  type Ident = TypedInfo
  type TypeIdent[F[_]] = Either[TypeResolutionError, ProperType[TypeId]]

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentFunctorK

  private object TypeIdentFunctorK extends FunctorK[TypeIdent] {
    def mapK[F[_], G[_]](tf: Either[TypeResolutionError, ProperType[TypeId]])
                        (nt: F ~> G)
                        (implicit F: Functor[F]): Either[TypeResolutionError, ProperType[TypeId]] =
      tf
  }

  def posToAttributed(defaultType: ProperType[TypeId]): Pos ~> Attributed =
    Attributed.lift(defaultType)
}
