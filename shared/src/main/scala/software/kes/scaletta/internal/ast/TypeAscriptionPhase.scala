package software.kes.scaletta.internal.ast

import software.kes.scaletta.api.{ProperType, TypeId}
import software.kes.scaletta.internal.types.TypeResolutionError
import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

object TypeAscriptionPhase extends Phase {
  type Ident[F[_]] = Identifier[F]
  type TypeIdent[F[_]] = F[Either[TypeResolutionError, ProperType[TypeId]]]

  protected implicit def identFunctorK: FunctorK[Ident] = Identifier.identifierFunctorK

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentFunctorK

  private object TypeIdentFunctorK extends FunctorK[TypeIdent] {
    def mapK[F[_], G[_]](tf: F[Either[TypeResolutionError, ProperType[TypeId]]])
                        (nt: F ~> G)
                        (implicit F: Functor[F]): G[Either[TypeResolutionError, ProperType[TypeId]]] =
      nt(tf)
  }
}
