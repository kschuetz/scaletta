package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

trait PhaseWithTypedInfo extends Phase {
  type Ident[F[_]] = F[TypedInfo]

  protected implicit def identFunctorK: FunctorK[Ident] = IdentFunctorK

  private object IdentFunctorK extends FunctorK[Ident] {
    def mapK[F[_], G[_]](tf: F[TypedInfo])
                        (nt: F ~> G)
                        (implicit F: Functor[F]): G[TypedInfo] =
      nt(tf)
  }
}
