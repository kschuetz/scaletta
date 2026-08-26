package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

trait PhaseWithPreResolutionInfo extends Phase {
  type Ident[F[_]] = F[PreResolutionInfo]

  protected implicit def identFunctorK: FunctorK[Ident] = IdentFunctorK

  private object IdentFunctorK extends FunctorK[Ident] {
    def mapK[F[_], G[_]](tf: F[PreResolutionInfo])
                        (nt: F ~> G)
                        (implicit F: Functor[F]): G[PreResolutionInfo] =
      nt(tf)
  }
}
