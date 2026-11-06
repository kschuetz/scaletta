package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

trait PhaseWithLinkedInfo extends Phase {
  type Ident[F[_]] = F[LinkedInfo]

  protected implicit def identFunctorK: FunctorK[Ident] = IdentFunctorK

  private object IdentFunctorK extends FunctorK[Ident] {
    def mapK[F[_], G[_]](tf: F[LinkedInfo])
                        (nt: F ~> G)
                        (implicit F: Functor[F]): G[LinkedInfo] =
      nt(tf)
  }
}
