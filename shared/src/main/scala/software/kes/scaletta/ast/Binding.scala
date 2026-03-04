package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.{Functor, ~>}

sealed trait Binding[F[_]] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Binding[G]
}

object Binding {
  case class Val[F[_]](pattern: F[Pattern[F]],
                       rhs: F[Expression[F]]) extends Binding[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Val[G] =
      Val(
        phi(F.map(pattern)(_.mapK(phi))),
        phi(F.map(rhs)(_.mapK(phi)))
      )
  }

  case class Def[F[_]](name: F[Identifier],
                       params: Vector[F[FormalParameterGroup[F]]],
                       body: F[Expression[F]]) extends Binding[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Def[G] =
      Def(
        phi(name),
        params.map(p => phi(F.map(p)(_.mapK(phi)))),
        phi(F.map(body)(_.mapK(phi)))
      )
  }

  case class LazyVal[F[_]](pattern: F[Pattern[F]],
                           rhs: F[Expression[F]]) extends Binding[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): LazyVal[G] =
      LazyVal(
        phi(F.map(pattern)(_.mapK(phi))),
        phi(F.map(rhs)(_.mapK(phi)))
      )
  }
}
