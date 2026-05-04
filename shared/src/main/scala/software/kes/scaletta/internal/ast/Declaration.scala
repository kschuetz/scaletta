package software.kes.scaletta.internal.ast

import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.util.functional.{Functor, ~>}

sealed trait Declaration[F[_]] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Declaration[G]
}

object Declaration {
  def val_[F[_]](pattern: F[Pattern[F]], rhs: F[Expression[F]]): Declaration[F] =
    Val(pattern, rhs)

  def lazyVal[F[_]](pattern: F[Pattern[F]], rhs: F[Expression[F]]): Declaration[F] =
    LazyVal(pattern, rhs)

  def def_[F[_]](name: F[Identifier[F]],
                 params: Vector[F[FormalParameterGroup[F]]],
                 returnType: Option[F[TypeIdentifier[F]]],
                 body: F[Expression[F]]): Declaration[F] =
    Def(name, params, returnType, body)

  case class Val[F[_]](pattern: F[Pattern[F]],
                       rhs: F[Expression[F]]) extends Declaration[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Val[G] =
      Val(
        phi(F.map(pattern)(_.mapK(phi))),
        phi(F.map(rhs)(_.mapK(phi)))
      )
  }

  case class Def[F[_]](name: F[Identifier[F]],
                       params: Vector[F[FormalParameterGroup[F]]],
                       returnType: Option[F[TypeIdentifier[F]]],
                       body: F[Expression[F]]) extends Declaration[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Def[G] =
      Def(
        phi(F.map(name)(_.mapK(phi))),
        params.map(p => phi(F.map(p)(_.mapK(phi)))),
        returnType.map(rt => phi(F.map(rt)(_.mapK(phi)))),
        phi(F.map(body)(_.mapK(phi)))
      )
  }

  case class LazyVal[F[_]](pattern: F[Pattern[F]],
                           rhs: F[Expression[F]]) extends Declaration[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): LazyVal[G] =
      LazyVal(
        phi(F.map(pattern)(_.mapK(phi))),
        phi(F.map(rhs)(_.mapK(phi)))
      )
  }

  case class Error[F[_]](error: ParseError) extends Declaration[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Error[G] = Error(error)
  }
}
