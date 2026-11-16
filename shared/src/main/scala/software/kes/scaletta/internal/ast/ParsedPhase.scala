package software.kes.scaletta.internal.ast

import software.kes.scaletta.util.functional.{Functor, FunctorK, ~>}

object ParsedPhase extends CorePhase {
  type Ident = Identifier
  type TypeIdent[F[_]] = TypeIdentifier[F]

  protected implicit def typeIdentFunctorK: FunctorK[TypeIdent] = TypeIdentifier.typeIdentifierFunctorK

  sealed trait Enumerator[F[_]] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Enumerator[G]
  }

  object Enumerator {
    final case class Generator[F[_]](
                                      isCase: Boolean,
                                      pattern: F[Pattern[F]],
                                      expression: F[Expression[F]]
                                    ) extends Enumerator[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Generator[G] =
        Generator(
          isCase,
          phi(F.map(pattern)(_.mapK(phi))),
          phi(F.map(expression)(_.mapK(phi)))
        )
    }

    final case class ValueBinding[F[_]](
                                         pattern: F[Pattern[F]],
                                         expression: F[Expression[F]]
                                       ) extends Enumerator[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): ValueBinding[G] =
        ValueBinding(
          phi(F.map(pattern)(_.mapK(phi))),
          phi(F.map(expression)(_.mapK(phi)))
        )
    }

    final case class Guard[F[_]](
                                  condition: F[Expression[F]]
                                ) extends Enumerator[F] {
      def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Guard[G] =
        Guard(phi(F.map(condition)(_.mapK(phi))))
    }
  }

  final case class ForYield[F[_]](
                                   enumerators: Vector[F[Enumerator[F]]],
                                   body: F[Expression[F]]
                                 ) extends Expression[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): ForYield[G] =
      ForYield(
        enumerators.map(e => phi(F.map(e)(_.mapK(phi)))),
        phi(F.map(body)(_.mapK(phi)))
      )
  }
}
