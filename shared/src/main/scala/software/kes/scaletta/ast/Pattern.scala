package software.kes.scaletta.ast

import software.kes.scaletta.ast
import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.util.functional.{Functor, ~>}

sealed trait Pattern[F[_]] {
  def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Pattern[G]
}

object Pattern {
  /** Matches any value and binds it to a name. */
  case class Identifier[F[_]](name: F[ast.Identifier[F]]) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Identifier[G] =
      Identifier(phi(F.map(name)(_.mapK(phi))))
  }

  /** Wildcard pattern `_` that matches anything without binding. */
  case class Wildcard[F[_]]() extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Wildcard[G] = Wildcard()
  }

  /** Matches an exact literal value (Int, String, Null, etc.). */
  case class Literal[F[_]](value: F[ast.Literal[F]]) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Literal[G] =
      Literal(phi(F.map(value)(_.mapK(phi).asInstanceOf[ast.Literal[G]])))
  }

  /** Binds a name to a value if it matches another pattern: `name @ pattern`. */
  case class As[F[_]](name: F[ast.Identifier[F]], pattern: F[Pattern[F]]) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): As[G] =
      As(phi(F.map(name)(_.mapK(phi))), phi(F.map(pattern)(_.mapK(phi))))
  }

  /** Matches if the value satisfies a type test: `pattern: Type`. */
  case class Typed[F[_]](pattern: F[Pattern[F]], ascription: F[TypeIdentifier[F]]) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Typed[G] =
      Typed(phi(F.map(pattern)(_.mapK(phi))), phi(F.map(ascription)(_.mapK(phi))))
  }

  /** Positional destructuring of tuples: `(p1, p2, ...)`. */
  case class Tuple[F[_]](elements: Vector[F[Pattern[F]]]) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Tuple[G] =
      Tuple(elements.map(e => phi(F.map(e)(_.mapK(phi)))))
  }

  /**
   * Constructor-like pattern for host-provided product types: `Some(p1)`.
   * This is necessary to support matching on `Option` types.
   */
  case class Product[F[_]](typeId: F[TypeIdentifier[F]], args: Vector[F[Pattern[F]]]) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Product[G] =
      Product(phi(F.map(typeId)(_.mapK(phi))), args.map(a => phi(F.map(a)(_.mapK(phi)))))
  }

  case class Error[F[_]](error: ParseError) extends Pattern[F] {
    def mapK[G[_]](phi: F ~> G)(implicit F: Functor[F]): Error[G] = Error(error)
  }
}
