package software.kes.scaletta.ast

import software.kes.scaletta.ast

sealed trait Pattern

object Pattern {
  /** Matches any value and binds it to a name. */
  case class Identifier(name: ast.Identifier) extends Pattern

  /** Wildcard pattern `_` that matches anything without binding. */
  case object Wildcard extends Pattern

  /** Matches an exact literal value (Int, String, Null, etc.). */
  case class Literal(value: ast.Literal) extends Pattern

  /** Binds a name to a value if it matches another pattern: `name @ pattern`. */
  case class As(name: ast.Identifier, pattern: Pattern) extends Pattern

  /** Matches if the value satisfies a type test: `pattern: Type`. */
  case class Typed(pattern: Pattern, ascription: TypeIdentifier) extends Pattern

  /** Positional destructuring of tuples: `(p1, p2, ...)`. */
  case class Tuple(elements: Vector[Pattern]) extends Pattern

  /**
   * Constructor-like pattern for host-provided product types: `Some(p1)`.
   * This is necessary to support matching on `Option` types.
   */
  case class Product(typeId: TypeIdentifier, args: Vector[Pattern]) extends Pattern
}
