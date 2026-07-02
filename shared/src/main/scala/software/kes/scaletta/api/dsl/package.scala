package software.kes.scaletta.api

import scala.language.implicitConversions

package object dsl {
  /**
   * Implicitly converts a \String\ to a \Name\.
   * This allows using string literals directly in places where a \Name\ is required.
   *
   * This will throw an IllegalArgumentException at runtime if the string is not a proper name
   * (e.g., if empty or null).
   */
  implicit def stringToName(s: String): Name = Name(s)

  /**
   * Provides a DSL for constructing \FormalParameter\s using the \ofType\ operator.
   *
   * Example:
   * {{{
   * val p = "count" ofType IntType
   * }}}
   */
  implicit class ParameterStringOps(private val name: String) extends AnyVal {
    def ofType(typ: ProperType[TypeId]): FormalParameter =
      FormalParameter(Name(name), typ, None)
  }
}
