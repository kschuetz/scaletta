package software.kes.scaletta.internal.parser

sealed trait ParseHint

object ParseHint {
  case object UnnecessaryParentheses extends ParseHint
}
