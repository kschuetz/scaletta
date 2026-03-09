package software.kes.scaletta.parser

sealed trait ParseHint

object ParseHint {
  case object UnnecessaryParentheses extends ParseHint
}
