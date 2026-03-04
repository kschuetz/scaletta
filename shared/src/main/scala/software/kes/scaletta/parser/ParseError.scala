package software.kes.scaletta.parser

sealed trait ParseError

object ParseError {
  case object Generic extends ParseError
}
