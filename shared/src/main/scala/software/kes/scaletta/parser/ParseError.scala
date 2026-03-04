package software.kes.scaletta.parser

sealed trait ParseError

object ParseError {
  case class Generic(message: String) extends ParseError
}
