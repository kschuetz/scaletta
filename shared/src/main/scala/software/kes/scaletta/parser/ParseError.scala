package software.kes.scaletta.parser

import software.kes.scaletta.scanner.Token

sealed trait ParseError

object ParseError {
  case class Generic(message: String) extends ParseError

  case class UnexpectedToken(token: Token) extends ParseError
}
