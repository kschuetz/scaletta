package software.kes.scaletta.parser

import software.kes.scaletta.scanner.Token

sealed trait ParseError

object ParseError {
  case class UnexpectedToken(token: Token) extends ParseError

  case class MissingExpression(context: String) extends ParseError

  case class UnclosedDelimiter(open: Token, expectedClose: Token) extends ParseError

  case class ExtraToken(token: Token, expected: String) extends ParseError
}
