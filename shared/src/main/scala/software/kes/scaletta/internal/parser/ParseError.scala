package software.kes.scaletta.internal.parser

import software.kes.scaletta.internal.scanner.Token

sealed trait ParseError

object ParseError {
  case class UnexpectedToken(token: Token) extends ParseError

  case class ExpectedToken(expected: Token, found: Token, context: String) extends ParseError

  case class ExpectedIdentifier(found: Token, context: String) extends ParseError

  case class MalformedDeclaration(keyword: Token, message: String) extends ParseError

  case class MissingExpression(context: String) extends ParseError

  case class UnclosedDelimiter(open: Token, expectedClose: Token) extends ParseError

  case class ExtraToken(token: Token, expected: String) extends ParseError

  case object VariadicParameterMustBeLast extends ParseError

  case class Message(message: String) extends ParseError

  case object PositionalAfterNamedArgument extends ParseError
}
