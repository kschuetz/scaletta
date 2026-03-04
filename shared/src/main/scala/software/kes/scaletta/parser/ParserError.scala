package software.kes.scaletta.parser

sealed trait ParserError

object ParserError {
  case object Generic extends ParserError
}
