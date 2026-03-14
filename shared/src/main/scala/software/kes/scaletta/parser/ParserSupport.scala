package software.kes.scaletta.parser

import software.kes.scaletta.reporting.{CharIndex, Pos}
import software.kes.scaletta.scanner.{Scanner, Token}

private[parser] object ParserSupport {

  /**
   * Consumes the next token and verifies it matches the `expected` token.
   *
   * If the token matches, it is returned.
   *
   * If the token does not match, a [[ParseError.ExpectedToken]] (or [[ParseError.UnclosedDelimiter]]
   * if the end of input was reached while looking for a closing delimiter) is reported via `reportError`.
   * The actual token found is still returned to allow the parser to attempt to continue.
   *
   * @param scanner     the scanner to read tokens from
   * @param expected    the token that is expected to be at the current position
   * @param context     a descriptive string explaining what was being parsed (e.g., "if condition")
   * @param reportError a callback function to report a diagnostic if the token does not match
   * @param openPos     optional starting position of an opening delimiter, used to provide better error
   *                    locations for unclosed delimiters
   * @return the token consumed from the scanner
   */
  def expect(scanner: Scanner,
             expected: Token,
             context: String,
             reportError: Pos[ParseError] => Unit,
             openPos: Option[CharIndex] = None): Pos[Token] = {
    val token = scanner.get()
    if (token.value != expected) {
      val error: ParseError = token.value match {
        case Token.EndOfInput =>
          val open = getOpenForClose(expected)
          if (open != expected) ParseError.UnclosedDelimiter(open, expected)
          else ParseError.ExpectedToken(expected, token.value, context)
        case _ => ParseError.ExpectedToken(expected, token.value, context)
      }
      val pos = token.value match {
        case Token.EndOfInput =>
          val open = getOpenForClose(expected)
          if (open != expected) {
            Pos(error, openPos.getOrElse(token.begin), token.end)
          } else {
            Pos(error, token.begin, token.end)
          }
        case _ => Pos(error, token.begin, token.end)
      }
      reportError(pos)
    }
    token
  }

  def expectIdentifier(scanner: Scanner,
                       context: String,
                       reportError: Pos[ParseError] => Unit): Pos[Token.Identifier] = {
    val token = scanner.get()
    token.value match {
      case id: Token.Identifier => token.as(id)
      case _ =>
        reportError(Pos(ParseError.ExpectedIdentifier(token.value, context), token.begin, token.end))
        // Return a synthetic identifier to allow parsing to continue
        val name = if (token.value == Token.EndOfInput) "<error>" else s"<error:${token.value}>"
        token.as(Token.Identifier.Lower(name))
    }
  }

  def synchronizeTo(scanner: Scanner,
                    predicate: Token => Boolean,
                    isFatal: Token => Boolean): Boolean = {
    var next = scanner.peek(1)
    while (!predicate(next.value) && !isFatal(next.value) && next.value != Token.EndOfInput) {
      scanner.get()
      next = scanner.peek(1)
    }
    isFatal(next.value)
  }

  def getOpenForClose(close: Token): Token = {
    close match {
      case Token.RParen => Token.LParen
      case Token.RBrace => Token.LBrace
      case Token.RBracket => Token.LBracket
      case _ => close
    }
  }

  def isCommonSynchronizationBoundary(token: Token): Boolean = {
    token match {
      case Token.Comma | Token.RParen | Token.EndOfInput | Token.Val | Token.Def | Token.If | Token.Case |
           Token.Semicolon | Token.RBrace =>
        true
      case _ => false
    }
  }

  def isStructuralBoundary(token: Token): Boolean = {
    token match {
      case Token.Val | Token.Def | Token.Else | Token.Then | Token.Case | Token.Semicolon | Token.RBrace |
           Token.EndOfInput =>
        true
      case _ => false
    }
  }

}
