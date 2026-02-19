package software.kes.scaletta.testsupport

import software.kes.scaletta.scanner.{CharIndex, Pos, ScannerError, Token}

object ScannerTestHelpers {
  def success(token: Token, begin: Int, end: Int): Pos[Token] =
    Pos(token, CharIndex(begin), CharIndex(end))

  def failure(error: ScannerError, begin: Int, end: Int): Pos[Token] =
    Pos(Token.Error(error), CharIndex(begin), CharIndex(end))
}
