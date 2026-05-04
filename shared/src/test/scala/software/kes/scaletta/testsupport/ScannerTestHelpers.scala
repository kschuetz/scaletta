package software.kes.scaletta.testsupport

import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.scanner.{ScanError, Token}

object ScannerTestHelpers {
  def success(token: Token, begin: Int, end: Int): Pos[Token] =
    Pos(token, CharIndex(begin), CharIndex(end))

  def failure(error: ScanError, begin: Int, end: Int): Pos[Token] =
    Pos(Token.Error(error), CharIndex(begin), CharIndex(end))
}
