package software.kes.scaletta.testsupport

import software.kes.scaletta.scanner.{CharIndex, Pos, Token}

object ScannerTestHelpers {
  def success[E](token: Token, begin: Int, end: Int): Pos[Either[E, Token]] =
    Pos(Right(token), CharIndex(begin), CharIndex(end))
}
