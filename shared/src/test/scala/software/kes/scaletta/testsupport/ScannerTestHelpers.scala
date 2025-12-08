package software.kes.scaletta.testsupport

import software.kes.scaletta.scanner.{CharIndex, Pos, ScannerError, Token}

object ScannerTestHelpers {
  def success[E](token: Token, begin: Int, end: Int): Pos[Either[E, Token]] =
    Pos(Right(token), CharIndex(begin), CharIndex(end))

  def failure[A](error: ScannerError, begin: Int, end: Int): Pos[Either[ScannerError, A]] =
    Pos(Left(error), CharIndex(begin), CharIndex(end))
}
