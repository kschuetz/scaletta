package software.kes.scaletta.scanner

sealed trait ScannerResult

object ScannerResult {
  case class Success(value: Pos[Token]) extends ScannerResult

  case class Error(value: Pos[ScannerError]) extends ScannerResult

  case object EndOfInput extends ScannerResult
}
