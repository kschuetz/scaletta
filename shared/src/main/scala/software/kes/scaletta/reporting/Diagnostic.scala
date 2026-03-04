package software.kes.scaletta.reporting

import software.kes.scaletta.parser.ParserError
import software.kes.scaletta.scanner.ScannerError

sealed trait Diagnostic {
  def severity: Severity
}

object Diagnostic {
  case class Scan(error: ScannerError) extends Diagnostic {
    def severity: Severity = Severity.Error
  }

  case class Parse(error: ParserError) extends Diagnostic {
    def severity: Severity = Severity.Error
  }
}
