package software.kes.scaletta.reporting

import software.kes.scaletta.parser.ParseError
import software.kes.scaletta.scanner.ScanError

sealed trait Diagnostic {
  def severity: Severity
}

object Diagnostic {
  case class Scan(error: ScanError) extends Diagnostic {
    def severity: Severity = Severity.Error
  }

  case class Parse(error: ParseError) extends Diagnostic {
    def severity: Severity = Severity.Error
  }
}
