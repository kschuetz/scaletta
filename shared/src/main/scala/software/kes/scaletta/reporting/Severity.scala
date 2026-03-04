package software.kes.scaletta.reporting

sealed trait Severity

object Severity {
  case object Error extends Severity

  case object Warning extends Severity
}
