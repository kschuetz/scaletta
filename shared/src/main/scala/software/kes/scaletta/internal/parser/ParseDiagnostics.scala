package software.kes.scaletta.internal.parser

import software.kes.scaletta.reporting.Pos

case class ParseDiagnostics(errors: Vector[Pos[ParseError]] = Vector.empty,
                            warnings: Vector[Pos[ParseWarning]] = Vector.empty,
                            hints: Vector[Pos[ParseHint]] = Vector.empty) {
  def hasErrors: Boolean = errors.nonEmpty

  def ++(other: ParseDiagnostics): ParseDiagnostics = ParseDiagnostics(
    errors ++ other.errors,
    warnings ++ other.warnings,
    hints ++ other.hints
  )

  def addError(error: Pos[ParseError]): ParseDiagnostics = copy(errors = errors :+ error)

  def addWarning(warning: Pos[ParseWarning]): ParseDiagnostics = copy(warnings = warnings :+ warning)

  def addHint(hint: Pos[ParseHint]): ParseDiagnostics = copy(hints = hints :+ hint)
}

object ParseDiagnostics {
  val empty: ParseDiagnostics = ParseDiagnostics()
}
