package software.kes.scaletta.internal.parser

import software.kes.scaletta.internal.reporting.Pos

case class ParseResult[F[_], +A](value: Option[A] = None,
                                 diagnostics: ParseDiagnostics = ParseDiagnostics.empty) {
  def errors: Vector[Pos[ParseError]] = diagnostics.errors

  def warnings: Vector[Pos[ParseWarning]] = diagnostics.warnings

  def hints: Vector[Pos[ParseHint]] = diagnostics.hints

  def isSuccess: Boolean = value.isDefined && !hasErrors

  def hasValue: Boolean = value.isDefined

  def hasErrors: Boolean = diagnostics.hasErrors

  def addError(error: Pos[ParseError]): ParseResult[F, A] =
    modifyDiagnostics(_.addError(error))

  def addWarning(warning: Pos[ParseWarning]): ParseResult[F, A] =
    modifyDiagnostics(_.addWarning(warning))

  def addHint(hint: Pos[ParseHint]): ParseResult[F, A] =
    modifyDiagnostics(_.addHint(hint))

  /**
   * Combines diagnostics from another result into this one.
   */
  def addDiagnostics(other: ParseDiagnostics): ParseResult[F, A] =
    modifyDiagnostics(_ ++ other)

  private def modifyDiagnostics(fn: ParseDiagnostics => ParseDiagnostics): ParseResult[F, A] =
    copy(diagnostics = fn(diagnostics))
}

object ParseResult {
  def empty[F[_], A]: ParseResult[F, A] = ParseResult()

  def create[F[_], A](value: A): ParseResult[F, A] = ParseResult(Some(value))

  def error[F[_], A](error: Pos[ParseError]): ParseResult[F, A] = ParseResult(diagnostics = ParseDiagnostics.empty.addError(error))

  /**
   * Aggregates diagnostics from multiple results into a single Diagnostics object.
   */
  def combineDiagnostics[F[_]](results: Iterable[ParseResult[F, _]]): ParseDiagnostics =
    results.foldLeft(ParseDiagnostics.empty)(_ ++ _.diagnostics)
}
