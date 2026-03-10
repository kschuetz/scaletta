package software.kes.scaletta.parser

import software.kes.scaletta.reporting.Pos

case class ParseResult[F[_], +A](value: Option[A] = None,
                                 errors: Vector[Pos[ParseError]] = Vector.empty,
                                 warnings: Vector[Pos[ParseWarning]] = Vector.empty,
                                 hints: Vector[Pos[ParseHint]] = Vector.empty) {
  def isSuccess: Boolean = value.isDefined && !hasErrors

  def hasErrors: Boolean = errors.nonEmpty

  def addError(error: Pos[ParseError]): ParseResult[F, A] =
    copy(errors = errors :+ error)

  def addWarning(warning: Pos[ParseWarning]): ParseResult[F, A] =
    copy(warnings = warnings :+ warning)

  def addHint(hint: Pos[ParseHint]): ParseResult[F, A] =
    copy(hints = hints :+ hint)
}

object ParseResult {
  def empty[F[_], A]: ParseResult[F, A] = ParseResult()

  def create[F[_], A](value: A): ParseResult[F, A] = ParseResult(Some(value))

  def error[F[_], A](error: Pos[ParseError]): ParseResult[F, A] = ParseResult(errors = Vector(error))
}
