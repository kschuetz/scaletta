package software.kes.scaletta.parser

import software.kes.scaletta.ast.Expression
import software.kes.scaletta.reporting.Pos

object ParseResult {
  def empty[F[_]]: ParseResult[F] = ParseResult()

  def create[F[_]](value: F[Expression[F]]): ParseResult[F] = ParseResult(Some(value))

  def error[F[_]](error: Pos[ParseError]): ParseResult[F] = ParseResult(errors = Vector(error))
}

case class ParseResult[F[_]](value: Option[F[Expression[F]]] = None,
                             errors: Vector[Pos[ParseError]] = Vector.empty,
                             warnings: Vector[Pos[ParseWarning]] = Vector.empty,
                             hints: Vector[Pos[ParseHint]] = Vector.empty) {
  def isSuccess: Boolean = value.isDefined && !hasErrors

  def hasErrors: Boolean = errors.nonEmpty

  def withExpression(value: F[Expression[F]]): ParseResult[F] =
    copy(value = Some(value))

  def addError(error: Pos[ParseError]): ParseResult[F] =
    copy(errors = errors :+ error)

  def addWarning(warning: Pos[ParseWarning]): ParseResult[F] =
    copy(warnings = warnings :+ warning)

  def addHint(hint: Pos[ParseHint]): ParseResult[F] =
    copy(hints = hints :+ hint)
}
