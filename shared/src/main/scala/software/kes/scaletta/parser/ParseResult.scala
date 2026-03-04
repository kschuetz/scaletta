package software.kes.scaletta.parser

import software.kes.scaletta.ast.Expression
import software.kes.scaletta.reporting.Pos

case class ParseResult(value: Option[Expression] = None,
                       errors: Vector[Pos[ParseError]] = Vector.empty,
                       warnings: Vector[Pos[ParseWarning]] = Vector.empty) {
  def isSuccess: Boolean = value.isDefined && errors.isEmpty

  def withExpression(value: Expression): ParseResult =
    copy(value = Some(value))

  def addError(error: Pos[ParseError]): ParseResult =
    copy(errors = errors :+ error)

  def addWarning(warning: Pos[ParseWarning]): ParseResult =
    copy(warnings = warnings :+ warning)
}
