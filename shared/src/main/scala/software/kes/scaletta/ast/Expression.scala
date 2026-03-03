package software.kes.scaletta.ast

import software.kes.scaletta.common.Interpolator

sealed trait Expression

case class Block(bindings: Vector[Binding], result: Expression) extends Expression

case class Reference(path: ::[Identifier]) extends Expression

sealed trait Literal extends Expression

object Literal {
  case class IntLiteral(value: Int) extends Literal

  case class LongLiteral(value: Long) extends Literal

  case class FloatLiteral(value: Float) extends Literal

  case class DoubleLiteral(value: Double) extends Literal

  sealed trait BooleanLiteral extends Literal {
    def value: Boolean
  }

  case object True extends BooleanLiteral {
    def value: Boolean = true
  }

  case object False extends BooleanLiteral {
    def value: Boolean = false
  }

  case object Null extends Literal

  case class CharLiteral(value: Char) extends Literal

  case class StringLiteral(value: String) extends Literal
}

case class Tuple(elements: Vector[Expression]) extends Expression

case class Conditional(condition: Expression,
                       thenBranch: Expression,
                       elseBranch: Expression) extends Expression

case class FunctionCall(target: Expression,
                        args: Vector[Argument]) extends Expression

case class Lambda(params: Vector[LambdaParameter],
                  body: Expression) extends Expression

case class InterpolatedString(interpolator: Interpolator,
                              value: Concat) extends Expression
