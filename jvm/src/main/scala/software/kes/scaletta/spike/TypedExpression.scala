package software.kes.scaletta.spike

sealed trait TypedExpression {
  def typ: Type
}

object TypedExpression {
  case class Constant(value: Any, typ: Type) extends TypedExpression

  case class BinaryOperation(lhs: TypedExpression,
                             op: BinaryOperator,
                             rhs: TypedExpression,
                             typ: Type) extends TypedExpression

  case class Reference(get: () => Any,
                       typ: Type) extends TypedExpression
}
