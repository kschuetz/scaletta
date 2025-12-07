package software.kes.scaletta.spike

sealed trait BinaryOperator

object BinaryOperator {
  case object Add extends BinaryOperator

  case object Sub extends BinaryOperator

  case object Mul extends BinaryOperator

  case object Div extends BinaryOperator

  case object Mod extends BinaryOperator

  case object Eq extends BinaryOperator

  case object Neq extends BinaryOperator

  case object Gt extends BinaryOperator

  case object Lt extends BinaryOperator

  case object Ge extends BinaryOperator

  case object Le extends BinaryOperator

  case object And extends BinaryOperator

  case object Or extends BinaryOperator

  case object BitAnd extends BinaryOperator

  case object BitOr extends BinaryOperator

  case object BitXor extends BinaryOperator

  case object Shl extends BinaryOperator

  case object Shr extends BinaryOperator

  case object UnsignedShr extends BinaryOperator
}
