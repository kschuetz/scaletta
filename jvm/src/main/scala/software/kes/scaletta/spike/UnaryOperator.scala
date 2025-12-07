package software.kes.scaletta.spike

sealed trait UnaryOperator

object UnaryOperator {
  case object Neg extends UnaryOperator

  case object Not extends UnaryOperator

  case object BitNot extends UnaryOperator
}
