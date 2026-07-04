package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.internal.intermediate.IntermediateExpression._

sealed trait Binding

object Binding {
  case class ObjectBinding(value: ObjectExpression) extends Binding

  case class BooleanBinding(value: BooleanExpression) extends Binding

  case class IntBinding(value: IntExpression) extends Binding

  case class LongBinding(value: LongExpression) extends Binding

  case class ShortBinding(value: ShortExpression) extends Binding

  case class ByteBinding(value: ByteExpression) extends Binding

  case class CharBinding(value: CharExpression) extends Binding

  case class DoubleBinding(value: DoubleExpression) extends Binding

  case class FloatBinding(value: FloatExpression) extends Binding
}
