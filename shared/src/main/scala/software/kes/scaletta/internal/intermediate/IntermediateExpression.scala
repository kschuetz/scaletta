package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.api.NativeFunctionId

sealed trait IntermediateExpression

object IntermediateExpression {

  case class NativeCall(target: NativeFunctionId,
                        arguments: Vector[IntermediateExpression]) extends IntermediateExpression

  case class LocalCall(scope: Int,
                       slot: Int,
                       arguments: Vector[IntermediateExpression]) extends IntermediateExpression

  case class Conditional(condition: BooleanExpression,
                         thenBranch: IntermediateExpression,
                         elseBranch: IntermediateExpression) extends IntermediateExpression

  case class Reference(scope: Int,
                       slot: Int) extends IntermediateExpression

  sealed trait ObjectExpression extends IntermediateExpression

  case class ObjectNativeCall(target: NativeFunctionId,
                              arguments: Vector[IntermediateExpression]) extends ObjectExpression

  case class ObjectLocalCall(scope: Int,
                             slot: Int,
                             arguments: Vector[IntermediateExpression]) extends ObjectExpression

  case class ObjectConditional(condition: BooleanExpression,
                               thenBranch: ObjectExpression,
                               elseBranch: ObjectExpression) extends ObjectExpression

  case class ObjectWithBindings(bindings: Vector[Binding],
                                body: ObjectExpression) extends ObjectExpression

  case class ObjectReference(scope: Int,
                             slot: Int) extends ObjectExpression

  sealed trait BooleanExpression extends IntermediateExpression

  case class BooleanNativeCall(target: NativeFunctionId,
                               arguments: Vector[IntermediateExpression]) extends BooleanExpression

  case class BooleanLocalCall(scope: Int,
                              slot: Int,
                              arguments: Vector[IntermediateExpression]) extends BooleanExpression

  case class BooleanConditional(condition: BooleanExpression,
                                thenBranch: BooleanExpression,
                                elseBranch: BooleanExpression) extends BooleanExpression

  case class BooleanWithBindings(bindings: Vector[Binding],
                                 body: BooleanExpression) extends BooleanExpression

  case class BooleanReference(scope: Int,
                              slot: Int) extends BooleanExpression

  sealed trait IntExpression extends IntermediateExpression

  case class IntNativeCall(target: NativeFunctionId,
                           arguments: Vector[IntermediateExpression]) extends IntExpression

  case class IntLocalCall(scope: Int,
                          slot: Int,
                          arguments: Vector[IntermediateExpression]) extends IntExpression

  case class IntConditional(condition: BooleanExpression,
                            thenBranch: IntExpression,
                            elseBranch: IntExpression) extends IntExpression

  case class IntWithBindings(bindings: Vector[Binding],
                             body: IntExpression) extends IntExpression

  case class IntReference(scope: Int,
                          slot: Int) extends IntExpression

  sealed trait LongExpression extends IntermediateExpression

  case class LongNativeCall(target: NativeFunctionId,
                            arguments: Vector[IntermediateExpression]) extends LongExpression

  case class LongLocalCall(scope: Int,
                           slot: Int,
                           arguments: Vector[IntermediateExpression]) extends LongExpression

  case class LongConditional(condition: BooleanExpression,
                             thenBranch: LongExpression,
                             elseBranch: LongExpression) extends LongExpression

  case class LongWithBindings(bindings: Vector[Binding],
                              body: LongExpression) extends LongExpression

  case class LongReference(scope: Int,
                           slot: Int) extends LongExpression

  sealed trait ShortExpression extends IntermediateExpression

  case class ShortNativeCall(target: NativeFunctionId,
                             arguments: Vector[IntermediateExpression]) extends ShortExpression

  case class ShortLocalCall(scope: Int,
                            slot: Int,
                            arguments: Vector[IntermediateExpression]) extends ShortExpression

  case class ShortConditional(condition: BooleanExpression,
                              thenBranch: ShortExpression,
                              elseBranch: ShortExpression) extends ShortExpression

  case class ShortWithBindings(bindings: Vector[Binding],
                               body: ShortExpression) extends ShortExpression

  case class ShortReference(scope: Int,
                            slot: Int) extends ShortExpression

  sealed trait ByteExpression extends IntermediateExpression

  case class ByteNativeCall(target: NativeFunctionId,
                            arguments: Vector[IntermediateExpression]) extends ByteExpression

  case class ByteLocalCall(scope: Int,
                           slot: Int,
                           arguments: Vector[IntermediateExpression]) extends ByteExpression

  case class ByteConditional(condition: BooleanExpression,
                             thenBranch: ByteExpression,
                             elseBranch: ByteExpression) extends ByteExpression

  case class ByteWithBindings(bindings: Vector[Binding],
                              body: ByteExpression) extends ByteExpression

  case class ByteReference(scope: Int,
                           slot: Int) extends ByteExpression

  sealed trait CharExpression extends IntermediateExpression

  case class CharNativeCall(target: NativeFunctionId,
                            arguments: Vector[IntermediateExpression]) extends CharExpression

  case class CharLocalCall(scope: Int,
                           slot: Int,
                           arguments: Vector[IntermediateExpression]) extends CharExpression

  case class CharConditional(condition: BooleanExpression,
                             thenBranch: CharExpression,
                             elseBranch: CharExpression) extends CharExpression

  case class CharWithBindings(bindings: Vector[Binding],
                              body: CharExpression) extends CharExpression

  case class CharReference(scope: Int,
                           slot: Int) extends CharExpression

  sealed trait DoubleExpression extends IntermediateExpression

  case class DoubleNativeCall(target: NativeFunctionId,
                              arguments: Vector[IntermediateExpression]) extends DoubleExpression

  case class DoubleLocalCall(scope: Int,
                             slot: Int,
                             arguments: Vector[IntermediateExpression]) extends DoubleExpression

  case class DoubleConditional(condition: BooleanExpression,
                               thenBranch: DoubleExpression,
                               elseBranch: DoubleExpression) extends DoubleExpression

  case class DoubleWithBindings(bindings: Vector[Binding],
                                body: DoubleExpression) extends DoubleExpression

  case class DoubleReference(scope: Int,
                             slot: Int) extends DoubleExpression

  sealed trait FloatExpression extends IntermediateExpression

  case class FloatNativeCall(target: NativeFunctionId,
                             arguments: Vector[IntermediateExpression]) extends FloatExpression

  case class FloatLocalCall(scope: Int,
                            slot: Int,
                            arguments: Vector[IntermediateExpression]) extends FloatExpression

  case class FloatConditional(condition: BooleanExpression,
                              thenBranch: FloatExpression,
                              elseBranch: FloatExpression) extends FloatExpression

  case class FloatWithBindings(bindings: Vector[Binding],
                               body: FloatExpression) extends FloatExpression

  case class FloatReference(scope: Int,
                            slot: Int) extends FloatExpression

  case class And(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

  case class Or(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

  case class StringConcat(segments: Vector[IntermediateExpression]) extends ObjectExpression

  sealed trait Value extends IntermediateExpression

  object Value {
    def apply(value: Any): Value = value match {
      case x: Int => int(x)
      case x: Long => long(x)
      case x: Short => short(x)
      case x: Byte => byte(x)
      case x: Float => float(x)
      case x: Double => double(x)
      case x: Boolean => boolean(x)
      case x: Char => char(x)
      case x: String => string(x)
      case x: AnyRef => object_(x)
    }

    def int(value: Int): Value = IntValue(value)

    def long(value: Long): Value = LongValue(value)

    def short(value: Short): Value = ShortValue(value)

    def byte(value: Byte): Value = ByteValue(value)

    def float(value: Float): Value = FloatValue(value)

    def double(value: Double): Value = DoubleValue(value)

    def boolean(value: Boolean): Value = if (value) True else False

    def true_(): Value = True

    def false_(): Value = False

    def null_(): Value = Null

    def char(value: Char): Value = CharValue(value)

    def string(value: String): Value = StringValue(value)

    def object_(value: AnyRef): Value =
      if (value == null) Null
      else value match {
        case x: String => StringValue(x)
        case other => ObjectValue(other)
      }

    case class IntValue(value: Int) extends Value with IntExpression

    case class LongValue(value: Long) extends Value with LongExpression

    case class FloatValue(value: Float) extends Value with FloatExpression

    case class DoubleValue(value: Double) extends Value with DoubleExpression

    case class ShortValue(value: Short) extends Value with ShortExpression

    case class ByteValue(value: Byte) extends Value with ByteExpression

    sealed trait BooleanValue extends Value with BooleanExpression {
      def value: Boolean
    }

    case object True extends BooleanValue {
      def value: Boolean = true
    }

    case object False extends BooleanValue {
      def value: Boolean = false
    }

    case class CharValue(value: Char) extends Value with CharExpression

    sealed trait AnyRefValue extends Value with ObjectExpression {
      def value: AnyRef
    }

    case object Null extends AnyRefValue {
      def value: AnyRef = null
    }

    case class StringValue(value: String) extends AnyRefValue with ObjectExpression

    case class ObjectValue(value: AnyRef) extends AnyRefValue with ObjectExpression
  }
}
