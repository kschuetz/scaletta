package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.api.NativeFunctionId
import software.kes.scaletta.common.BasicType
import software.kes.scaletta.internal.runtime.UserFunctionSignature
import software.kes.scaletta.util.VectorTwoPlus

sealed trait IntermediateExpression

object IntermediateExpression {
  case class NativeCall(target: NativeFunctionId,
                        arguments: Vector[IntermediateExpression]) extends IntermediateExpression

  case class LocalCall(scope: Int,
                       slot: Int,
                       arguments: Vector[IntermediateExpression]) extends IntermediateExpression

  case class ClosureCall(target: IntermediateExpression,
                         arguments: Vector[IntermediateExpression],
                         returnType: BasicType) extends IntermediateExpression

  case class Conditional(condition: IntermediateExpression,
                         thenBranch: IntermediateExpression,
                         elseBranch: IntermediateExpression) extends IntermediateExpression

  case class WithBindings(bindings: Vector[Binding],
                          body: IntermediateExpression) extends IntermediateExpression

  case class Reference(scope: Int,
                       slot: Int) extends IntermediateExpression

  case class Lambda(signature: UserFunctionSignature,
                    captures: Vector[IntermediateExpression.Reference],
                    body: IntermediateExpression) extends IntermediateExpression

  case class FunctionValue(scope: Int,
                           slot: Int,
                           signature: UserFunctionSignature,
                           captures: Vector[IntermediateExpression.Reference]) extends IntermediateExpression

  case class PartialNativeFunctionApplication(functionId: NativeFunctionId,
                                              arguments: Vector[Option[IntermediateExpression]]) extends IntermediateExpression

  case class And(lhs: IntermediateExpression, rhs: IntermediateExpression) extends IntermediateExpression

  case class Or(lhs: IntermediateExpression, rhs: IntermediateExpression) extends IntermediateExpression

  case class StringConcat(segments: Vector[IntermediateExpression]) extends IntermediateExpression

  case class Convert(value: IntermediateExpression,
                     targetType: BasicType) extends IntermediateExpression

  case class Tuple(elements: VectorTwoPlus[IntermediateExpression]) extends IntermediateExpression

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
      case () => unit()
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

    def unit(): Value = UnitValue

    case class IntValue(value: Int) extends Value

    case class LongValue(value: Long) extends Value

    case class FloatValue(value: Float) extends Value

    case class DoubleValue(value: Double) extends Value

    case class ShortValue(value: Short) extends Value

    case class ByteValue(value: Byte) extends Value

    sealed trait BooleanValue extends Value {
      def value: Boolean
    }

    case object True extends BooleanValue {
      def value: Boolean = true
    }

    case object False extends BooleanValue {
      def value: Boolean = false
    }

    case class CharValue(value: Char) extends Value

    case object UnitValue extends Value

    sealed trait AnyRefValue extends Value {
      def value: AnyRef
    }

    case object Null extends AnyRefValue {
      def value: AnyRef = null
    }

    case class StringValue(value: String) extends AnyRefValue

    case class ObjectValue(value: AnyRef) extends AnyRefValue
  }
}
