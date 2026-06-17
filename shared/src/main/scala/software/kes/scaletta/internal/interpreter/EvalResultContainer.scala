package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.EvalResult
import software.kes.scaletta.common.{BasicType, BasicTypes}

trait EvalResultContainer extends EvalResult {
  def loadFromOperandStack(operandStack: OperandStack): Unit
}

private[interpreter] object EvalResultContainer {
  def create(basicType: BasicType): EvalResultContainer =
    basicType match {
      case BasicTypes.Boolean => new BooleanEvalResult(false)
      case BasicTypes.Int => new IntEvalResult(0)
      case BasicTypes.Long => new LongEvalResult(0L)
      case BasicTypes.Short => new ShortEvalResult(0.toShort)
      case BasicTypes.Byte => new ByteEvalResult(0.toByte)
      case BasicTypes.Char => new CharEvalResult(0.toChar)
      case BasicTypes.Double => new DoubleEvalResult(0d)
      case BasicTypes.Float => new FloatEvalResult(0f)
      case _ => new AnyEvalResult(null)
    }

  private class AnyEvalResult(var result: Any) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.pop()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean =
      result match {
        case x: Boolean => x
        case x: Byte => x != 0.toByte
        case x: Short => x != 0.toShort
        case x: Char => x != 0.toChar
        case x: Int => x != 0
        case x: Long => x != 0L
        case x: Float => x != 0f
        case x: Double => x != 0d
        case _ => false
      }

    def intValue(): Int =
      result match {
        case x: Boolean => if (x) 1 else 0
        case x: Byte => x.toInt
        case x: Short => x.toInt
        case x: Char => x.toInt
        case x: Int => x
        case x: Long => x.toInt
        case x: Float => x.toInt
        case x: Double => x.toInt
        case _ => 0
      }

    def longValue(): Long =
      result match {
        case x: Boolean => if (x) 1 else 0
        case x: Byte => x.toLong
        case x: Short => x.toLong
        case x: Char => x.toLong
        case x: Int => x.toLong
        case x: Long => x
        case x: Float => x.toLong
        case x: Double => x.toLong
        case _ => 0L
      }

    def shortValue(): Short =
      result match {
        case x: Boolean => if (x) 1 else 0
        case x: Byte => x.toShort
        case x: Short => x
        case x: Char => x.toShort
        case x: Int => x.toShort
        case x: Long => x.toShort
        case x: Float => x.toShort
        case x: Double => x.toShort
        case _ => 0.toShort
      }

    def byteValue(): Byte =
      result match {
        case x: Boolean => if (x) 1.toByte else 0.toByte
        case x: Byte => x
        case x: Short => x.toByte
        case x: Char => x.toByte
        case x: Int => x.toByte
        case x: Long => x.toByte
        case x: Float => x.toByte
        case x: Double => x.toByte
        case _ => 0.toByte
      }

    def charValue(): Char =
      result match {
        case x: Boolean => if (x) 1.toChar else 0.toChar
        case x: Byte => x.toChar
        case x: Short => x.toChar
        case x: Char => x
        case x: Int => x.toChar
        case x: Long => x.toChar
        case x: Float => x.toChar
        case x: Double => x.toChar
        case _ => 0.toChar
      }

    def doubleValue(): Double =
      result match {
        case x: Boolean => if (x) 1d else 0d
        case x: Byte => x.toDouble
        case x: Short => x.toDouble
        case x: Char => x.toDouble
        case x: Int => x.toDouble
        case x: Long => x.toDouble
        case x: Float => x.toDouble
        case x: Double => x
        case _ => 0d
      }

    def floatValue(): Float =
      result match {
        case x: Boolean => if (x) 1f else 0f
        case x: Byte => x.toFloat
        case x: Short => x.toFloat
        case x: Char => x.toFloat
        case x: Int => x.toFloat
        case x: Long => x.toFloat
        case x: Float => x
        case x: Double => x.toFloat
        case _ => 0f
      }
  }

  private class BooleanEvalResult(var result: Boolean) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopBoolean()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result

    def intValue(): Int = if (result) 1 else 0

    def longValue(): Long = if (result) 1L else 0L

    def shortValue(): Short = if (result) 1.toShort else 0.toShort

    def byteValue(): Byte = if (result) 1.toByte else 0.toByte

    def charValue(): Char = if (result) 1.toChar else 0.toChar

    def doubleValue(): Double = if (result) 1d else 0d

    def floatValue(): Float = if (result) 1f else 0f
  }

  private class IntEvalResult(var result: Int) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopInt()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0

    def intValue(): Int = result

    def longValue(): Long = result.toLong

    def shortValue(): Short = result.toShort

    def byteValue(): Byte = result.toByte

    def charValue(): Char = result.toChar

    def doubleValue(): Double = result.toDouble

    def floatValue(): Float = result.toFloat
  }

  private class LongEvalResult(var result: Long) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopLong()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0L

    def intValue(): Int = result.toInt

    def longValue(): Long = result

    def shortValue(): Short = result.toShort

    def byteValue(): Byte = result.toByte

    def charValue(): Char = result.toChar

    def doubleValue(): Double = result.toDouble

    def floatValue(): Float = result.toFloat
  }

  private class ShortEvalResult(var result: Short) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopShort()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0.toShort

    def intValue(): Int = result.toInt

    def longValue(): Long = result.toLong

    def shortValue(): Short = result

    def byteValue(): Byte = result.toByte

    def charValue(): Char = result.toChar

    def doubleValue(): Double = result.toDouble

    def floatValue(): Float = result.toFloat
  }

  private class ByteEvalResult(var result: Byte) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopByte()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0.toByte

    def intValue(): Int = result.toInt

    def longValue(): Long = result.toLong

    def shortValue(): Short = result.toShort

    def byteValue(): Byte = result

    def charValue(): Char = result.toChar

    def doubleValue(): Double = result.toDouble

    def floatValue(): Float = result.toFloat
  }

  private class CharEvalResult(var result: Char) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopChar()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0.toChar

    def intValue(): Int = result.toInt

    def longValue(): Long = result.toLong

    def shortValue(): Short = result.toShort

    def byteValue(): Byte = result.toByte

    def charValue(): Char = result

    def doubleValue(): Double = result.toDouble

    def floatValue(): Float = result.toFloat
  }

  private class DoubleEvalResult(var result: Double) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopDouble()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0d

    def intValue(): Int = result.toInt

    def longValue(): Long = result.toLong

    def shortValue(): Short = result.toShort

    def byteValue(): Byte = result.toByte

    def charValue(): Char = result.toChar

    def doubleValue(): Double = result

    def floatValue(): Float = result.toFloat
  }

  private class FloatEvalResult(var result: Float) extends EvalResultContainer {
    def loadFromOperandStack(operandStack: OperandStack): Unit =
      result = operandStack.unsafePopFloat()

    def value[A](): A = result.asInstanceOf[A]

    def booleanValue(): Boolean = result != 0f

    def intValue(): Int = result.toInt

    def longValue(): Long = result.toLong

    def shortValue(): Short = result.toShort

    def byteValue(): Byte = result.toByte

    def charValue(): Char = result.toChar

    def doubleValue(): Double = result.toDouble

    def floatValue(): Float = result
  }
}
