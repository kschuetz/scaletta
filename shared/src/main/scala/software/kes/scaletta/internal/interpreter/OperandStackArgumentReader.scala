package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.runtime.ParamsSignature

import scala.collection.immutable.ArraySeq

private class OperandStackArgumentReader(stack: OperandStack,
                                         signature: ParamsSignature) extends ArgumentReader {
  def argCount: Int = signature.paramCount

  def read(index: Int): Any =
    signature.basicTypeOf(index) match {
      case BasicTypes.Boolean => unsafeReadBoolean(index)
      case BasicTypes.Int => unsafeReadInt(index)
      case BasicTypes.Long => unsafeReadLong(index)
      case BasicTypes.Short => unsafeReadShort(index)
      case BasicTypes.Byte => unsafeReadByte(index)
      case BasicTypes.Char => unsafeReadChar(index)
      case BasicTypes.Double => unsafeReadDouble(index)
      case BasicTypes.Float => unsafeReadFloat(index)
      case _ => unsafeReadObject(index)
    }

  def toVector: Vector[Any] = {
    val out = Vector.newBuilder[Any]
    for (i <- 0 until argCount) {
      out += read(i)
    }
    out.result()
  }

  def toArray: Array[Any] = {
    val out = Array.newBuilder[Any]
    for (i <- 0 until argCount) {
      out += read(i)
    }
    out.result()
  }

  def unsafeReadBoolean(index: Int): Boolean =
    stack.booleans.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadByte(index: Int): Byte =
    stack.bytes.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadChar(index: Int): Char =
    stack.chars.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadDouble(index: Int): Double =
    stack.doubles.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadFloat(index: Int): Float =
    stack.floats.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadInt(index: Int): Int =
    stack.ints.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadLong(index: Int): Long =
    stack.longs.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadShort(index: Int): Short =
    stack.shorts.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadObject(index: Int): AnyRef =
    stack.objects.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadBooleanArray(index: Int): ArraySeq[Boolean] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Boolean]]

  def unsafeReadByteArray(index: Int): ArraySeq[Byte] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Byte]]

  def unsafeReadCharArray(index: Int): ArraySeq[Char] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Char]]

  def unsafeReadDoubleArray(index: Int): ArraySeq[Double] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Double]]

  def unsafeReadFloatArray(index: Int): ArraySeq[Float] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Float]]

  def unsafeReadIntArray(index: Int): ArraySeq[Int] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Int]]

  def unsafeReadLongArray(index: Int): ArraySeq[Long] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Long]]

  def unsafeReadShortArray(index: Int): ArraySeq[Short] =
    unsafeReadObject(index).asInstanceOf[ArraySeq[Short]]
}
