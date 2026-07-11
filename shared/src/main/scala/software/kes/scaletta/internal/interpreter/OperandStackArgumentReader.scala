package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.ParamsSignature
import software.kes.scaletta.util.conversions.ObjectToPrimitive

import scala.collection.immutable.ArraySeq

/**
 * Mutable. The same instance will be reused for every native call.
 */
private[interpreter] class OperandStackArgumentReader(stack: OperandStack,
                                                      var signature: ParamsSignature) extends ArgumentReader {
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

  /**
   * Reads the argument at index and converts it to a Boolean.
   * Safer than [[unsafeReadBoolean]] but slower.
   */
  def readAsBoolean(index: Int): Boolean = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Boolean) {
      unsafeReadBoolean(index)
    } else {
      readAndConvertToBoolean(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to an Int.
   * Safer than [[unsafeReadInt]] but slower.
   */
  def readAsInt(index: Int): Int = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Int) {
      unsafeReadInt(index)
    } else {
      readAndConvertToInt(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to a Long.
   * Safer than [[unsafeReadLong]] but slower.
   */
  def readAsLong(index: Int): Long = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Long) {
      unsafeReadLong(index)
    } else {
      readAndConvertToLong(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to a Short.
   * Safer than [[unsafeReadShort]] but slower.
   */
  def readAsShort(index: Int): Short = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Short) {
      unsafeReadShort(index)
    } else {
      readAndConvertToShort(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to a Byte.
   * Safer than [[unsafeReadByte]] but slower.
   */
  def readAsByte(index: Int): Byte = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Byte) {
      unsafeReadByte(index)
    } else {
      readAndConvertToByte(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to a Char.
   * Safer than [[unsafeReadChar]] but slower.
   */
  def readAsChar(index: Int): Char = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Char) {
      unsafeReadChar(index)
    } else {
      readAndConvertToChar(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to a Double.
   * Safer than [[unsafeReadDouble]] but slower.
   */
  def readAsDouble(index: Int): Double = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Double) {
      unsafeReadDouble(index)
    } else {
      readAndConvertToDouble(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to a Float.
   * Safer than [[unsafeReadFloat]] but slower.
   */
  def readAsFloat(index: Int): Float = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Float) {
      unsafeReadFloat(index)
    } else {
      readAndConvertToFloat(index, basicType)
    }
  }

  /**
   * Reads the argument at index and converts it to an object.
   * Safer than [[unsafeReadObject]] but slower.
   */
  def readAsObject(index: Int): AnyRef = {
    val basicType = signature.basicTypeOf(index)
    if (basicType == BasicTypes.Object) {
      unsafeReadObject(index)
    } else {
      read(index).asInstanceOf[AnyRef]
    }
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

  private def readAndConvertToBoolean(index: Int, incomingType: Byte): Boolean =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index) != 0
      case BasicTypes.Long => unsafeReadLong(index) != 0L
      case BasicTypes.Short => unsafeReadShort(index) != 0
      case BasicTypes.Byte => unsafeReadByte(index) != 0
      case BasicTypes.Char => unsafeReadChar(index) != 0
      case BasicTypes.Double => unsafeReadDouble(index) != 0.0d
      case BasicTypes.Float => unsafeReadFloat(index) != 0.0f
      case _ => ObjectToPrimitive.objectToBoolean(unsafeReadObject(index))
    }

  private def readAndConvertToInt(index: Int, incomingType: Byte): Int =
    incomingType match {
      case BasicTypes.Long => unsafeReadLong(index).toInt
      case BasicTypes.Short => unsafeReadShort(index).toInt
      case BasicTypes.Byte => unsafeReadByte(index).toInt
      case BasicTypes.Char => unsafeReadChar(index).toInt
      case BasicTypes.Double => unsafeReadDouble(index).toInt
      case BasicTypes.Float => unsafeReadFloat(index).toInt
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1 else 0
      case _ => ObjectToPrimitive.objectToInt(unsafeReadObject(index))
    }

  private def readAndConvertToLong(index: Int, incomingType: Byte): Long =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index).toLong
      case BasicTypes.Short => unsafeReadShort(index).toLong
      case BasicTypes.Byte => unsafeReadByte(index).toLong
      case BasicTypes.Char => unsafeReadChar(index).toLong
      case BasicTypes.Double => unsafeReadDouble(index).toLong
      case BasicTypes.Float => unsafeReadFloat(index).toLong
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1L else 0L
      case _ => ObjectToPrimitive.objectToLong(unsafeReadObject(index))
    }

  private def readAndConvertToShort(index: Int, incomingType: Byte): Short =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index).toShort
      case BasicTypes.Long => unsafeReadLong(index).toShort
      case BasicTypes.Byte => unsafeReadByte(index).toShort
      case BasicTypes.Char => unsafeReadChar(index).toShort
      case BasicTypes.Double => unsafeReadDouble(index).toShort
      case BasicTypes.Float => unsafeReadFloat(index).toShort
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1.toShort else 0.toShort
      case _ => ObjectToPrimitive.objectToShort(unsafeReadObject(index))
    }

  private def readAndConvertToByte(index: Int, incomingType: Byte): Byte =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index).toByte
      case BasicTypes.Long => unsafeReadLong(index).toByte
      case BasicTypes.Short => unsafeReadShort(index).toByte
      case BasicTypes.Char => unsafeReadChar(index).toByte
      case BasicTypes.Double => unsafeReadDouble(index).toByte
      case BasicTypes.Float => unsafeReadFloat(index).toByte
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1.toByte else 0.toByte
      case _ => ObjectToPrimitive.objectToByte(unsafeReadObject(index))
    }

  private def readAndConvertToChar(index: Int, incomingType: Byte): Char =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index).toChar
      case BasicTypes.Long => unsafeReadLong(index).toChar
      case BasicTypes.Short => unsafeReadShort(index).toChar
      case BasicTypes.Byte => unsafeReadByte(index).toChar
      case BasicTypes.Double => unsafeReadDouble(index).toChar
      case BasicTypes.Float => unsafeReadFloat(index).toChar
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1.toChar else 0.toChar
      case _ => ObjectToPrimitive.objectToChar(unsafeReadObject(index))
    }

  private def readAndConvertToDouble(index: Int, incomingType: Byte): Double =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index).toDouble
      case BasicTypes.Long => unsafeReadLong(index).toDouble
      case BasicTypes.Short => unsafeReadShort(index).toDouble
      case BasicTypes.Byte => unsafeReadByte(index).toDouble
      case BasicTypes.Char => unsafeReadChar(index).toDouble
      case BasicTypes.Float => unsafeReadFloat(index).toDouble
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1.0d else 0.0d
      case _ => ObjectToPrimitive.objectToDouble(unsafeReadObject(index))
    }

  private def readAndConvertToFloat(index: Int, incomingType: Byte): Float =
    incomingType match {
      case BasicTypes.Int => unsafeReadInt(index).toFloat
      case BasicTypes.Long => unsafeReadLong(index).toFloat
      case BasicTypes.Short => unsafeReadShort(index).toFloat
      case BasicTypes.Byte => unsafeReadByte(index).toFloat
      case BasicTypes.Char => unsafeReadChar(index).toFloat
      case BasicTypes.Double => unsafeReadDouble(index).toFloat
      case BasicTypes.Boolean => if (unsafeReadBoolean(index)) 1.0f else 0.0f
      case _ => ObjectToPrimitive.objectToFloat(unsafeReadObject(index))
    }
}
