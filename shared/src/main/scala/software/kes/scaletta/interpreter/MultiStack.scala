package software.kes.scaletta.interpreter

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.runtime.ParamsSignature
import software.kes.scaletta.util.stack._

import scala.collection.immutable.ArraySeq

object MultiStack {
  def create(): MultiStack =
    new MultiStack(ByteStack.create(), ObjectStack.create(), BooleanStack.create(), IntStack.create(),
      LongStack.create(), ShortStack.create(), ByteStack.create(), CharStack.create(), DoubleStack.create(),
      FloatStack.create())
}

final class MultiStack private(private[interpreter] val control: ByteStack,
                               private[interpreter] val objects: ObjectStack,
                               private[interpreter] val booleans: BooleanStack,
                               private[interpreter] val ints: IntStack,
                               private[interpreter] val longs: LongStack,
                               private[interpreter] val shorts: ShortStack,
                               private[interpreter] val bytes: ByteStack,
                               private[interpreter] val chars: CharStack,
                               private[interpreter] val doubles: DoubleStack,
                               private[interpreter] val floats: FloatStack) {

  def size(): Int =
    control.size()

  def isEmpty: Boolean =
    control.isEmpty

  def push(value: Any): Unit =
    value match {
      case x: AnyRef => pushObject(x)
      case x: Boolean => pushBoolean(x)
      case x: Int => pushInt(x)
      case x: Long => pushLong(x)
      case x: Short => pushShort(x)
      case x: Byte => pushByte(x)
      case x: Char => pushChar(x)
      case x: Double => pushDouble(x)
      case x: Float => pushFloat(x)
    }

  def pushObject(value: AnyRef): Unit = {
    control.push(BasicTypes.Object)
    objects.push(value)
  }

  def pushBoolean(value: Boolean): Unit = {
    control.push(BasicTypes.Boolean)
    booleans.push(value)
  }

  def pushInt(value: Int): Unit = {
    control.push(BasicTypes.Int)
    ints.push(value)
  }

  def pushLong(value: Long): Unit = {
    control.push(BasicTypes.Long)
    longs.push(value)
  }

  def pushShort(value: Short): Unit = {
    control.push(BasicTypes.Short)
    shorts.push(value)
  }

  def pushByte(value: Byte): Unit = {
    control.push(BasicTypes.Byte)
    bytes.push(value)
  }

  def pushChar(value: Char): Unit = {
    control.push(BasicTypes.Char)
    chars.push(value)
  }

  def pushDouble(value: Double): Unit = {
    control.push(BasicTypes.Double)
    doubles.push(value)
  }

  def pushFloat(value: Float): Unit = {
    control.push(BasicTypes.Float)
    floats.push(value)
  }

  def peekBasicType: Option[Byte] =
    control.peek()

  def peek: Option[Any] = {
    if (control.isEmpty) None
    else control.unsafeGet(0) match {
      case BasicTypes.Boolean => booleans.peek()
      case BasicTypes.Int => ints.peek()
      case BasicTypes.Long => longs.peek()
      case BasicTypes.Short => shorts.peek()
      case BasicTypes.Byte => bytes.peek()
      case BasicTypes.Char => chars.peek()
      case BasicTypes.Double => doubles.peek()
      case BasicTypes.Float => floats.peek()
      case _ => objects.peek()
    }
  }

  def pop(): Any = {
    val basicType = control.pop()
    basicType match {
      case BasicTypes.Object => objects.pop()
      case BasicTypes.Boolean => booleans.pop()
      case BasicTypes.Int => ints.pop()
      case BasicTypes.Long => longs.pop()
      case BasicTypes.Short => shorts.pop()
      case BasicTypes.Byte => bytes.pop()
      case BasicTypes.Char => chars.pop()
      case BasicTypes.Double => doubles.pop()
      case BasicTypes.Float => floats.pop()
      case _ => throw new IllegalStateException(s"Unknown type: $basicType")
    }
  }

  def unsafePopObject(): AnyRef = {
    control.pop()
    objects.pop()
  }

  def unsafePopBoolean(): Boolean = {
    control.pop()
    booleans.pop()
  }

  def unsafePopInt(): Int = {
    control.pop()
    ints.pop()
  }

  def unsafePopLong(): Long = {
    control.pop()
    longs.pop()
  }

  def unsafePopShort(): Short = {
    control.pop()
    shorts.pop()
  }

  def unsafePopByte(): Byte = {
    control.pop()
    bytes.pop()
  }

  def unsafePopChar(): Char = {
    control.pop()
    chars.pop()
  }

  def unsafePopDouble(): Double = {
    control.pop()
    doubles.pop()
  }

  def unsafePopFloat(): Float = {
    control.pop()
    floats.pop()
  }

  def clear(): Unit = {
    control.clear()
    objects.clear()
    booleans.clear()
    ints.clear()
    longs.clear()
    shorts.clear()
    bytes.clear()
    chars.clear()
    doubles.clear()
    floats.clear()
  }

  def argumentReader(signature: ParamsSignature): ArgumentReader =
    new MultiStackArgumentReader(this, signature)
}

private class MultiStackArgumentReader(stack: MultiStack,
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
    stack.booleans.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadByte(index: Int): Byte =
    stack.bytes.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadChar(index: Int): Char =
    stack.chars.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadDouble(index: Int): Double =
    stack.doubles.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadFloat(index: Int): Float =
    stack.floats.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadInt(index: Int): Int =
    stack.ints.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadLong(index: Int): Long =
    stack.longs.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadShort(index: Int): Short =
    stack.shorts.unsafeGet(signature.stackOffsetOf(index))

  def unsafeReadObject(index: Int): AnyRef =
    stack.objects.unsafeGet(signature.stackOffsetOf(index))

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
