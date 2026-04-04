package software.kes.scaletta.interpreter

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.runtime.ParamsSignature
import software.kes.scaletta.util.stack._

object OperandStack {
  def create(): OperandStack =
    new OperandStack(ByteStack.create(), ObjectStack.create(), BooleanStack.create(),
      IntStack.create(), LongStack.create(), ShortStack.create(), ByteStack.create(),
      CharStack.create(), DoubleStack.create(), FloatStack.create()
    )
}

final class OperandStack(private[interpreter] val control: ByteStack,
                         private[interpreter] val objects: ObjectStack,
                         private[interpreter] val booleans: BooleanStack,
                         private[interpreter] val ints: IntStack,
                         private[interpreter] val longs: LongStack,
                         private[interpreter] val shorts: ShortStack,
                         private[interpreter] val bytes: ByteStack,
                         private[interpreter] val chars: CharStack,
                         private[interpreter] val doubles: DoubleStack,
                         private[interpreter] val floats: FloatStack
                        ) {

  def size(): Int = control.size()

  def isEmpty: Boolean = control.isEmpty

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
    else control.unsafeRead(0) match {
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
    objects.clear()
    booleans.clear()
    ints.clear()
    longs.clear()
    shorts.clear()
    bytes.clear()
    chars.clear()
    doubles.clear()
    floats.clear()
    control.clear()
  }

  def argumentReader(signature: ParamsSignature): ArgumentReader =
    new OperandStackArgumentReader(this, signature)
}
