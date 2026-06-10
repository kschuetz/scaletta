package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.ParamsSignature
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
      case x: Boolean => pushBoolean(x)
      case x: Int => pushInt(x)
      case x: Long => pushLong(x)
      case x: Short => pushShort(x)
      case x: Byte => pushByte(x)
      case x: Char => pushChar(x)
      case x: Double => pushDouble(x)
      case x: Float => pushFloat(x)
      case x: AnyRef => pushObject(x)
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
    (basicType: @annotation.switch) match {
      case BasicTypes.Object =>
        val value = objects.unsafeRead(0)
        objects.contract(1)
        value
      case BasicTypes.Boolean =>
        val value = booleans.unsafeRead(0)
        booleans.contract(1)
        value
      case BasicTypes.Int =>
        val value = ints.unsafeRead(0)
        ints.contract(1)
        value
      case BasicTypes.Long =>
        val value = longs.unsafeRead(0)
        longs.contract(1)
        value
      case BasicTypes.Short =>
        val value = shorts.unsafeRead(0)
        shorts.contract(1)
        value
      case BasicTypes.Byte =>
        val value = bytes.unsafeRead(0)
        bytes.contract(1)
        value
      case BasicTypes.Char =>
        val value = chars.unsafeRead(0)
        chars.contract(1)
        value
      case BasicTypes.Double =>
        val value = doubles.unsafeRead(0)
        doubles.contract(1)
        value
      case BasicTypes.Float =>
        val value = floats.unsafeRead(0)
        floats.contract(1)
        value
      case _ => throw new IllegalStateException(s"Unknown type: $basicType")
    }
  }

  /**
   * Pops the value at the top of the stack (regardless of type), and returns true if the value is "truthy".
   */
  def popCondition(): Boolean = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Object =>
        val value = objects.unsafeRead(0)
        objects.contract(1)
        value != null
      case BasicTypes.Boolean =>
        val value = booleans.unsafeRead(0)
        booleans.contract(1)
        value
      case BasicTypes.Int =>
        val value = ints.unsafeRead(0)
        ints.contract(1)
        value != 0
      case BasicTypes.Long =>
        val value = longs.unsafeRead(0)
        longs.contract(1)
        value != 0L
      case BasicTypes.Short =>
        val value = shorts.unsafeRead(0)
        shorts.contract(1)
        value != 0
      case BasicTypes.Byte =>
        val value = bytes.unsafeRead(0)
        bytes.contract(1)
        value != 0
      case BasicTypes.Char =>
        val value = chars.unsafeRead(0)
        chars.contract(1)
        value != 0
      case BasicTypes.Double =>
        val value = doubles.unsafeRead(0)
        doubles.contract(1)
        value != 0d
      case BasicTypes.Float =>
        val value = floats.unsafeRead(0)
        floats.contract(1)
        value != 0f
      case _ => throw new IllegalStateException(s"Unknown type: $basicType")
    }
  }

  /**
   * Peeks the top of the stack (regardless of type) and returns true if that value is "truthy".
   *
   * @param popIfEquals if the result on the top of the stack is equal to this value,
   *                    then the value is popped from the stack, otherwise the stack is left unchanged
   */
  def maybePopCondition(popIfEquals: Boolean): Boolean = {
    val basicType = control.unsafeRead(0)
    (basicType: @annotation.switch) match {
      case BasicTypes.Object =>
        val value = objects.unsafeRead(0)
        val result = value != null
        if (result == popIfEquals) {
          objects.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Boolean =>
        val result = booleans.unsafeRead(0)
        if (result == popIfEquals) {
          booleans.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Int =>
        val value = ints.unsafeRead(0)
        val result = value != 0
        if (result == popIfEquals) {
          ints.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Long =>
        val value = longs.unsafeRead(0)
        val result = value != 0L
        if (result == popIfEquals) {
          longs.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Short =>
        val value = shorts.unsafeRead(0)
        val result = value != 0
        if (result == popIfEquals) {
          shorts.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Byte =>
        val value = bytes.unsafeRead(0)
        val result = value != 0
        if (result == popIfEquals) {
          bytes.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Char =>
        val value = chars.unsafeRead(0)
        val result = value != 0
        if (result == popIfEquals) {
          chars.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Double =>
        val value = doubles.unsafeRead(0)
        val result = value != 0d
        if (result == popIfEquals) {
          doubles.contract(1)
          control.contract(1)
        }
        result
      case BasicTypes.Float =>
        val value = floats.unsafeRead(0)
        val result = value != 0f
        if (result == popIfEquals) {
          floats.contract(1)
          control.contract(1)
        }
        result
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

  /**
   * Duplicates the top value on the stack.
   * This is used by the Dup opcode.
   */
  def duplicate(): Unit = {
    control.duplicate()
    val basicType = control.unsafeRead(0)
    (basicType: @annotation.switch) match {
      case BasicTypes.Object => objects.duplicate()
      case BasicTypes.Boolean => booleans.duplicate()
      case BasicTypes.Int => ints.duplicate()
      case BasicTypes.Long => longs.duplicate()
      case BasicTypes.Short => shorts.duplicate()
      case BasicTypes.Byte => bytes.duplicate()
      case BasicTypes.Char => chars.duplicate()
      case BasicTypes.Double => doubles.duplicate()
      case BasicTypes.Float => floats.duplicate()
      case _ =>
        throw new IllegalStateException(s"Unknown type: $basicType")
    }
  }

  /**
   * Swaps the top two values on the stack.
   * This is used by the Swap opcode.
   */
  def swap(): Unit = {
    val size = control.size()
    if (size < 2) {
      throw new IllegalStateException("Cannot swap with fewer than 2 values on the stack")
    }
    val b = pop()
    val a = pop()
    push(b)
    push(a)
  }

  def argumentReader(signature: ParamsSignature): ArgumentReader =
    new OperandStackArgumentReader(this, signature)

  def contract(signature: ParamsSignature): Unit = {
    val count = signature.paramCount
    if (count > 0) {
      val typeCounts = signature.typeCounts
      objects.contract(typeCounts(BasicTypes.Object))
      booleans.contract(typeCounts(BasicTypes.Boolean))
      ints.contract(typeCounts(BasicTypes.Int))
      longs.contract(typeCounts(BasicTypes.Long))
      shorts.contract(typeCounts(BasicTypes.Short))
      bytes.contract(typeCounts(BasicTypes.Byte))
      chars.contract(typeCounts(BasicTypes.Char))
      doubles.contract(typeCounts(BasicTypes.Double))
      floats.contract(typeCounts(BasicTypes.Float))
      control.contract(count)
    }
  }
}
