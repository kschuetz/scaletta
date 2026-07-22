package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.common.{BasicType, BasicTypes}
import software.kes.scaletta.internal.runtime.ParamsSignature
import software.kes.scaletta.util.conversions.ObjectToPrimitive
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

  def popAsObject(): AnyRef = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Object => objects.pop()
      case BasicTypes.Boolean => java.lang.Boolean.valueOf(booleans.pop())
      case BasicTypes.Int => java.lang.Integer.valueOf(ints.pop())
      case BasicTypes.Long => java.lang.Long.valueOf(longs.pop())
      case BasicTypes.Short => java.lang.Short.valueOf(shorts.pop())
      case BasicTypes.Byte => java.lang.Byte.valueOf(bytes.pop())
      case BasicTypes.Char => java.lang.Character.valueOf(chars.pop())
      case BasicTypes.Double => java.lang.Double.valueOf(doubles.pop())
      case BasicTypes.Float => java.lang.Float.valueOf(floats.pop())
      case _ => throw new IllegalStateException(s"Unknown type: $basicType")
    }
  }

  def popAsBoolean(): Boolean = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Boolean => booleans.pop()
      case BasicTypes.Int => ints.pop() != 0
      case BasicTypes.Long => longs.pop() != 0L
      case BasicTypes.Short => shorts.pop() != 0
      case BasicTypes.Byte => bytes.pop() != 0
      case BasicTypes.Char => chars.pop() != 0
      case BasicTypes.Double => doubles.pop() != 0.0d
      case BasicTypes.Float => floats.pop() != 0.0f
      case _ => ObjectToPrimitive.objectToBoolean(objects.pop())
    }
  }

  def popAsInt(): Int = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Int => ints.pop()
      case BasicTypes.Long => longs.pop().toInt
      case BasicTypes.Short => shorts.pop().toInt
      case BasicTypes.Byte => bytes.pop().toInt
      case BasicTypes.Char => chars.pop().toInt
      case BasicTypes.Double => doubles.pop().toInt
      case BasicTypes.Float => floats.pop().toInt
      case BasicTypes.Boolean => if (booleans.pop()) 1 else 0
      case _ => ObjectToPrimitive.objectToInt(objects.pop())
    }
  }

  def popAsLong(): Long = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Long => longs.pop()
      case BasicTypes.Int => ints.pop().toLong
      case BasicTypes.Short => shorts.pop().toLong
      case BasicTypes.Byte => bytes.pop().toLong
      case BasicTypes.Char => chars.pop().toLong
      case BasicTypes.Double => doubles.pop().toLong
      case BasicTypes.Float => floats.pop().toLong
      case BasicTypes.Boolean => if (booleans.pop()) 1L else 0L
      case _ => ObjectToPrimitive.objectToLong(objects.pop())
    }
  }

  def popAsShort(): Short = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Short => shorts.pop()
      case BasicTypes.Int => ints.pop().toShort
      case BasicTypes.Long => longs.pop().toShort
      case BasicTypes.Byte => bytes.pop().toShort
      case BasicTypes.Char => chars.pop().toShort
      case BasicTypes.Double => doubles.pop().toShort
      case BasicTypes.Float => floats.pop().toShort
      case BasicTypes.Boolean => if (booleans.pop()) 1.toShort else 0.toShort
      case _ => ObjectToPrimitive.objectToShort(objects.pop())
    }
  }

  def popAsByte(): Byte = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Byte => bytes.pop()
      case BasicTypes.Int => ints.pop().toByte
      case BasicTypes.Long => longs.pop().toByte
      case BasicTypes.Short => shorts.pop().toByte
      case BasicTypes.Char => chars.pop().toByte
      case BasicTypes.Double => doubles.pop().toByte
      case BasicTypes.Float => floats.pop().toByte
      case BasicTypes.Boolean => if (booleans.pop()) 1.toByte else 0.toByte
      case _ => ObjectToPrimitive.objectToByte(objects.pop())
    }
  }

  def popAsChar(): Char = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Char => chars.pop()
      case BasicTypes.Int => ints.pop().toChar
      case BasicTypes.Long => longs.pop().toChar
      case BasicTypes.Short => shorts.pop().toChar
      case BasicTypes.Byte => bytes.pop().toChar
      case BasicTypes.Double => doubles.pop().toChar
      case BasicTypes.Float => floats.pop().toChar
      case BasicTypes.Boolean => if (booleans.pop()) 1.toChar else 0.toChar
      case _ => ObjectToPrimitive.objectToChar(objects.pop())
    }
  }

  def popAsDouble(): Double = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Double => doubles.pop()
      case BasicTypes.Int => ints.pop().toDouble
      case BasicTypes.Long => longs.pop().toDouble
      case BasicTypes.Short => shorts.pop().toDouble
      case BasicTypes.Byte => bytes.pop().toDouble
      case BasicTypes.Char => chars.pop().toDouble
      case BasicTypes.Float => floats.pop().toDouble
      case BasicTypes.Boolean => if (booleans.pop()) 1.0d else 0.0d
      case _ => ObjectToPrimitive.objectToDouble(objects.pop())
    }
  }

  def popAsFloat(): Float = {
    val basicType = control.pop()
    (basicType: @annotation.switch) match {
      case BasicTypes.Float => floats.pop()
      case BasicTypes.Int => ints.pop().toFloat
      case BasicTypes.Long => longs.pop().toFloat
      case BasicTypes.Short => shorts.pop().toFloat
      case BasicTypes.Byte => bytes.pop().toFloat
      case BasicTypes.Char => chars.pop().toFloat
      case BasicTypes.Double => doubles.pop().toFloat
      case BasicTypes.Boolean => if (booleans.pop()) 1.0f else 0.0f
      case _ => ObjectToPrimitive.objectToFloat(objects.pop())
    }
  }

  def unsafeReadControl(position: Int): Byte =
    control.unsafeRead(position)

  def unsafeReadObject(position: Int): AnyRef =
    objects.unsafeRead(position)

  def unsafeReadBoolean(position: Int): Boolean =
    booleans.unsafeRead(position)

  def unsafeReadInt(position: Int): Int =
    ints.unsafeRead(position)

  def unsafeReadLong(position: Int): Long =
    longs.unsafeRead(position)

  def unsafeReadShort(position: Int): Short =
    shorts.unsafeRead(position)

  def unsafeReadByte(position: Int): Byte =
    bytes.unsafeRead(position)

  def unsafeReadChar(position: Int): Char =
    chars.unsafeRead(position)

  def unsafeReadDouble(position: Int): Double =
    doubles.unsafeRead(position)

  def unsafeReadFloat(position: Int): Float =
    floats.unsafeRead(position)

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
    val bType = control.pop()
    val aType = control.pop()
    control.push(bType)
    control.push(aType)

    // Since we've already swapped the control stack, we only need to swap the values on the
    // operand stack if both values are of the same type.
    if (aType == bType) {
      (aType: @annotation.switch) match {
        case BasicTypes.Object => objects.swap()
        case BasicTypes.Boolean => booleans.swap()
        case BasicTypes.Int => ints.swap()
        case BasicTypes.Long => longs.swap()
        case BasicTypes.Short => shorts.swap()
        case BasicTypes.Byte => bytes.swap()
        case BasicTypes.Char => chars.swap()
        case BasicTypes.Double => doubles.swap()
        case BasicTypes.Float => floats.swap()
        case _ => ()
      }
    }
  }

  /**
   * Boxes the top value on the operand stack. If it is already an object, it is left unchanged.
   */
  def box(): Unit = {
    control.pop() match {
      case BasicTypes.Boolean =>
        pushObject(Boolean.box(booleans.pop()))
      case BasicTypes.Int =>
        pushObject(Int.box(ints.pop()))
      case BasicTypes.Long =>
        pushObject(Long.box(longs.pop()))
      case BasicTypes.Short =>
        pushObject(Short.box(shorts.pop()))
      case BasicTypes.Byte =>
        pushObject(Byte.box(bytes.pop()))
      case BasicTypes.Char =>
        pushObject(Char.box(chars.pop()))
      case BasicTypes.Double =>
        pushObject(Double.box(doubles.pop()))
      case BasicTypes.Float =>
        pushObject(Float.box(floats.pop()))
      case other => control.push(other) // already an object
    }
  }

  /**
   * Converts the value on the type of the stack to the specified type.
   * Always makes the best effort; if conversion is not possible, the value is set to zero.
   */
  def convert(toType: BasicType): Unit =
    control.pop() match {
      case BasicTypes.Boolean => toType match {
        case BasicTypes.Boolean => control.push(BasicTypes.Boolean)
        case BasicTypes.Int => pushInt(if (booleans.pop()) 1 else 0)
        case BasicTypes.Long => pushLong(if (booleans.pop()) 1L else 0L)
        case BasicTypes.Short => pushShort(if (booleans.pop()) 1.toShort else 0.toShort)
        case BasicTypes.Byte => pushByte(if (booleans.pop()) 1.toByte else 0.toByte)
        case BasicTypes.Char => pushChar(if (booleans.pop()) 1.toChar else 0.toChar)
        case BasicTypes.Double => pushDouble(if (booleans.pop()) 1.0d else 0.0d)
        case BasicTypes.Float => pushFloat(if (booleans.pop()) 1.0f else 0.0f)
        case _ => pushObject(Boolean.box(booleans.pop()))
      }
      case BasicTypes.Int => toType match {
        case BasicTypes.Boolean => pushBoolean(ints.pop() != 0)
        case BasicTypes.Int => control.push(BasicTypes.Int)
        case BasicTypes.Long => pushLong(ints.pop().toLong)
        case BasicTypes.Short => pushShort(ints.pop().toShort)
        case BasicTypes.Byte => pushByte(ints.pop().toByte)
        case BasicTypes.Char => pushChar(ints.pop().toChar)
        case BasicTypes.Double => pushDouble(ints.pop().toDouble)
        case BasicTypes.Float => pushFloat(ints.pop().toFloat)
        case _ => pushObject(Int.box(ints.pop()))
      }
      case BasicTypes.Long => toType match {
        case BasicTypes.Boolean => pushBoolean(longs.pop() != 0)
        case BasicTypes.Int => pushInt(longs.pop().toInt)
        case BasicTypes.Long => control.push(BasicTypes.Long)
        case BasicTypes.Short => pushShort(longs.pop().toShort)
        case BasicTypes.Byte => pushByte(longs.pop().toByte)
        case BasicTypes.Char => pushChar(longs.pop().toChar)
        case BasicTypes.Double => pushDouble(longs.pop().toDouble)
        case BasicTypes.Float => pushFloat(longs.pop().toFloat)
        case _ => pushObject(Long.box(longs.pop()))
      }
      case BasicTypes.Short => toType match {
        case BasicTypes.Boolean => pushBoolean(shorts.pop() != 0)
        case BasicTypes.Int => pushInt(shorts.pop().toInt)
        case BasicTypes.Long => pushLong(shorts.pop().toLong)
        case BasicTypes.Short => control.push(BasicTypes.Short)
        case BasicTypes.Byte => pushByte(shorts.pop().toByte)
        case BasicTypes.Char => pushChar(shorts.pop().toChar)
        case BasicTypes.Double => pushDouble(shorts.pop().toDouble)
        case BasicTypes.Float => pushFloat(shorts.pop().toFloat)
        case _ => pushObject(Short.box(shorts.pop()))
      }
      case BasicTypes.Byte => toType match {
        case BasicTypes.Boolean => pushBoolean(bytes.pop() != 0)
        case BasicTypes.Int => pushInt(bytes.pop().toInt)
        case BasicTypes.Long => pushLong(bytes.pop().toLong)
        case BasicTypes.Short => pushShort(bytes.pop().toShort)
        case BasicTypes.Byte => control.push(BasicTypes.Byte)
        case BasicTypes.Char => pushChar(bytes.pop().toChar)
        case BasicTypes.Double => pushDouble(bytes.pop().toDouble)
        case BasicTypes.Float => pushFloat(bytes.pop().toFloat)
        case _ => pushObject(Byte.box(bytes.pop()))
      }
      case BasicTypes.Char => toType match {
        case BasicTypes.Boolean => pushBoolean(chars.pop() != 0)
        case BasicTypes.Int => pushInt(chars.pop().toInt)
        case BasicTypes.Long => pushLong(chars.pop().toLong)
        case BasicTypes.Short => pushShort(chars.pop().toShort)
        case BasicTypes.Byte => pushByte(chars.pop().toByte)
        case BasicTypes.Char => control.push(BasicTypes.Char)
        case BasicTypes.Double => pushDouble(chars.pop().toDouble)
        case BasicTypes.Float => pushFloat(chars.pop().toFloat)
        case _ => pushObject(Char.box(chars.pop()))
      }
      case BasicTypes.Double => toType match {
        case BasicTypes.Boolean => pushBoolean(doubles.pop() != 0)
        case BasicTypes.Int => pushInt(doubles.pop().toInt)
        case BasicTypes.Long => pushLong(doubles.pop().toLong)
        case BasicTypes.Short => pushShort(doubles.pop().toShort)
        case BasicTypes.Byte => pushByte(doubles.pop().toByte)
        case BasicTypes.Char => pushChar(doubles.pop().toChar)
        case BasicTypes.Double => control.push(BasicTypes.Double)
        case BasicTypes.Float => pushFloat(doubles.pop().toFloat)
        case _ => pushObject(Double.box(doubles.pop()))
      }
      case BasicTypes.Float => toType match {
        case BasicTypes.Boolean => pushBoolean(floats.pop() != 0)
        case BasicTypes.Int => pushInt(floats.pop().toInt)
        case BasicTypes.Long => pushLong(floats.pop().toLong)
        case BasicTypes.Short => pushShort(floats.pop().toShort)
        case BasicTypes.Byte => pushByte(floats.pop().toByte)
        case BasicTypes.Char => pushChar(floats.pop().toChar)
        case BasicTypes.Double => pushDouble(floats.pop().toDouble)
        case BasicTypes.Float => control.push(BasicTypes.Float)
        case _ => pushObject(Float.box(floats.pop()))
      }
      case _ => toType match {
        case BasicTypes.Boolean => pushBoolean(ObjectToPrimitive.objectToBoolean(objects.pop()))
        case BasicTypes.Int => pushInt(ObjectToPrimitive.objectToInt(objects.pop()))
        case BasicTypes.Long => pushLong(ObjectToPrimitive.objectToLong(objects.pop()))
        case BasicTypes.Short => pushShort(ObjectToPrimitive.objectToShort(objects.pop()))
        case BasicTypes.Byte => pushByte(ObjectToPrimitive.objectToByte(objects.pop()))
        case BasicTypes.Char => pushChar(ObjectToPrimitive.objectToChar(objects.pop()))
        case BasicTypes.Double => pushDouble(ObjectToPrimitive.objectToDouble(objects.pop()))
        case BasicTypes.Float => pushFloat(ObjectToPrimitive.objectToFloat(objects.pop()))
        case _ => control.push(BasicTypes.Object)
      }
    }

  def argumentReader(signature: ParamsSignature): ArgumentReader =
    new OperandStackArgumentReader(this, signature, _ => throw new UnsupportedOperationException("CallTarget resolution not supported here"))

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
