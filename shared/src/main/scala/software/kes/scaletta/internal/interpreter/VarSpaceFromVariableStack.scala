package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.VarSpaceSignature

object VarSpaceFromVariableStack {
  def create(stack: VariableStack,
             initialSignature: VarSpaceSignature = VarSpaceSignature.empty): VarSpaceFromVariableStack =
    new VarSpaceFromVariableStack(stack, initialSignature)
}

final class VarSpaceFromVariableStack private(stack: VariableStack,
                                              private var signature: VarSpaceSignature) extends VarSpace {
  def setSignature(signature: VarSpaceSignature): Unit = this.signature = signature

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

  def pushIntoOperandStack(index: Int,
                           operandStack: OperandStack): Unit = {
    signature.basicTypeOf(index) match {
      case BasicTypes.Boolean => operandStack.pushBoolean(unsafeReadBoolean(index))
      case BasicTypes.Int => operandStack.pushInt(unsafeReadInt(index))
      case BasicTypes.Long => operandStack.pushLong(unsafeReadLong(index))
      case BasicTypes.Short => operandStack.pushShort(unsafeReadShort(index))
      case BasicTypes.Byte => operandStack.pushByte(unsafeReadByte(index))
      case BasicTypes.Char => operandStack.pushChar(unsafeReadChar(index))
      case BasicTypes.Double => operandStack.pushDouble(unsafeReadDouble(index))
      case BasicTypes.Float => operandStack.pushFloat(unsafeReadFloat(index))
      case _ => operandStack.pushObject(unsafeReadObject(index))
    }
  }

  def unsafeReadObject(index: Int): AnyRef =
    stack.objects.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadBoolean(index: Int): Boolean =
    stack.booleans.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadInt(index: Int): Int =
    stack.ints.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadLong(index: Int): Long =
    stack.longs.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadShort(index: Int): Short =
    stack.shorts.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadByte(index: Int): Byte =
    stack.bytes.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadChar(index: Int): Char =
    stack.chars.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadDouble(index: Int): Double =
    stack.doubles.unsafeRead(signature.stackOffsetOf(index))

  def unsafeReadFloat(index: Int): Float =
    stack.floats.unsafeRead(signature.stackOffsetOf(index))

  def unsafeWriteObject(index: Int, value: AnyRef): Unit =
    stack.objects.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteBoolean(index: Int, value: Boolean): Unit =
    stack.booleans.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteInt(index: Int, value: Int): Unit =
    stack.ints.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteLong(index: Int, value: Long): Unit =
    stack.longs.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteShort(index: Int, value: Short): Unit =
    stack.shorts.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteByte(index: Int, value: Byte): Unit =
    stack.bytes.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteChar(index: Int, value: Char): Unit =
    stack.chars.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteDouble(index: Int, value: Double): Unit =
    stack.doubles.unsafeWrite(signature.stackOffsetOf(index), value)

  def unsafeWriteFloat(index: Int, value: Float): Unit =
    stack.floats.unsafeWrite(signature.stackOffsetOf(index), value)

  def slotCount: Int = signature.slotCount

  def readAll(): Array[Any] = {
    val count = slotCount
    val result = new Array[Any](count)
    var i = 0
    while (i < count) {
      result(i) = read(i)
      i += 1
    }
    result
  }
}
