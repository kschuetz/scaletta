package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.VarSpaceSignature

object VarSpaceFromMultiStack {
  def create(stack: MultiStack,
             initialSignature: VarSpaceSignature = VarSpaceSignature.empty): VarSpaceFromMultiStack =
    new VarSpaceFromMultiStack(stack, initialSignature)
}

final class VarSpaceFromMultiStack private(stack: MultiStack,
                                           private var signature: VarSpaceSignature) extends VarSpace {
  def setSignature(signature: VarSpaceSignature): Unit = this.signature = signature

  def read(index: Int): Any =
    signature.basicTypeOf(index) match {
      case software.kes.scaletta.common.BasicTypes.Boolean => unsafeReadBoolean(index)
      case software.kes.scaletta.common.BasicTypes.Int => unsafeReadInt(index)
      case software.kes.scaletta.common.BasicTypes.Long => unsafeReadLong(index)
      case software.kes.scaletta.common.BasicTypes.Short => unsafeReadShort(index)
      case software.kes.scaletta.common.BasicTypes.Byte => unsafeReadByte(index)
      case software.kes.scaletta.common.BasicTypes.Char => unsafeReadChar(index)
      case software.kes.scaletta.common.BasicTypes.Double => unsafeReadDouble(index)
      case software.kes.scaletta.common.BasicTypes.Float => unsafeReadFloat(index)
      case _ => unsafeReadObject(index)
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
}
