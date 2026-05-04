package software.kes.scaletta.internal.interpreter

trait VarSpace {
  def read(index: Int): Any

  def unsafeReadObject(index: Int): AnyRef

  def unsafeReadBoolean(index: Int): Boolean

  def unsafeReadInt(index: Int): Int

  def unsafeReadLong(index: Int): Long

  def unsafeReadShort(index: Int): Short

  def unsafeReadByte(index: Int): Byte

  def unsafeReadChar(index: Int): Char

  def unsafeReadDouble(index: Int): Double

  def unsafeReadFloat(index: Int): Float

  def unsafeWriteObject(index: Int, value: AnyRef): Unit

  def unsafeWriteBoolean(index: Int, value: Boolean): Unit

  def unsafeWriteInt(index: Int, value: Int): Unit

  def unsafeWriteLong(index: Int, value: Long): Unit

  def unsafeWriteShort(index: Int, value: Short): Unit

  def unsafeWriteByte(index: Int, value: Byte): Unit

  def unsafeWriteChar(index: Int, value: Char): Unit

  def unsafeWriteDouble(index: Int, value: Double): Unit

  def unsafeWriteFloat(index: Int, value: Float): Unit
}
