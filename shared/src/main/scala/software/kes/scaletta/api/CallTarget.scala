package software.kes.scaletta.api

trait CallTarget {
  def setArgument(index: Int, value: Any): Unit

  def setArgumentObject(index: Int, value: AnyRef): Unit

  def setArgumentInt(index: Int, value: Int): Unit

  def setArgumentLong(index: Int, value: Long): Unit

  def setArgumentFloat(index: Int, value: Float): Unit

  def setArgumentDouble(index: Int, value: Double): Unit

  def setArgumentBoolean(index: Int, value: Boolean): Unit

  def setArgumentChar(index: Int, value: Char): Unit

  def setArgumentShort(index: Int, value: Short): Unit

  def setArgumentByte(index: Int, value: Byte): Unit
}
