package software.kes.scaletta.interpreter

trait ConstantInterner {
  def internObject(value: AnyRef): Int

  def internLong(value: Long): Int

  def internDouble(value: Double): Int

  def internFloat(value: Float): Int
}
