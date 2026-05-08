package software.kes.scaletta.internal.reporting

object CharIndex {
  def apply(value: Int): CharIndex = new CharIndex(value)
}

/**
 * 0-based
 */
final class CharIndex(val value: Int) extends AnyVal {
  def +(rhs: Int): CharIndex = new CharIndex(value + rhs)

  def -(rhs: Int): CharIndex = new CharIndex(value - rhs)

  def <(rhs: CharIndex): Boolean = value < rhs.value

  def >(rhs: CharIndex): Boolean = value > rhs.value

  def <=(rhs: CharIndex): Boolean = value <= rhs.value

  def >=(rhs: CharIndex): Boolean = value >= rhs.value

  override def toString: String = value.toString
}
