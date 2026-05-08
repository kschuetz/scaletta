package software.kes.scaletta.api

object line {
  def apply(value: Int): LineIndex = new LineIndex(value)
}

/**
 * 1-based
 */
final class LineIndex(val value: Int) extends AnyVal {
  def next: LineIndex = new LineIndex(value + 1)

  def prev: LineIndex = new LineIndex(value - 1)

  override def toString: String = value.toString
}
