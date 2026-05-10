package software.kes.scaletta.api

object line {
  def apply(value: Int): LineIndex = LineIndex(value)
}

object LineIndex {
  def apply(value: Int): LineIndex =
    if (value < 1) new LineIndex(1) else new LineIndex(value)
}

/**
 * 1-based
 */
final class LineIndex private(val value: Int) extends AnyVal {
  def next: LineIndex = new LineIndex(value + 1)

  override def toString: String = value.toString
}
