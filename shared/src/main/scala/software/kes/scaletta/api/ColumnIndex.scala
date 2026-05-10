package software.kes.scaletta.api

object column {
  def apply(value: Int): ColumnIndex = ColumnIndex(value)
}

object ColumnIndex {
  def apply(value: Int): ColumnIndex =
    if (value < 1) new ColumnIndex(1) else new ColumnIndex(value)
}

/**
 * 1-based
 */
final class ColumnIndex private(val value: Int) extends AnyVal {
  def +(rhs: Int): ColumnIndex = ColumnIndex(value + rhs)

  override def toString: String = value.toString
}
