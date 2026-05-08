package software.kes.scaletta.api

object column {
  def apply(value: Int): ColumnIndex = new ColumnIndex(value)
}

/**
 * 1-based
 */
final class ColumnIndex(val value: Int) extends AnyVal {
  def +(rhs: Int): ColumnIndex = new ColumnIndex(value + rhs)

  override def toString: String = value.toString
}
