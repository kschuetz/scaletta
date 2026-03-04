package software.kes.scaletta.reporting

object Position {
  val zero: Position = Position(LineIndex(0), ColumnIndex(0))
}

case class Position(line: LineIndex,
                    column: ColumnIndex)

object LineIndex {
  def apply(value: Int): LineIndex = new LineIndex(value)
}

/**
 * 0-based
 */
final class LineIndex(val value: Int) extends AnyVal {
  def next: LineIndex = new LineIndex(value + 1)

  def prev: LineIndex = new LineIndex(value - 1)
}

object ColumnIndex {
  def apply(value: Int): ColumnIndex = new ColumnIndex(value)
}

/**
 * 0-based
 */
final class ColumnIndex(val value: Int) extends AnyVal {
  def +(rhs: Int): ColumnIndex = new ColumnIndex(value + rhs)
}

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

  override def toString: String = value.toString
}

object Pos {
  def apply[A](value: A, index: CharIndex): Pos[A] =
    Pos(value, index, index)
}

case class Pos[A](value: A,
                  begin: CharIndex,
                  end: CharIndex) {
  def map[B](fn: A => B): Pos[B] =
    Pos(fn(value), begin, end)

  def withNewValue[B](value: B): Pos[B] =
    Pos(value, begin, end)

  def positionTuple: (Int, Int) = (begin.value, end.value)
}
