package software.kes.scaletta.api

object Position {
  val first: Position = Position(line(1), column(1))

  def of(lineIndex: Int, columnIndex: Int): Position =
    Position(line(lineIndex), column(columnIndex))
}

case class Position(line: LineIndex,
                    column: ColumnIndex)
