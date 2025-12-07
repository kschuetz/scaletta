package software.kes.scaletta.scanner

import scala.collection.immutable.TreeMap

object LineMap {
  def apply(basePosition: Position = Position.zero): LineMap =
    new LineMap(basePosition.line, basePosition.column, TreeMap(0 -> basePosition.line))
}

final class LineMap private(currentLine: LineIndex,
                            baseColumn: ColumnIndex,
                            indexToLineBegin: TreeMap[Int, LineIndex]) {

  def addLineBegin(index: CharIndex): LineMap = {
    val nextLine = currentLine.next
    new LineMap(nextLine, baseColumn, indexToLineBegin.updated(index.value, nextLine))
  }

  def indexToPosition(index: CharIndex): Position = {
    indexToLineBegin.maxBefore(index.value + 1) match {
      case Some((idx, line)) =>
        val colIndex = ColumnIndex(index.value - idx)
        Position(line, colIndex)
      case None => Position(LineIndex(0), ColumnIndex(index.value))
    }
  }
}
