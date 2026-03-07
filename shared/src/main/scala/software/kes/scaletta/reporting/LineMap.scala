package software.kes.scaletta.reporting

import scala.collection.immutable.TreeMap

object LineMap {
  def create(basePosition: Position = Position.first): LineMap =
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
        val colIndex = column(index.value - idx + 1)
        Position(line, colIndex)
      case None => Position(line(1), column(index.value + 1))
    }
  }

  def builder: LineMapBuilder = LineMapBuilder.create(this)
}
