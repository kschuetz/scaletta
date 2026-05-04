package software.kes.scaletta.internal.reporting

import scala.collection.immutable.TreeMap

object LineMap {
  def create(basePosition: Position = Position.first): LineMap =
    new LineMap(basePosition.line, basePosition.column, TreeMap(0 -> basePosition.line), CharIndex(0))
}

final class LineMap private(currentLine: LineIndex,
                            baseColumn: ColumnIndex,
                            indexToLineBegin: TreeMap[Int, LineIndex],
                            lastIndex: CharIndex) {

  def addLineBegin(index: CharIndex): LineMap =
    if (index.value <= lastIndex.value) {
      this
    } else {
      val nextLine = currentLine.next
      new LineMap(nextLine, baseColumn, indexToLineBegin.updated(index.value, nextLine), index)
    }

  def indexToPosition(index: CharIndex): Position = {
    indexToLineBegin.maxBefore(index.value + 1) match {
      case Some((idx, line)) =>
        val colOffset = if (idx == 0) baseColumn.value else 1
        val colIndex = column(index.value - idx + colOffset)
        Position(line, colIndex)
      case None => Position(line(1), column(index.value + 1))
    }
  }

  def builder: LineMapBuilder =
    LineMapBuilder.create(this)
}
