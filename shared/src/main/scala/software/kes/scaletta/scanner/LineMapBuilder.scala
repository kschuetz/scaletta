package software.kes.scaletta.scanner

object LineMapBuilder {
  def create(initial: LineMap = LineMap.create()): LineMapBuilder =
    new LineMapBuilder(initial)
}

final class LineMapBuilder(private var state: LineMap) {
  def addLineBegin(index: CharIndex): Unit =
    state = state.addLineBegin(index)

  def result: LineMap = state
}
