package software.kes.scaletta.internal.reporting

object LineMapBuilder {
  def create(initial: LineMap = LineMap.create()): LineMapBuilder =
    new LineMapBuilder(initial)
}

final class LineMapBuilder private(private var state: LineMap) {
  def addLineBegin(index: CharIndex): Unit = {
    state = state.addLineBegin(index)
  }

  def result: LineMap = state
}
