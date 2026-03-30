package software.kes.scaletta.util.stack

protected abstract class PrimitiveStack(protected var _size: Int = 0) {
  def size(): Int = _size

  def isEmpty: Boolean = _size == 0

  def clear(): Unit = _size = 0

  final protected def checkBeforePop(): Unit = {
    if (_size == 0) {
      throw new NoSuchElementException("pop() called on empty stack")
    }
  }
}
