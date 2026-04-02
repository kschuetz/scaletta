package software.kes.scaletta.util.stack

protected abstract class PrimitiveStack(protected var _size: Int = 0) {
  def size(): Int = _size

  def isEmpty: Boolean = _size == 0

  def clear(): Unit = _size = 0

  /**
   * Expands the stack by the specified amount.
   * The items occupying the newly created space are undefined.
   */
  def expand(amount: Int): Unit = {
    if (amount > 0) {
      ensureCapacity(_size + amount)
      _size += amount
    }
  }

  /**
   * Contracts the stack by the specified amount.
   */
  def contract(amount: Int): Unit = {
    if (amount > 0) {
      _size = Math.max(0, _size - amount)
    }
  }

  protected def ensureCapacity(minCapacity: Int): Unit

  final protected def checkBeforePop(): Unit = {
    if (_size == 0) {
      throw new NoSuchElementException("pop() called on empty stack")
    }
  }
}
