package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

object IntStack {
  def create(initialCapacity: Int = 16): IntStack = {
    new IntStack(new Array[Int](Math.max(initialCapacity, 1)))
  }
}

final class IntStack private(private var elements: Array[Int]) extends PrimitiveStack {
  def push(value: Int): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[Int] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): Int = {
    checkBeforePop()
    val value = elements(_size - 1)
    _size -= 1
    value
  }

  /**
   * Gets the value at the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeGet(position: Int): Int = {
    val idx = _size - 1 - position
    elements(idx)
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    elements = ArrayUtil.growIntArray(elements, minCapacity, _size)
  }
}
