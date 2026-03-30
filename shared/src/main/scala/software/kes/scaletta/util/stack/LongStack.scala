package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

object LongStack {
  def create(initialCapacity: Int = 16): LongStack = {
    new LongStack(new Array[Long](Math.max(initialCapacity, 1)))
  }
}

final class LongStack private(private var elements: Array[Long]) extends PrimitiveStack {
  def push(value: Long): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[Long] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): Long = {
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
  def unsafeGet(position: Int): Long = {
    val idx = _size - 1 - position
    elements(idx)
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    elements = ArrayUtil.growLongArray(elements, minCapacity, _size)
  }
}
