package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

object CharStack {
  def create(initialCapacity: Int = 16): CharStack = {
    new CharStack(new Array[Char](Math.max(initialCapacity, 1)))
  }
}

final class CharStack private(private var elements: Array[Char]) extends PrimitiveStack {
  def push(value: Char): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[Char] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): Char = {
    checkBeforePop()
    val value = elements(_size - 1)
    _size -= 1
    value
  }

  /**
   * Reads the value at the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeRead(position: Int): Char = {
    val idx = _size - 1 - position
    elements(idx)
  }

  /**
   * Writes a value to the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeWrite(position: Int, value: Char): Unit = {
    val idx = _size - 1 - position
    elements(idx) = value
  }

  protected def ensureCapacity(minCapacity: Int): Unit = {
    elements = ArrayUtil.growCharArray(elements, minCapacity, _size)
  }
}
