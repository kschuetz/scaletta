package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

object ByteStack {
  def create(initialCapacity: Int = 16): ByteStack = {
    new ByteStack(new Array[Byte](Math.max(initialCapacity, 1)))
  }
}

final class ByteStack private(private var elements: Array[Byte]) extends PrimitiveStack {
  def push(value: Byte): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[Byte] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): Byte = {
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
  def unsafeRead(position: Int): Byte = {
    val idx = _size - 1 - position
    elements(idx)
  }

  /**
   * Writes a value to the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeWrite(position: Int, value: Byte): Unit = {
    val idx = _size - 1 - position
    elements(idx) = value
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    elements = ArrayUtil.growByteArray(elements, minCapacity, _size)
  }
}
