package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

object FloatStack {
  def create(initialCapacity: Int = 16): FloatStack = {
    new FloatStack(new Array[Float](Math.max(initialCapacity, 1)))
  }
}

final class FloatStack private(private var elements: Array[Float]) extends PrimitiveStack {
  def push(value: Float): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[Float] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): Float = {
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
  def unsafeRead(position: Int): Float = {
    val idx = _size - 1 - position
    elements(idx)
  }

  /**
   * Writes a value to the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeWrite(position: Int, value: Float): Unit = {
    val idx = _size - 1 - position
    elements(idx) = value
  }

  protected def ensureCapacity(minCapacity: Int): Unit =
    elements = ArrayUtil.growFloatArray(elements, minCapacity, _size)
}
