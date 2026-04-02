package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

object DoubleStack {
  def create(initialCapacity: Int = 16): DoubleStack = {
    new DoubleStack(new Array[Double](Math.max(initialCapacity, 1)))
  }
}

final class DoubleStack private(private var elements: Array[Double]) extends PrimitiveStack {
  def push(value: Double): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[Double] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): Double = {
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
  def unsafeRead(position: Int): Double = {
    val idx = _size - 1 - position
    elements(idx)
  }

  /**
   * Writes a value to the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeWrite(position: Int, value: Double): Unit = {
    val idx = _size - 1 - position
    elements(idx) = value
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    elements = ArrayUtil.growDoubleArray(elements, minCapacity, _size)
  }
}
