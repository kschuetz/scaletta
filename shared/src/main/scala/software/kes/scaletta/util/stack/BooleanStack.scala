package software.kes.scaletta.util.stack

import software.kes.scaletta.util.BitArray

object BooleanStack {
  def create(): BooleanStack = {
    new BooleanStack(BitArray.create(64))
  }
}

final class BooleanStack private(private val elements: BitArray) extends PrimitiveStack {
  def push(value: Boolean): Unit = {
    elements.update(_size, value)
    _size += 1
  }

  def peek(): Option[Boolean] = {
    if (_size == 0) {
      None
    } else {
      Some(elements.get(_size - 1))
    }
  }

  def pop(): Boolean = {
    checkBeforePop()
    val value = elements.get(_size - 1)
    _size -= 1
    value
  }

  /**
   * Gets the value at the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeGet(position: Int): Boolean = {
    val idx = _size - 1 - position
    elements.get(idx)
  }
}
