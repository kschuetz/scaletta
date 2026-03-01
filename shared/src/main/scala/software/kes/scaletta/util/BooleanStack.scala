package software.kes.scaletta.util

object BooleanStack {
  def create(): BooleanStack = {
    new BooleanStack(BitArray.create(64), 0)
  }
}

final class BooleanStack private(private val elements: BitArray,
                                 private var _size: Int) {
  def size(): Int = _size

  def isEmpty: Boolean = _size == 0

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
    if (_size == 0) {
      throw new NoSuchElementException("pop() called on empty stack")
    } else {
      val value = elements.get(_size - 1)
      _size -= 1
      value
    }
  }
}
