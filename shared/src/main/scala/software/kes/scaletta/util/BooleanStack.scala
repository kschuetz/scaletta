package software.kes.scaletta.util

object BooleanStack {
  def create(): BooleanStack = {
    new BooleanStack(new Array[Long](1), 0)
  }
}

final class BooleanStack private(private var elements: Array[Long],
                                 private var _size: Int) {
  def size(): Int = _size

  def isEmpty: Boolean = _size == 0

  def push(value: Boolean): Unit = {
    ensureCapacity()
    val bitIndex = _size & 63
    val arrayIndex = _size >>> 6
    if (value) {
      elements(arrayIndex) |= (1L << bitIndex)
    } else {
      elements(arrayIndex) &= ~(1L << bitIndex)
    }
    _size += 1
  }

  def peek(): Option[Boolean] = {
    if (_size == 0) {
      None
    } else {
      val index = _size - 1
      val bitIndex = index & 63
      val arrayIndex = index >>> 6
      val value = (elements(arrayIndex) & (1L << bitIndex)) != 0
      Some(value)
    }
  }

  def pop(): Boolean = {
    if (_size == 0) {
      throw new NoSuchElementException("pop() called on empty stack")
    } else {
      val index = _size - 1
      val bitIndex = index & 63
      val arrayIndex = index >>> 6
      val value = (elements(arrayIndex) & (1L << bitIndex)) != 0
      _size -= 1
      value
    }
  }

  private def ensureCapacity(): Unit = {
    if (_size == elements.length * 64) {
      val newElements = new Array[Long](elements.length * 2)
      System.arraycopy(elements, 0, newElements, 0, elements.length)
      elements = newElements
    }
  }
}
