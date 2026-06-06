package software.kes.scaletta.util.stack

import software.kes.scaletta.util.array.ArrayUtil

import scala.reflect.classTag

object ObjectStack {
  def create(initialCapacity: Int = 16): ObjectStack = {
    new ObjectStack(new Array[AnyRef](Math.max(initialCapacity, 1)))
  }
}

final class ObjectStack private(private var elements: Array[AnyRef]) extends PrimitiveStack {
  def push(value: AnyRef): Unit = {
    ensureCapacity(_size + 1)
    elements(_size) = value
    _size += 1
  }

  def peek(): Option[AnyRef] = {
    if (_size == 0) {
      None
    } else {
      Some(elements(_size - 1))
    }
  }

  def pop(): AnyRef = {
    checkBeforePop()
    _size -= 1
    val value = elements(_size)
    elements(_size) = null // Prevent memory leaks by clearing the reference
    value
  }

  def duplicate(): Unit = {
    checkBeforeDuplicate()
    push(elements(_size - 1))
  }

  /**
   * Reads the value at the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeRead(position: Int): AnyRef = {
    val idx = _size - 1 - position
    elements(idx)
  }

  /**
   * Writes a value to the specified position from the top of the stack.
   *
   * @param position 0 is top of the stack, 1 is second from top, etc.
   *                 position must be less than size, or the result is undefined.
   */
  def unsafeWrite(position: Int, value: AnyRef): Unit = {
    val idx = _size - 1 - position
    elements(idx) = value
  }

  override def clear(): Unit = {
    // For object stacks, we must null out all elements to allow GC
    var i = 0
    while (i < _size) {
      elements(i) = null
      i += 1
    }
    super.clear()
  }

  /**
   * Contracts the stack by the specified amount.
   *
   * The previously occupied slots will be replaced with nulls.
   */
  override def contract(amount: Int): Unit = {
    if (amount > 0) {
      val actualAmount = Math.min(amount, _size)
      var i = actualAmount
      while (i > 0) {
        _size -= 1
        elements(_size) = null
        i -= 1
      }
    }
  }

  protected def ensureCapacity(minCapacity: Int): Unit =
    // Using the generic growArray with an explicit ClassTag for AnyRef
    elements = ArrayUtil.growArray(elements, minCapacity, _size)(classTag[AnyRef])
}
