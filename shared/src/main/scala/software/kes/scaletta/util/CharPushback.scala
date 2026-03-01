package software.kes.scaletta.util

object CharPushback {
  def create(initialCapacity: Int = 256): CharPushback = {
    if (initialCapacity <= 0) new CharPushback(null, null)
    else new CharPushback(new Array[Char](initialCapacity),
      BitArray.create(initialCapacity))
  }
}

final class CharPushback private(private var buffer: Array[Char],
                                 private var widthFlags: BitArray) {
  private var ptr = 0

  def push(ch: Char, isDoubleWidth: Boolean = false): Unit = {
    grow(ptr + 1)
    buffer(ptr) = ch
    widthFlags.update(ptr, isDoubleWidth)
    ptr += 1
  }

  def push(s: String): Unit = {
    grow(ptr + s.length)
    s.reverseIterator.foreach { ch =>
      buffer(ptr) = ch
      widthFlags.clear(ptr)
      ptr += 1
    }
  }

  def pop(): Char = {
    ptr -= 1
    buffer(ptr)
  }

  /**
   * Returns the character at the current pointer, or 0.toChar if the buffer is empty.
   */
  def peek(): Char =
    if (ptr > 0) buffer(ptr - 1)
    else 0.toChar

  /**
   * Returns true if the character at the current pointer is double-width, and false otherwise.
   * If the buffer is empty, it returns false.
   */
  def peekDoubleWidth(): Boolean =
    ptr > 0 && widthFlags.get(ptr - 1)

  def reset(): Unit =
    ptr = 0

  def isEmpty: Boolean = ptr <= 0

  def nonEmpty: Boolean = ptr > 0

  private def grow(capacity: Int): Unit = {
    if (buffer == null) {
      val newCapacity = Math.max(capacity * 2, 64)
      buffer = new Array[Char](newCapacity)
      widthFlags = BitArray.create(newCapacity)
    } else if (capacity > buffer.length) {
      val newCapacity = capacity * 2
      val newBuffer = new Array[Char](newCapacity)
      System.arraycopy(buffer, 0, newBuffer, 0, buffer.length)
      buffer = newBuffer
      widthFlags.ensureCapacity(newCapacity)
    }
  }
}
