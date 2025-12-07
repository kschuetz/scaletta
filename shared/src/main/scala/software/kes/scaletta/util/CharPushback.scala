package software.kes.scaletta.util

object CharPushback {
  def create(initialCapacity: Int = 256): CharPushback = {
    if (initialCapacity <= 0) new CharPushback(null)
    else new CharPushback(new Array[Char](initialCapacity))
  }
}

final class CharPushback private(private var buffer: Array[Char]) {
  private var ptr = 0

  def push(ch: Char): Unit = {
    grow(ptr + 1)
    buffer(ptr) = ch
    ptr += 1
  }

  def push(s: String): Unit = {
    grow(ptr + s.length)
    s.reverseIterator.foreach { ch =>
      buffer(ptr) = ch
      ptr += 1
    }
  }

  def pop(): Char = {
    ptr -= 1
    buffer(ptr)
  }

  def peek(): Char =
    if (ptr > 0) buffer(ptr - 1)
    else 0.toChar

  def reset(): Unit =
    ptr = 0

  def isEmpty: Boolean = ptr <= 0

  def nonEmpty: Boolean = ptr > 0

  private def grow(capacity: Int): Unit = {
    if (buffer == null) {
      val newCapacity = Math.max(capacity * 2, 64)
      buffer = new Array[Char](newCapacity)
    } else if (capacity > buffer.length) {
      val newBuffer = new Array[Char](capacity * 2)
      System.arraycopy(buffer, 0, newBuffer, 0, buffer.length)
      buffer = newBuffer
    }
  }
}
