package software.kes.scaletta.util

object CharPushback {
  def create(initialCapacity: Int = 256): CharPushback = {
    if (initialCapacity <= 0) new CharPushback(null, null)
    else new CharPushback(new Array[Char](initialCapacity),
      new Array[Long]((initialCapacity + 63) / 64))
  }
}

final class CharPushback private(private var buffer: Array[Char],
                                 private var widthFlags: Array[Long]) {
  private var ptr = 0

  def push(ch: Char, isDoubleWidth: Boolean = false): Unit = {
    grow(ptr + 1)
    buffer(ptr) = ch
    if (isDoubleWidth) setFlag(ptr)
    else clearFlag(ptr)
    ptr += 1
  }

  def push(s: String): Unit = {
    grow(ptr + s.length)
    s.reverseIterator.foreach { ch =>
      buffer(ptr) = ch
      clearFlag(ptr)
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

  def peekWidth(): Int =
    if (ptr > 0 && getFlag(ptr - 1)) 2 else 1

  def reset(): Unit =
    ptr = 0

  def isEmpty: Boolean = ptr <= 0

  def nonEmpty: Boolean = ptr > 0

  private def grow(capacity: Int): Unit = {
    if (buffer == null) {
      val newCapacity = Math.max(capacity * 2, 64)
      buffer = new Array[Char](newCapacity)
      widthFlags = new Array[Long]((newCapacity + 63) / 64)
    } else if (capacity > buffer.length) {
      val newCapacity = capacity * 2
      val newBuffer = new Array[Char](newCapacity)
      System.arraycopy(buffer, 0, newBuffer, 0, buffer.length)
      buffer = newBuffer

      val newWidthFlags = new Array[Long]((newCapacity + 63) / 64)
      System.arraycopy(widthFlags, 0, newWidthFlags, 0, widthFlags.length)
      widthFlags = newWidthFlags
    }
  }

  private def setFlag(index: Int): Unit = {
    widthFlags(index / 64) |= (1L << (index % 64))
  }

  private def clearFlag(index: Int): Unit = {
    widthFlags(index / 64) &= ~(1L << (index % 64))
  }

  private def getFlag(index: Int): Boolean = {
    (widthFlags(index / 64) & (1L << (index % 64))) != 0
  }
}
