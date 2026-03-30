package software.kes.scaletta.util

import software.kes.scaletta.util.array.ArrayUtil

object CharBuffer {
  def create(initialCapacity: Int = 256): CharBuffer =
    new CharBuffer(new Array[Char](initialCapacity))
}

final class CharBuffer private(private var buffer: Array[Char]) {
  private var ptr = 0

  def write(ch: Char): Unit = {
    if (ch == 0) return
    grow(ptr + 1)
    buffer(ptr) = ch
    ptr += 1
  }

  def write(s: Iterable[Char]): Unit =
    s.foreach(write)

  def mark(): Int = ptr

  def chop(): Char = {
    val result = buffer(ptr - 1)
    ptr -= 1
    result
  }

  def firstChar: Char =
    if (ptr > 0) buffer(0)
    else 0.toChar

  def lastChar: Char =
    if (ptr > 0) buffer(ptr - 1)
    else 0.toChar

  def reset(): Unit =
    ptr = 0

  def charAtIndex(idx: Int): Char =
    buffer(idx)

  def isEmpty: Boolean = ptr <= 0

  def nonEmpty: Boolean = ptr > 0

  def capacity: Int = buffer.length

  def size: Int = ptr

  def truncate(size: Int): Unit = {
    if (size <= 0) ptr = 0
    else ptr = ptr.min(size)
  }

  def slice(start: Int, end: Int): String = {
    val actualEnd = end min ptr
    new String(buffer, start, actualEnd - start)
  }

  def slice(end: Int): String =
    slice(0, end)

  def slice(): String =
    slice(ptr)

  def insert(idx: Int, ch: Char): Unit = {
    grow(ptr + 1)
    System.arraycopy(buffer, idx, buffer, idx + 1, ptr - idx)
    buffer(idx) = ch
    ptr += 1
  }

  private def grow(minCapacity: Int): Unit =
    buffer = ArrayUtil.growCharArray(buffer, minCapacity, ptr)
}
