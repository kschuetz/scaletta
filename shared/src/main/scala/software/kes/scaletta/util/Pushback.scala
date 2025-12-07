package software.kes.scaletta.util

import scala.reflect.ClassTag

object Pushback {
  def create[A: ClassTag](initialCapacity: Int = 256): Pushback[A] = {
    if (initialCapacity <= 0) new Pushback[A](null)
    else new Pushback(new Array[A](initialCapacity))
  }
}

final class Pushback[A: ClassTag] private(private var buffer: Array[A]) {
  private var ptr = 0

  def push(item: A): Unit = {
    grow(ptr + 1)
    buffer(ptr) = item
    ptr += 1
  }

  def pop(): A = {
    ptr -= 1
    buffer(ptr)
  }

  def peek(): Option[A] =
    if (ptr > 0) Some(buffer(ptr - 1))
    else None

  def reset(): Unit =
    ptr = 0

  def isEmpty: Boolean = ptr <= 0

  def nonEmpty: Boolean = ptr > 0

  private def grow(capacity: Int): Unit = {
    if (buffer == null) {
      val newCapacity = Math.max(capacity * 2, 64)
      buffer = new Array[A](newCapacity)
    } else if (capacity > buffer.length) {
      val newBuffer = new Array[A](capacity * 2)
      System.arraycopy(buffer, 0, newBuffer, 0, buffer.length)
      buffer = newBuffer
    }
  }
}
