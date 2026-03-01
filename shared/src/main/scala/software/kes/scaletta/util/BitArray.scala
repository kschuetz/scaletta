package software.kes.scaletta.util

object BitArray {
  def create(initialBitCapacity: Int = 0): BitArray = {
    if (initialBitCapacity <= 0) new BitArray(new Array[Long](0))
    else new BitArray(new Array[Long]((initialBitCapacity + 63) / 64))
  }
}

final class BitArray private(private var elements: Array[Long]) {
  def get(index: Int): Boolean = {
    val arrayIndex = index >>> 6
    if (arrayIndex >= elements.length) false
    else {
      val bitIndex = index & 63
      (elements(arrayIndex) & (1L << bitIndex)) != 0
    }
  }

  def set(index: Int): Unit = {
    ensureCapacity(index + 1)
    val bitIndex = index & 63
    val arrayIndex = index >>> 6
    elements(arrayIndex) |= (1L << bitIndex)
  }

  def clear(index: Int): Unit = {
    val arrayIndex = index >>> 6
    if (arrayIndex < elements.length) {
      val bitIndex = index & 63
      elements(arrayIndex) &= ~(1L << bitIndex)
    }
  }

  def update(index: Int, value: Boolean): Unit = {
    if (value) set(index)
    else clear(index)
  }

  def ensureCapacity(bitCapacity: Int): Unit = {
    val requiredLongs = (bitCapacity + 63) / 64
    if (requiredLongs > elements.length) {
      val newCapacity = Math.max(requiredLongs, elements.length * 2)
      val newElements = new Array[Long](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, elements.length)
      elements = newElements
    }
  }

  def bitCapacity(): Int = elements.length * 64
}
