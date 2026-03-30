package software.kes.scaletta.util

import software.kes.scaletta.util.array.ArrayUtil

object BitArray {
  def create(initialBitCapacity: Int = 0): BitArray = {
    if (initialBitCapacity <= 0) new BitArray(new Array[Int](0))
    else new BitArray(new Array[Int]((initialBitCapacity + 31) / 32))
  }
}

final class BitArray private(private var elements: Array[Int]) {
  def get(index: Int): Boolean = {
    val arrayIndex = index >>> 5
    if (arrayIndex >= elements.length) false
    else {
      val bitIndex = index & 31
      (elements(arrayIndex) & (1 << bitIndex)) != 0
    }
  }

  def set(index: Int): Unit = {
    ensureCapacity(index + 1)
    val bitIndex = index & 31
    val arrayIndex = index >>> 5
    elements(arrayIndex) |= (1 << bitIndex)
  }

  def clear(index: Int): Unit = {
    val arrayIndex = index >>> 5
    if (arrayIndex < elements.length) {
      val bitIndex = index & 31
      elements(arrayIndex) &= ~(1 << bitIndex)
    }
  }

  def update(index: Int, value: Boolean): Unit = {
    if (value) set(index)
    else clear(index)
  }

  def ensureCapacity(bitCapacity: Int): Unit = {
    val requiredInts = (bitCapacity + 31) / 32
    elements = ArrayUtil.growIntArray(elements, requiredInts, elements.length)
  }

  def bitCapacity(): Int = elements.length * 32
}
