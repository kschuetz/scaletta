package software.kes.scaletta.util.array

import scala.reflect.ClassTag

object ArrayUtil {
  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growArray[A: ClassTag](elements: Array[A], minCapacity: Int, currentSize: Int): Array[A] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[A](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growBooleanArray(elements: Array[Boolean], minCapacity: Int, currentSize: Int): Array[Boolean] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Boolean](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growIntArray(elements: Array[Int], minCapacity: Int, currentSize: Int): Array[Int] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Int](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growLongArray(elements: Array[Long], minCapacity: Int, currentSize: Int): Array[Long] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Long](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growShortArray(elements: Array[Short], minCapacity: Int, currentSize: Int): Array[Short] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Short](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growByteArray(elements: Array[Byte], minCapacity: Int, currentSize: Int): Array[Byte] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Byte](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growCharArray(elements: Array[Char], minCapacity: Int, currentSize: Int): Array[Char] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Char](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growDoubleArray(elements: Array[Double], minCapacity: Int, currentSize: Int): Array[Double] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Double](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }

  /**
   * Resizes an array if the current capacity is less than the minimum required capacity.
   *
   * @param elements    the existing array
   * @param minCapacity the required minimum capacity
   * @param currentSize the number of elements currently in the array
   * @return a new array with sufficient capacity, or the original array if no resize was needed
   */
  def growFloatArray(elements: Array[Float], minCapacity: Int, currentSize: Int): Array[Float] = {
    if (minCapacity > elements.length) {
      val newCapacity = Math.max(minCapacity, elements.length * 2)
      val newElements = new Array[Float](newCapacity)
      System.arraycopy(elements, 0, newElements, 0, currentSize)
      newElements
    } else {
      elements
    }
  }
}
