package software.kes.scaletta.api

import scala.collection.immutable.ArraySeq

/**
 * A reader for function arguments.
 * The lifetime of this object is limited to the duration of the function call,
 * so don't keep references to it that will escape that scope. The consequences of doing so
 * are undefined.
 *
 * Use [[toVector]] or [[toArray]] to copy the arguments into a safe container that can escape
 * the scope of the function call.
 *
 * [[read]] can be used to read any argument, but it may result in undesired boxing.
 * If you know for sure that a particular argument is a primitive, you can use the unsafeRead*
 * methods to avoid boxing.
 */
trait ArgumentReader {
  /**
   * Number of arguments available. Note that varargs (if present) are stored in an ArraySeq,
   * and this ArraySeq will count as a single argument (always the last one).
   *
   * If the context is a method call, then the first argument (index 0) is always the receiver
   * (aka "this").
   */
  def argCount: Int

  /**
   * Read the argument at index.
   */
  def read(index: Int): Any

  /**
   * Copies all arguments into a Vector that is safe to share.
   */
  def toVector: Vector[Any]

  /**
   * Copies all arguments into a new Array.
   */
  def toArray: Array[Any]

  /**
   * Read the argument at index as an unboxed Boolean.
   * Use only if you are sure the argument is a Boolean, otherwise the behavior is undefined.
   */
  def unsafeReadBoolean(index: Int): Boolean

  /**
   * Read the argument at index as an unboxed Byte.
   * Use only if you are sure the argument is an Byte, otherwise the behavior is undefined.
   */
  def unsafeReadByte(index: Int): Byte

  /**
   * Read the argument at index as an unboxed Char.
   * Use only if you are sure the argument is a Char, otherwise the behavior is undefined.
   */
  def unsafeReadChar(index: Int): Char

  /**
   * Read the argument at index as an unboxed Double.
   * Use only if you are sure the argument is a Double, otherwise the behavior is undefined.
   */
  def unsafeReadDouble(index: Int): Double

  /**
   * Read the argument at index as an unboxed Float.
   * Use only if you are sure the argument is a Float, otherwise the behavior is undefined.
   */
  def unsafeReadFloat(index: Int): Float

  /**
   * Read the argument at index as an unboxed Int.
   * Use only if you are sure the argument is an Int, otherwise the behavior is undefined.
   */
  def unsafeReadInt(index: Int): Int

  /**
   * Read the argument at index as an unboxed Long.
   * Use only if you are sure the argument is an Long, otherwise the behavior is undefined.
   */
  def unsafeReadLong(index: Int): Long

  /**
   * Read the argument at index as an unboxed Short.
   * Use only if you are sure the argument is a Short, otherwise the behavior is undefined.
   */
  def unsafeReadShort(index: Int): Short

  /**
   * Read the argument at index as an ArraySeq of unboxed Booleans.
   * Use only if you are sure the argument is an ArraySeq[Boolean], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadBooleanArray(index: Int): ArraySeq[Boolean]

  /**
   * Read the argument at index as an ArraySeq of unboxed Bytes.
   * Use only if you are sure the argument is an ArraySeq[Byte], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadByteArray(index: Int): ArraySeq[Byte]

  /**
   * Read the argument at index as an ArraySeq of unboxed Chars.
   * Use only if you are sure the argument is an ArraySeq[Char], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadCharArray(index: Int): ArraySeq[Char]

  /**
   * Read the argument at index as an ArraySeq of unboxed Doubles.
   * Use only if you are sure the argument is an ArraySeq[Double], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadDoubleArray(index: Int): ArraySeq[Double]

  /**
   * Read the argument at index as an ArraySeq of unboxed Floats.
   * Use only if you are sure the argument is an ArraySeq[Float], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadFloatArray(index: Int): ArraySeq[Float]

  /**
   * Read the argument at index as an ArraySeq of unboxed Ints.
   * Use only if you are sure the argument is an ArraySeq[Int], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadIntArray(index: Int): ArraySeq[Int]

  /**
   * Read the argument at index as an ArraySeq of unboxed Longs.
   * Use only if you are sure the argument is an ArraySeq[Long], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadLongArray(index: Int): ArraySeq[Long]

  /**
   * Read the argument at index as an ArraySeq of unboxed Shorts.
   * Use only if you are sure the argument is an ArraySeq[Short], otherwise a RuntimeException will be thrown.
   *
   * The returned ArraySeq is safe to share.
   */
  def unsafeReadShortArray(index: Int): ArraySeq[Short]

  /**
   * Read the argument at index as a thunk to the specified type.
   * Use only if you are sure the argument is a thunk, otherwise a RuntimeException will be thrown.
   */
  def unsafeReadThunk[A](index: Int): () => A =
    read(index).asInstanceOf[() => A]

}
