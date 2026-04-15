package software.kes.scaletta.api

trait EvalResult {
  def value[A](): A

  def booleanValue(): Boolean

  def intValue(): Int

  def longValue(): Long

  def shortValue(): Short

  def byteValue(): Byte

  def charValue(): Char

  def doubleValue(): Double

  def floatValue(): Float
}
