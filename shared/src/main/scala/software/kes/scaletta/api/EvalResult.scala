package software.kes.scaletta.api

/**
 * Access to the result of an evaluation.
 *
 * Extract the value immediately using one of the value* methods.
 * Do not keep a reference to EvalResult after the evaluation is complete,
 * since the internals may be reused for future evaluations.
 */
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
