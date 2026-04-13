package software.kes.scaletta.builtins

import software.kes.scaletta.api.{ArgumentReader, RuntimeContextReader}

sealed trait FunctionImpl

object FunctionImpl {
  /**
   * Returns an object (AnyRef) as a result. Does not require access to a runtime context.
   */
  def objectResult(body: ArgumentReader => AnyRef): FunctionImpl = ObjectResult(body)

  /**
   * Returns a Boolean as a result. Does not require access to a runtime context.
   */
  def booleanResult(body: ArgumentReader => Boolean): FunctionImpl = BooleanResult(body)

  /**
   * Returns an Int as a result. Does not require access to a runtime context.
   */
  def intResult(body: ArgumentReader => Int): FunctionImpl = IntResult(body)

  /**
   * Returns a Long as a result. Does not require access to a runtime context.
   */
  def longResult(body: ArgumentReader => Long): FunctionImpl = LongResult(body)

  /**
   * Returns a Short as a result. Does not require access to a runtime context.
   */
  def shortResult(body: ArgumentReader => Short): FunctionImpl = ShortResult(body)

  /**
   * Returns a Byte as a result. Does not require access to a runtime context.
   */
  def byteResult(body: ArgumentReader => Byte): FunctionImpl = ByteResult(body)

  /**
   * Returns a Char as a result. Does not require access to a runtime context.
   */
  def charResult(body: ArgumentReader => Char): FunctionImpl = CharResult(body)

  /**
   * Returns a Double as a result. Does not require access to a runtime context.
   */
  def doubleResult(body: ArgumentReader => Double): FunctionImpl = DoubleResult(body)

  /**
   * Returns a Float as a result. Does not require access to a runtime context.
   */
  def floatResult(body: ArgumentReader => Float): FunctionImpl = FloatResult(body)

  /**
   * Returns an object as a result. Requires access to a runtime context.
   */
  def objectResultWithContext(body: (RuntimeContextReader, ArgumentReader) => AnyRef): FunctionImpl =
    ObjectResultWithContext(body)

  /**
   * Returns a Boolean as a result. Requires access to a runtime context.
   */
  def booleanResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Boolean): FunctionImpl =
    BooleanResultWithContext(body)

  /**
   * Returns an Int as a result. Requires access to a runtime context.
   */
  def intResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Int): FunctionImpl =
    IntResultWithContext(body)

  /**
   * Returns a Long as a result. Requires access to a runtime context.
   */
  def longResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Long): FunctionImpl =
    LongResultWithContext(body)

  /**
   * Returns a Short as a result. Requires access to a runtime context.
   */
  def shortResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Short): FunctionImpl =
    ShortResultWithContext(body)

  /**
   * Returns a Byte as a result. Requires access to a runtime context.
   */
  def byteResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Byte): FunctionImpl =
    ByteResultWithContext(body)

  /**
   * Returns a Char as a result. Requires access to a runtime context.
   */
  def charResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Char): FunctionImpl =
    CharResultWithContext(body)

  /**
   * Returns a Double as a result. Requires access to a runtime context.
   */
  def doubleResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Double): FunctionImpl =
    DoubleResultWithContext(body)

  /**
   * Returns a Float as a result. Requires access to a runtime context.
   */
  def floatResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Float): FunctionImpl =
    FloatResultWithContext(body)

  case class ObjectResult(body: ArgumentReader => AnyRef) extends FunctionImpl

  case class BooleanResult(body: ArgumentReader => Boolean) extends FunctionImpl

  case class IntResult(body: ArgumentReader => Int) extends FunctionImpl

  case class LongResult(body: ArgumentReader => Long) extends FunctionImpl

  case class ShortResult(body: ArgumentReader => Short) extends FunctionImpl

  case class ByteResult(body: ArgumentReader => Byte) extends FunctionImpl

  case class CharResult(body: ArgumentReader => Char) extends FunctionImpl

  case class DoubleResult(body: ArgumentReader => Double) extends FunctionImpl

  case class FloatResult(body: ArgumentReader => Float) extends FunctionImpl

  case class ObjectResultWithContext(body: (RuntimeContextReader, ArgumentReader) => AnyRef) extends FunctionImpl

  case class BooleanResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Boolean) extends FunctionImpl

  case class IntResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Int) extends FunctionImpl

  case class LongResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Long) extends FunctionImpl

  case class ShortResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Short) extends FunctionImpl

  case class ByteResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Byte) extends FunctionImpl

  case class CharResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Char) extends FunctionImpl

  case class DoubleResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Double) extends FunctionImpl

  case class FloatResultWithContext(body: (RuntimeContextReader, ArgumentReader) => Float) extends FunctionImpl
}
