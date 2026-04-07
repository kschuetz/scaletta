package software.kes.scaletta.builtins

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.symbols.Name
import software.kes.scaletta.types.{Type, TypeId}

case class FunctionDefinition(paramGroups: Vector[ParameterGroup],
                              returnType: Type[TypeId],
                              pure: Boolean,
                              impl: FunctionImpl)

case class FormalParameter(name: Name,
                           typ: Type[TypeId],
                           default: Option[Any] = None)

object ParameterGroup {
  def single(params: FormalParameter*): Vector[ParameterGroup] = Vector(ParameterGroup(params.toVector))
}

case class ParameterGroup(params: Vector[FormalParameter])

sealed trait FunctionImpl

object FunctionImpl {
  def objectResult(body: ArgumentReader => AnyRef): FunctionImpl = ObjectResult(body)

  def booleanResult(body: ArgumentReader => Boolean): FunctionImpl = BooleanResult(body)

  def intResult(body: ArgumentReader => Int): FunctionImpl = IntResult(body)

  def longResult(body: ArgumentReader => Long): FunctionImpl = LongResult(body)

  def shortResult(body: ArgumentReader => Short): FunctionImpl = ShortResult(body)

  def byteResult(body: ArgumentReader => Byte): FunctionImpl = ByteResult(body)

  def charResult(body: ArgumentReader => Char): FunctionImpl = CharResult(body)

  def doubleResult(body: ArgumentReader => Double): FunctionImpl = DoubleResult(body)

  def floatResult(body: ArgumentReader => Float): FunctionImpl = FloatResult(body)

  case class ObjectResult(body: ArgumentReader => AnyRef) extends FunctionImpl

  case class BooleanResult(body: ArgumentReader => Boolean) extends FunctionImpl

  case class IntResult(body: ArgumentReader => Int) extends FunctionImpl

  case class LongResult(body: ArgumentReader => Long) extends FunctionImpl

  case class ShortResult(body: ArgumentReader => Short) extends FunctionImpl

  case class ByteResult(body: ArgumentReader => Byte) extends FunctionImpl

  case class CharResult(body: ArgumentReader => Char) extends FunctionImpl

  case class DoubleResult(body: ArgumentReader => Double) extends FunctionImpl

  case class FloatResult(body: ArgumentReader => Float) extends FunctionImpl
}
