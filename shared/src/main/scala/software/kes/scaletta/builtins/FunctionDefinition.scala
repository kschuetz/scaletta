package software.kes.scaletta.builtins

import software.kes.scaletta.api.ArgumentReader
import software.kes.scaletta.types.{Type, TypeId}

case class FunctionDefinition(paramGroups: Vector[ParameterGroup],
                              returnType: Type[TypeId],
                              pure: Boolean,
                              impl: FunctionImpl)

case class FormalParameter(name: String,
                           typ: Type[TypeId],
                           default: Option[Any] = None)

case class ParameterGroup(params: Vector[FormalParameter])

sealed trait FunctionImpl

object FunctionImpl {
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
