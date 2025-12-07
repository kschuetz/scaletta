package software.kes.scaletta.spike

import software.kes.scaletta.util.SetTwoPlus

sealed trait Type

sealed trait PrimitiveType extends Type

sealed trait NumberType extends PrimitiveType

sealed trait IntegerType extends NumberType

sealed trait RealNumberType extends NumberType

sealed trait AbstractIterableType extends Type

sealed trait FunctionType extends Type {
  def returnType: Type
}

object Type {
  case object IntT extends IntegerType

  case object LongT extends IntegerType

  case object ShortT extends IntegerType

  case object ByteT extends IntegerType

  case object DoubleT extends RealNumberType

  case object FloatT extends RealNumberType

  case object BooleanT extends PrimitiveType

  case object StringT extends PrimitiveType

  case object CharT extends PrimitiveType

  case object NullT extends PrimitiveType

  case class IterableT(elemType: Type) extends AbstractIterableType

  case class VectorT(elemType: Type) extends AbstractIterableType

  case class SetT(elemType: Type) extends AbstractIterableType

  case class OptionT(elemType: Type) extends AbstractIterableType

  case class Union(components: SetTwoPlus[Type]) extends Type {
    def ++(rhs: Union): Union = copy(components ++ rhs.components)
  }

  case class Function1T(param1: Type,
                        returnType: Type) extends FunctionType

  case class Function2T(param1: Type,
                        param2: Type,
                        returnType: Type) extends FunctionType

}
