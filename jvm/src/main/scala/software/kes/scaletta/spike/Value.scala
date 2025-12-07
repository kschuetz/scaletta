package software.kes.scaletta.spike

/**
 * Probably won't need this
 */

sealed trait Value {
  def typ: Type
}

object Value {
  case class IntValue(value: Int) extends Value {
    def typ: Type = Type.IntT
  }

  case class LongValue(value: Long) extends Value {
    def typ: Type = Type.LongT
  }

  case class ShortValue(value: Short) extends Value {
    def typ: Type = Type.ShortT
  }

  case class ByteValue(value: Byte) extends Value {
    def typ: Type = Type.ByteT
  }

  case class DoubleValue(value: Double) extends Value {
    def typ: Type = Type.DoubleT
  }

  case class FloatValue(value: Float) extends Value {
    def typ: Type = Type.FloatT
  }

  sealed trait BooleanValue {
    def typ: Type = Type.BooleanT
  }

  object True extends BooleanValue

  object False extends BooleanValue

  case class StringValue(value: String) extends Value {
    def typ: Type = Type.StringT
  }

  case class CharValue(value: Char) extends Value {
    def typ: Type = Type.CharT
  }

  case object NullValue extends Value {
    def typ: Type = Type.NullT
  }

  case class OptionValue(value: Option[Any],
                         elementType: Type) {
    def typ: Type = Type.OptionT(elementType)
  }

  case class IterableValue(elements: Iterable[Any],
                           elementType: Type) {
    def typ: Type = Type.IterableT(elementType)
  }

  case class VectorValue(elements: Vector[Any],
                         elementType: Type) {
    def typ: Type = Type.VectorT(elementType)
  }

  case class SetValue(elements: Set[Any],
                      elementType: Type) {
    def typ: Type = Type.SetT(elementType)
  }
}
