package software.kes.scaletta.internal.types

sealed trait TypeConversion

object TypeConversion {
  case object None extends TypeConversion

  case object Identity extends TypeConversion

  case object Widening extends TypeConversion
}
