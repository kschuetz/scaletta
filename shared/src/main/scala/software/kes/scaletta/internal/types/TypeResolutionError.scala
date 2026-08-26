package software.kes.scaletta.internal.types

sealed trait TypeResolutionError

object TypeResolutionError {
  case class UnknownType(name: String) extends TypeResolutionError
}
