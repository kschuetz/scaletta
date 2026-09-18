package software.kes.scaletta.internal.types

sealed trait TypeResolutionError

object TypeResolutionError {
  case class UnknownType(name: String) extends TypeResolutionError

  case class AmbiguousType(name: String, candidateCount: Int) extends TypeResolutionError

  case class NotAProperType(name: String) extends TypeResolutionError

  case class NotApplicable(name: String) extends TypeResolutionError

  case class WrongArity(name: String, expected: Int, actual: Int) extends TypeResolutionError
}
