package software.kes.scaletta.internal.types

sealed trait TypeResolutionError

object TypeResolutionError {
  final case class UnknownType(name: String) extends TypeResolutionError

  final case class AmbiguousType(name: String, candidateCount: Int) extends TypeResolutionError

  final case class NotAProperType(name: String) extends TypeResolutionError

  final case class NotApplicable(name: String) extends TypeResolutionError

  final case class WrongArity(name: String, expected: Int, actual: Int) extends TypeResolutionError

  final case class UnknownTerm(name: String) extends TypeResolutionError

  final case class ForwardReference(name: String) extends TypeResolutionError

  final case class RecursiveReference(name: String) extends TypeResolutionError

  final case class InvalidSingletonPath(name: String) extends TypeResolutionError
}
