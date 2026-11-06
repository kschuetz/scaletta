package software.kes.scaletta.internal.semantics

sealed trait SemanticError

object SemanticError {
  final case class ForwardReference(name: String) extends SemanticError

  final case class UnknownIdentifier(name: String) extends SemanticError

  final case class DuplicateDefinition(name: String) extends SemanticError
}
