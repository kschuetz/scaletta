package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.{ProperType, TypeId}

sealed trait SemanticError

object SemanticError {
  // Scoping and declaration errors
  final case class ForwardReference(name: String) extends SemanticError

  final case class RecursiveReference(name: String) extends SemanticError

  final case class UnknownIdentifier(name: String) extends SemanticError

  final case class DuplicateDefinition(name: String) extends SemanticError

  // Type compatibility errors
  final case class TypeMismatch(
                                 expected: ProperType[TypeId],
                                 actual: ProperType[TypeId]
                               ) extends SemanticError

  final case class ConditionNotBoolean(
                                        actual: ProperType[TypeId]
                                      ) extends SemanticError

  // Method, function, and field resolution errors
  final case class NoSuchMethod(
                                 receiverType: ProperType[TypeId],
                                 name: String,
                                 argTypes: Vector[ProperType[TypeId]]
                               ) extends SemanticError

  final case class NoSuchFunction(
                                   name: String,
                                   argTypes: Vector[ProperType[TypeId]]
                                 ) extends SemanticError

  final case class AmbiguousCall(
                                  name: String,
                                  candidateCount: Int
                                ) extends SemanticError

  final case class NoSuchField(
                                receiverType: ProperType[TypeId],
                                name: String
                              ) extends SemanticError

  final case class NotCallable(
                                targetType: ProperType[TypeId]
                              ) extends SemanticError

  // Pattern and declaration constraints
  final case class RefutableValPattern(
                                        description: String
                                      ) extends SemanticError

  final case class InexhaustiveMatch(
                                      description: String
                                    ) extends SemanticError

  final case class UnreachableCase() extends SemanticError

  // Runtime context errors
  final case class UnsatisfiedContextRequirement(
                                                  contextType: TypeId
                                                ) extends SemanticError
}
