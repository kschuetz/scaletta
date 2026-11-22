package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.RuntimeContextId
import software.kes.scaletta.internal.ast.{Attributed, TypedPhase}
import software.kes.scaletta.internal.reporting.Pos

final case class TypeCheckingResult(attributed: Attributed[TypedPhase.Expression[Attributed]],
                                    errors: Vector[Pos[SemanticError]],
                                    requiredContexts: Set[RuntimeContextId]) {
  def hasErrors: Boolean = errors.nonEmpty
}
