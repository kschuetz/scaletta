package software.kes.scaletta.builtins

import software.kes.scaletta.api.RuntimeContextId
import software.kes.scaletta.symbols.Name
import software.kes.scaletta.types.{Type, TypeId}

case class FunctionDefinition(paramGroups: Vector[ParameterGroup],
                              returnType: Type[TypeId],
                              pure: Boolean,
                              functionId: FunctionId,
                              requireRuntimeContexts: Set[RuntimeContextId] = Set.empty)

case class FormalParameter(name: Name,
                           typ: Type[TypeId],
                           default: Option[Any] = None)

object ParameterGroup {
  def single(params: FormalParameter*): Vector[ParameterGroup] = Vector(ParameterGroup(params.toVector))
}

case class ParameterGroup(params: Vector[FormalParameter])
