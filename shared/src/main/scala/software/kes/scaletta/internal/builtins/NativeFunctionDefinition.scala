package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api.{RuntimeContextId, Type, TypeId}
import software.kes.scaletta.internal.symbols.Name

case class NativeFunctionDefinition(paramGroups: Vector[ParameterGroup],
                                    returnType: Type[TypeId],
                                    pure: Boolean,
                                    nativeFunctionId: NativeFunctionId,
                                    requireRuntimeContexts: Set[RuntimeContextId] = Set.empty)

case class FormalParameter(name: Name,
                           typ: Type[TypeId],
                           default: Option[Any] = None)

object ParameterGroup {
  def single(params: FormalParameter*): Vector[ParameterGroup] = Vector(ParameterGroup(params.toVector))
}

case class ParameterGroup(params: Vector[FormalParameter])
