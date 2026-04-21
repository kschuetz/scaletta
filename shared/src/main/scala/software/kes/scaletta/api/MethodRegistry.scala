package software.kes.scaletta.api

import software.kes.scaletta.builtins.{FunctionImpl, NativeFunctionId, ParameterGroup}
import software.kes.scaletta.symbols.Name
import software.kes.scaletta.types.{Type, TypeId}

trait MethodRegistry {
  def addPureMethod(receiverType: ReceiverType,
                    name: Name,
                    paramGroups: Vector[ParameterGroup],
                    returnType: Type[TypeId],
                    impl: FunctionImpl,
                    requireRuntimeContexts: Set[RuntimeContextId] = Set.empty): NativeFunctionId

  def addImpureMethod(receiverType: ReceiverType,
                      name: Name,
                      paramGroups: Vector[ParameterGroup],
                      returnType: Type[TypeId],
                      impl: FunctionImpl,
                      requireRuntimeContexts: Set[RuntimeContextId] = Set.empty): NativeFunctionId
}
