package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api._

case class NativeFunctionDefinition(paramGroups: Vector[ParameterGroup],
                                    returnType: Type[TypeId],
                                    pure: Boolean,
                                    nativeFunctionId: NativeFunctionId,
                                    requireRuntimeContexts: Set[RuntimeContextId] = Set.empty)
