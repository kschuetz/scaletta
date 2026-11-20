package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api.{ProperType, TypeId}
import software.kes.scaletta.internal.types.TypeUniverse

/**
 * Dedicated helper exposing pure overload resolution entry points for call sites.
 */
object OverloadResolver {
  /**
   * Resolves the most specific function candidate matching the supplied argument types.
   */
  def resolveFunctionCall(typeUniverse: TypeUniverse,
                          candidates: Vector[NativeFunctionDefinition],
                          argTypes: Vector[Vector[ProperType[TypeId]]]): Either[ResolutionError, NativeFunctionDefinition] =
    OverloadTable.resolveFunctionCall(typeUniverse, candidates, argTypes)

  /**
   * Resolves the most specific method candidate matching the receiver and argument types.
   */
  def resolveMethodCall(typeUniverse: TypeUniverse,
                        candidates: Vector[NativeFunctionDefinition],
                        receiverType: ProperType[TypeId],
                        argTypes: Vector[Vector[ProperType[TypeId]]]): Either[ResolutionError, NativeFunctionDefinition] =
    OverloadTable.resolveMethodCall(typeUniverse, candidates, receiverType, argTypes)
}
