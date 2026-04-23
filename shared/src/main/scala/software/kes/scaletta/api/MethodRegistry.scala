package software.kes.scaletta.api

import software.kes.scaletta.api.MethodRegistry.Settings
import software.kes.scaletta.builtins.{FormalParameter, FunctionImpl, NativeFunctionId, ParameterGroup}
import software.kes.scaletta.types.{Type, TypeId}

object MethodRegistry {
  case class Settings(pureHint: Boolean = false,
                      requireRuntimeContexts: Set[RuntimeContextId] = Set.empty) {
    def withPureHint(value: Boolean): Settings =
      if (value == pureHint) this else copy(pureHint = value)

    def requireContexts(contexts: RuntimeContextId*): Settings =
      copy(requireRuntimeContexts = requireRuntimeContexts ++ contexts)
  }
}

trait MethodRegistry {
  /**
   * Adds a method with a single parameter group.
   * Use [[addMultiParamGroupMethod]] if you need multiple parameter groups.
   */
  def addMethod(methodName: MethodName,
                parameters: Vector[FormalParameter],
                returnType: Type[TypeId],
                impl: FunctionImpl): NativeFunctionId

  def addMultiParamGroupMethod(methodName: MethodName,
                               paramGroups: Vector[ParameterGroup],
                               returnType: Type[TypeId],
                               impl: FunctionImpl): NativeFunctionId

  /**
   * Use this as a shortcut to register multiple overloads for a method.
   * This is equivalent to calling addMethod() multiple times.
   */
  def overloadRegistryFor(methodName: MethodName): OverloadRegistry

  def settings: Settings

  /**
   * Modifies the settings in place. Does not affect the settings stack.
   */
  def modifySettings(fn: Settings => Settings): Unit

  /**
   * Pushes the current settings onto the stack, then modifies the active settings.
   * Should eventually be matched with a call to popSettings().
   */
  def pushSettings(fn: Settings => Settings): Unit

  /**
   * Pops the topmost settings from the stack, restoring the previous settings.
   * Should be matched with a call to pushSettings().
   */
  def popSettings(): Unit
}

trait OverloadRegistry {
  /**
   * Adds an overload for a method.
   *
   * Use [[addMultiParamGroupOverload]] if you need multiple parameter groups.
   */
  def addOverload(parameters: Vector[FormalParameter],
                  returnType: Type[TypeId],
                  impl: FunctionImpl): NativeFunctionId

  def addMultiParamGroupOverload(paramGroups: Vector[ParameterGroup],
                                 returnType: Type[TypeId],
                                 impl: FunctionImpl): NativeFunctionId
}
