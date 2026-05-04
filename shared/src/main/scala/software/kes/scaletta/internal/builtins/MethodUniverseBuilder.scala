package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api.ReceiverType.{Instance, Static}
import software.kes.scaletta.api.{MethodName, MethodRegistry, OverloadRegistry}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.ParamsSignature
import software.kes.scaletta.symbols.QualifiedName
import software.kes.scaletta.types.{Type, TypeId}
import software.kes.scaletta.util.SettingsStack

object MethodUniverseBuilder {
  def create(): MethodUniverseBuilder =
    new MethodUniverseBuilder(FunctionSymbolTable.builder(),
      NativeFunctionTable.builder(), SettingsStack.create(MethodRegistry.Settings()))
}

final class MethodUniverseBuilder private(private val functionSymbolTableBuilder: FunctionSymbolTable.Builder,
                                          private val nativeFunctionTableBuilder: NativeFunctionTable.Builder,
                                          private val settingsStack: SettingsStack[MethodRegistry.Settings]) extends MethodRegistry {

  def addMethod(methodName: MethodName,
                parameters: Vector[FormalParameter],
                returnType: Type[TypeId],
                impl: FunctionImpl): NativeFunctionId = {
    addMultiParamGroupMethod(methodName, ParameterGroup.single(parameters: _*), returnType, impl)
  }

  def addMultiParamGroupMethod(methodName: MethodName,
                               paramGroups: Vector[ParameterGroup],
                               returnType: Type[TypeId],
                               impl: FunctionImpl): NativeFunctionId = {
    val currentSettings = settings
    val flattenedParams = paramGroups.flatMap(_.params).map(_.typ)
    val nativeFunctionId = nativeFunctionTableBuilder.add(NativeFunction(ParamsSignature.fromSeq(flattenedParams), BasicTypes.fromType(returnType).toInt, impl))

    val definition = NativeFunctionDefinition(
      paramGroups = paramGroups,
      returnType = returnType,
      pure = currentSettings.pureHint,
      nativeFunctionId = nativeFunctionId,
      requireRuntimeContexts = currentSettings.requireRuntimeContexts
    )

    methodName.receiverType match {
      case Static(path) =>
        functionSymbolTableBuilder.addStatic(QualifiedName.full(path, methodName.name), definition)
      case Instance(typ) =>
        functionSymbolTableBuilder.addInstance(typ, methodName.name, definition)
    }

    nativeFunctionId
  }

  def overloadRegistryFor(methodName: MethodName): OverloadRegistry = new OverloadRegistry {
    def addOverload(parameters: Vector[FormalParameter],
                    returnType: Type[TypeId],
                    impl: FunctionImpl): NativeFunctionId =
      addMethod(methodName, parameters, returnType, impl)

    def addMultiParamGroupOverload(paramGroups: Vector[ParameterGroup],
                                   returnType: Type[TypeId],
                                   impl: FunctionImpl): NativeFunctionId =
      addMultiParamGroupMethod(methodName, paramGroups, returnType, impl)
  }

  def settings: MethodRegistry.Settings = settingsStack.current

  def modifySettings(fn: MethodRegistry.Settings => MethodRegistry.Settings): Unit =
    settingsStack.modify(fn)

  def pushSettings(fn: MethodRegistry.Settings => MethodRegistry.Settings): Unit =
    settingsStack.push(fn)

  def popSettings(): Unit =
    settingsStack.pop()

  def build(): MethodUniverse = {
    new MethodUniverse(functionSymbolTableBuilder.result(), nativeFunctionTableBuilder.result())
  }
}


