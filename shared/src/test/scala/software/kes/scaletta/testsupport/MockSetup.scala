package software.kes.scaletta.testsupport

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.{StandardTypes, StandardTypesImpl}
import software.kes.scaletta.internal.types.TypeRegistryBootstrap
import software.kes.scaletta.util.{NonEmptyVector, SettingsStack}

//noinspection AccessorLikeMethodIsEmptyParen
object MockSetup {
  def create(): Setup = new Setup {
    private var nextNativeFunctionId = 1
    private var nextTypeId = 1
    private var nextRuntimeContextId = 1

    private def getNextNativeFunctionId(): NativeFunctionId = {
      val id = nextNativeFunctionId
      nextNativeFunctionId += 1
      NativeFunctionId(id)
    }

    private def getNextTypeId(): TypeId = {
      val id = nextTypeId
      nextTypeId += 1
      TypeId(id)
    }

    private def getNextRuntimeContextId(): RuntimeContextId = {
      val id = nextRuntimeContextId
      nextRuntimeContextId += 1
      RuntimeContextId(id)
    }

    val methodRegistry: MethodRegistry = new MethodRegistry {
      private val settingsStack = SettingsStack.create(MethodRegistry.Settings())

      def addMethod(methodName: MethodName, parameters: Vector[FormalParameter], returnType: Type[TypeId], impl: FunctionImpl): NativeFunctionId =
        getNextNativeFunctionId()

      def addMultiParamGroupMethod(methodName: MethodName, paramGroups: Vector[ParameterGroup], returnType: Type[TypeId], impl: FunctionImpl): NativeFunctionId =
        getNextNativeFunctionId()

      def overloadRegistryFor(methodName: MethodName): OverloadRegistry = new OverloadRegistry {
        def addOverload(parameters: Vector[FormalParameter], returnType: Type[TypeId], impl: FunctionImpl): NativeFunctionId =
          getNextNativeFunctionId()

        def addMultiParamGroupOverload(paramGroups: Vector[ParameterGroup], returnType: Type[TypeId], impl: FunctionImpl): NativeFunctionId =
          getNextNativeFunctionId()
      }

      def settings: MethodRegistry.Settings = settingsStack.current

      def modifySettings(fn: MethodRegistry.Settings => MethodRegistry.Settings): Unit =
        settingsStack.modify(fn)

      def pushSettings(fn: MethodRegistry.Settings => MethodRegistry.Settings): Unit =
        settingsStack.push(fn)

      def popSettings(): Unit =
        settingsStack.pop()
    }

    val typeRegistry: TypeRegistryBootstrap = new TypeRegistryBootstrap {
      def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId] =
        Type.Nominal(getNextTypeId())

      def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId] =
        Type.Nominal(getNextTypeId())

      def addTypeConstructor(name: QualifiedName.Full, first: TypeParameter[TypeId], more: TypeParameter[TypeId]*): Type.Constructor[TypeId] = {
        val params = NonEmptyVector(first, more: _*)
        Type.Constructor(getNextTypeId(), params)
      }

      def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit = ()

      def addRelationship(name: QualifiedName.Full, target: Type[TypeId]): Unit = ()

      def registerCoreValueType(name: QualifiedName.Full, typ: Type.Nominal[TypeId]): Type.Nominal[TypeId] = typ

      def registerCoreRefType(name: QualifiedName.Full, typ: Type.Nominal[TypeId]): Type.Nominal[TypeId] = typ
    }

    val runtimeContextRegistry: RuntimeContextRegistry = () => getNextRuntimeContextId()

    val standardTypes: StandardTypes = new StandardTypesImpl(typeRegistry)
  }
}
