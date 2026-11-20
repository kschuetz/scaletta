package software.kes.scaletta.internal.builtins

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.library.standard.CommonParameterNames
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types.TypeRegistryImpl

final class MethodUniverseBuilderSpec extends AnyFunSpec with Matchers {

  describe("MethodUniverseBuilder") {
    it("should support static variadic method registration") {
      val f = new Fixtures
      val levelParam = FormalParameter(Name("level"), CoreTypes.StringT)
      val messagesParam = VariadicParameter(Name("messages"), CoreTypes.StringT)
      val logMethodName = MethodName(ReceiverType.Static(f.ns), Name("log"))

      val id = f.builder.addVariadicMethod(
        logMethodName,
        Vector(levelParam),
        messagesParam,
        CoreTypes.UnitT,
        FunctionImpl.objectResult(_ => "ok")
      )

      val universe = f.builder.build()
      val defn = universe.getDefinition(id)

      defn.paramGroups.length shouldBe 1
      defn.paramGroups.head.params shouldBe Vector(levelParam)
      defn.paramGroups.head.variadic shouldBe Some(messagesParam)
      defn.returnType shouldBe CoreTypes.UnitT

      val nativeFn = universe.dispatchTable.get(id)
      nativeFn.params.paramCount shouldBe 2
      nativeFn.params.basicTypeOf(0) shouldBe BasicTypes.Object
      nativeFn.params.basicTypeOf(1) shouldBe BasicTypes.Object
    }

    it("should preserve variadic metadata and inject synthetic this for instance variadic methods") {
      val f = new Fixtures
      val patternParam = FormalParameter(Name("pattern"), CoreTypes.StringT)
      val argsParam = VariadicParameter(Name("args"), CoreTypes.AnyT)
      val formatMethodName = MethodName(ReceiverType.Instance(f.formatterType), Name("format"))

      val id = f.builder.addVariadicMethod(
        formatMethodName,
        Vector(patternParam),
        argsParam,
        CoreTypes.StringT,
        FunctionImpl.objectResult(_ => "formatted")
      )

      val universe = f.builder.build()
      val defn = universe.getDefinition(id)

      defn.paramGroups.length shouldBe 1
      defn.paramGroups.head.params shouldBe Vector(
        FormalParameter(Name("this"), f.formatterType),
        patternParam
      )
      defn.paramGroups.head.variadic shouldBe Some(argsParam)
      defn.returnType shouldBe CoreTypes.StringT

      val nativeFn = universe.dispatchTable.get(id)
      nativeFn.params.paramCount shouldBe 3
      nativeFn.params.basicTypeOf(0) shouldBe BasicTypes.Object
      nativeFn.params.basicTypeOf(1) shouldBe BasicTypes.Object
      nativeFn.params.basicTypeOf(2) shouldBe BasicTypes.Object
    }

    it("should support registering overloads through OverloadRegistry including variadic methods") {
      val f = new Fixtures
      val printMethodName = MethodName(ReceiverType.Static(f.ns), Name("print"))
      val overloadRegistry = f.builder.overloadRegistryFor(printMethodName)

      val fixedId = overloadRegistry.addOverload(
        Vector(FormalParameter(CommonParameterNames.x, CoreTypes.StringT)),
        CoreTypes.UnitT,
        FunctionImpl.objectResult(_ => "ok")
      )

      val variadicId = overloadRegistry.addVariadicOverload(
        Vector(FormalParameter(Name("prefix"), CoreTypes.StringT)),
        VariadicParameter(Name("items"), CoreTypes.StringT),
        CoreTypes.UnitT,
        FunctionImpl.objectResult(_ => "ok")
      )

      val universe = f.builder.build()

      val fixedDefn = universe.getDefinition(fixedId)
      fixedDefn.paramGroups.head.variadic shouldBe None
      val fixedNativeFn = universe.dispatchTable.get(fixedId)
      fixedNativeFn.params.paramCount shouldBe 1
      fixedNativeFn.params.basicTypeOf(0) shouldBe BasicTypes.Object

      val variadicDefn = universe.getDefinition(variadicId)
      variadicDefn.paramGroups.head.variadic shouldBe Some(VariadicParameter(Name("items"), CoreTypes.StringT))
      val variadicNativeFn = universe.dispatchTable.get(variadicId)
      variadicNativeFn.params.paramCount shouldBe 2
      variadicNativeFn.params.basicTypeOf(0) shouldBe BasicTypes.Object
      variadicNativeFn.params.basicTypeOf(1) shouldBe BasicTypes.Object
    }

    it("should preserve existing behavior and signatures for non-variadic methods") {
      val f = new Fixtures
      val addMethodName = MethodName(ReceiverType.Static(f.ns), Name("add"))
      val id = f.builder.addMethod(
        addMethodName,
        Vector(
          FormalParameter(Name("a"), CoreTypes.IntT),
          FormalParameter(Name("b"), CoreTypes.IntT)
        ),
        CoreTypes.IntT,
        FunctionImpl.intResult(_ => 43)
      )

      val universe = f.builder.build()
      val defn = universe.getDefinition(id)

      defn.paramGroups.length shouldBe 1
      defn.paramGroups.head.params.length shouldBe 2
      defn.paramGroups.head.variadic shouldBe None
      defn.returnType shouldBe CoreTypes.IntT

      val nativeFn = universe.dispatchTable.get(id)
      nativeFn.params.paramCount shouldBe 2
      nativeFn.params.basicTypeOf(0) shouldBe BasicTypes.Int
      nativeFn.params.basicTypeOf(1) shouldBe BasicTypes.Int
    }
  }

  final class Fixtures {
    val ns: PackagePath.Absolute = PackagePath.absolute(PackageSegment("test"))
    val typeRegistry: TypeRegistryImpl = new TypeRegistryImpl()
    val formatterType: Type.Nominal[TypeId] = typeRegistry.addRefType(ns.qualify(Name("Formatter")), RuntimeTypeInfo.any)
    val builder: MethodUniverseBuilder = MethodUniverseBuilder.create()
  }
}
