package software.kes.scaletta.internal.builtins

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.{CoreTypes, ParamsSignature}

final class NativeFunctionTableSpec extends AnyFunSpec with Matchers {

  describe("NativeFunctionTable") {
    it("should store and retrieve NativeFunctionDefinition using createDefinition") {
      val builder = NativeFunctionTable.builder()
      val fn = NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.intResult(_ => 41))
      val paramGroups = ParameterGroup.single(FormalParameter(Name("x"), CoreTypes.IntT))

      val definition = builder.add(
        fn,
        allocatedId => NativeFunctionDefinition(
          paramGroups = paramGroups,
          returnType = CoreTypes.IntT,
          pure = true,
          nativeFunctionId = allocatedId
        )
      )
      val id = definition.nativeFunctionId

      id.value shouldBe 0
      builder.size shouldBe 1
      builder.getDefinition(id).nativeFunctionId.value shouldBe id.value
      builder.getDefinition(id).returnType shouldBe CoreTypes.IntT

      val table = builder.result()
      table.size shouldBe 1
      table.get(id) shouldBe fn

      val defn = table.getDefinition(id)
      defn.nativeFunctionId.value shouldBe id.value
      defn.paramGroups shouldBe paramGroups
      defn.returnType shouldBe CoreTypes.IntT
      defn.pure shouldBe true
    }

    it("should synthesize a default definition when only NativeFunction is added") {
      val builder = NativeFunctionTable.builder()
      val fn = NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.intResult(_ => 43))

      val id = builder.add(fn)
      id.value shouldBe 0

      val table = builder.result()
      table.size shouldBe 1
      table.get(id) shouldBe fn

      val defn = table.getDefinition(id)
      defn.nativeFunctionId.value shouldBe id.value
      defn.returnType shouldBe CoreTypes.IntT
      defn.paramGroups shouldBe empty
      defn.pure shouldBe false
    }

    it("should maintain parallel entries across multiple additions") {
      val builder = NativeFunctionTable.builder()
      val fn1 = NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.intResult(_ => 41))
      val fn2 = NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.intResult(_ => 43))
      val fn3 = NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.intResult(_ => 47))

      val id1 = builder.add(fn1, id => NativeFunctionDefinition(Vector.empty, CoreTypes.IntT, pure = true, id))
        .nativeFunctionId
      val id2 = builder.add(fn2)
      val id3 = builder.add(fn3, id => NativeFunctionDefinition(Vector.empty, CoreTypes.StringT, pure = false, id))
        .nativeFunctionId

      id1.value shouldBe 0
      id2.value shouldBe 1
      id3.value shouldBe 2

      val table = builder.result()
      table.size shouldBe 3

      table.get(id1) shouldBe fn1
      table.getDefinition(id1).returnType shouldBe CoreTypes.IntT

      table.get(id2) shouldBe fn2
      table.getDefinition(id2).returnType shouldBe CoreTypes.IntT

      table.get(id3) shouldBe fn3
      table.getDefinition(id3).returnType shouldBe CoreTypes.StringT
    }
  }
}
