package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.FunctionImpl
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._

class TypeResolverSpec extends AnyFunSpec with Matchers {
  private val (nativeTable, idInt, idBool) = {
    val builder = NativeFunctionTable.builder()
    val id1 = builder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int.toInt, FunctionImpl.intResult(_ => 0)))
    val id2 = builder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Boolean.toInt, FunctionImpl.booleanResult(_ => true)))
    (builder.result(), id1, id2)
  }

  private val emptySignature = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0)
  private val emptyEnv = CompileEnv.empty

  describe("TypeResolver") {
    it("should resolve literal values") {
      TypeResolver.resolveType(IntermediateExpression.Value.int(41), emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Int
      TypeResolver.resolveType(IntermediateExpression.Value.string("hello"), emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Object
      TypeResolver.resolveType(IntermediateExpression.Value.boolean(true), emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Boolean
    }

    it("should resolve references to variables") {
      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)),
        BasicTypes.Object,
        2
      )
      val env = CompileEnv(List(Vector(BindingInfo.Val(0), BindingInfo.Val(1))), 2)

      TypeResolver.resolveType(IntermediateExpression.Reference(0, 0), env, signature, nativeTable) shouldBe BasicTypes.Int
      TypeResolver.resolveType(IntermediateExpression.Reference(0, 1), env, signature, nativeTable) shouldBe BasicTypes.Object
    }

    it("should resolve native calls") {
      TypeResolver.resolveType(IntermediateExpression.NativeCall(idInt, Vector.empty), emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Int
      TypeResolver.resolveType(IntermediateExpression.NativeCall(idBool, Vector.empty), emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Boolean
    }

    it("should resolve logical expressions") {
      val expr = IntermediateExpression.And(IntermediateExpression.Value.boolean(true), IntermediateExpression.Value.boolean(false))
      TypeResolver.resolveType(expr, emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Boolean
    }

    it("should resolve conditional expressions") {
      val expr = IntermediateExpression.Conditional(
        IntermediateExpression.Value.boolean(true),
        IntermediateExpression.Value.int(41),
        IntermediateExpression.Value.int(43)
      )
      TypeResolver.resolveType(expr, emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Int
    }

    it("should resolve WithBindings and local calls") {
      val localFuncSignature = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Float, 0)
      val expr = IntermediateExpression.WithBindings(
        Vector(
          Binding.Val(IntermediateExpression.Value.int(101)),
          Binding.Def(localFuncSignature, IntermediateExpression.Value.float(1.1f))
        ),
        IntermediateExpression.LocalCall(0, 1, Vector.empty)
      )

      TypeResolver.resolveType(expr, emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Float
    }

    it("should resolve WithBindings and references to bound values") {
      val expr = IntermediateExpression.WithBindings(
        Vector(
          Binding.Val(IntermediateExpression.Value.int(101))
        ),
        IntermediateExpression.Reference(0, 0)
      )

      // We need a signature that has the slot for the bound value
      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.of(CoreTypes.IntT)),
        BasicTypes.Object,
        0
      )

      TypeResolver.resolveType(expr, emptyEnv, signature, nativeTable) shouldBe BasicTypes.Int
    }

    it("should resolve StringConcat as Object") {
      val expr = IntermediateExpression.StringConcat(Vector(IntermediateExpression.Value.string("a"), IntermediateExpression.Value.int(1)))
      TypeResolver.resolveType(expr, emptyEnv, emptySignature, nativeTable) shouldBe BasicTypes.Object
    }
  }
}
