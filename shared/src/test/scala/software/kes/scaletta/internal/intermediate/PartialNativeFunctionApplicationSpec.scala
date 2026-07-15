package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class PartialNativeFunctionApplicationSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("PartialNativeFunctionApplication") {
    it("should compile and execute a partial application of a native function (1 hole)") {
      // val f = add(10, _)
      // f(31) => 41
      val addId = stdLib.arithmetic.int.add.int
      val expr = WithBindings(
        Vector(Binding.Val(int(10))),
        WithBindings(
          Vector(Binding.Val(PartialNativeFunctionApplication(addId, Vector(Some(Reference(1, 0)), None)))),
          ClosureCall(Reference(0, 0), Vector(int(31)), BasicTypes.Int)
        )
      )

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should compile and execute a partial application of a native function (multiple holes)") {
      // val f = add(_, _)
      // f(10, 31) => 41
      val addId = stdLib.arithmetic.int.add.int
      val expr = WithBindings(
        Vector(Binding.Val(PartialNativeFunctionApplication(addId, Vector(None, None)))),
        ClosureCall(Reference(0, 0), Vector(int(10), int(31)), BasicTypes.Int)
      )

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should compile and execute a partial application with only holes (identity-like)") {
      val addId = stdLib.arithmetic.int.add.int
      val expr = PartialNativeFunctionApplication(addId, Vector(None, None))
      val signature = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0)
      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      val closure = interpreter.run(emptyContextReader).value[AnyRef]()
      closure shouldNot be(null)
    }

    it("should compile and execute a partial application with a literal") {
      // val f = add(10, _)
      // f(31) => 41
      val addId = stdLib.arithmetic.int.add.int
      val expr = ClosureCall(
        PartialNativeFunctionApplication(addId, Vector(Some(int(10)), None)),
        Vector(int(31)),
        BasicTypes.Int
      )

      val signature = UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Int, 0)
      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should compile and execute a partial application with multiple pre-filled arguments (one literal)") {
      val addId = stdLib.arithmetic.int.add.int
      val expr = ClosureCall(
        PartialNativeFunctionApplication(addId, Vector(Some(int(10)), Some(int(31)))),
        Vector.empty,
        BasicTypes.Int
      )

      val signature = UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))), BasicTypes.Int, 0)
      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should compile and execute a partial application with a complex expression") {
      val addId = stdLib.arithmetic.int.add.int
      val expr = ClosureCall(
        PartialNativeFunctionApplication(addId, Vector(Some(NativeCall(addId, Vector(int(5), int(5)))), None)),
        Vector(int(31)),
        BasicTypes.Int
      )

      val signature = UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Int, 0)
      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }
  }
}
