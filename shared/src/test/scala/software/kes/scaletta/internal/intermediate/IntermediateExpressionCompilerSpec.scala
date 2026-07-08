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

class IntermediateExpressionCompilerSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("IntermediateExpressionCompiler") {
    it("should compile literals") {
      val expr = int(41)
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should compile native calls") {
      val expr = NativeCall(stdLib.arithmetic.int.add.int, Vector(int(10), int(31)))
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle lexical references (parameters)") {
      val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
      val signature = VarSpaceSignature.of(frame)
      val expr = Reference(0, 0) // scope 0, slot 0 (param)
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 1), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 43))
      result.intValue() shouldBe 43
    }

    it("should handle WithBindings (val)") {
      val expr = WithBindings(
        Vector(Binding.Val(int(41))),
        Reference(0, 0)
      )
      // Main signature must have space for the local variable
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle nested WithBindings and scoping") {
      // { val x = 10; { val y = 31; x + y } }
      val expr = WithBindings(
        Vector(Binding.Val(int(10))),
        WithBindings(
          Vector(Binding.Val(int(31))),
          NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(1, 0), Reference(0, 0)))
        )
      )
      // Space for 2 ints (x and y)
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle local functions (recursion)") {
      // def factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)
      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // param n
        BasicTypes.Int,
        1
      )

      val factorialBody = Conditional(
        NativeCall(stdLib.comparison.int.le.int, Vector(Reference(0, 0), int(1))),
        int(1),
        NativeCall(stdLib.arithmetic.int.multiply.int, Vector(
          Reference(0, 0),
          LocalCall(1, 0, Vector(NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(0, 0), int(1)))))
        ))
      )

      val expr = WithBindings(
        Vector(Binding.Def(fSignature, factorialBody)),
        LocalCall(0, 0, Vector(int(5)))
      )

      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 120
    }

    it("should handle short-circuiting And") {
      // false && { sideEffect(); true }
      var failingNative: software.kes.scaletta.api.NativeFunctionId = software.kes.scaletta.api.NativeFunctionId(-1)
      val module = software.kes.scaletta.api.ScalettaModule { setup =>
        failingNative = setup.methodRegistry.addMethod(
          software.kes.scaletta.api.MethodName(software.kes.scaletta.api.ReceiverType.Static(software.kes.scaletta.api.PackagePath.root), software.kes.scaletta.api.Name("failAnd")),
          Vector.empty,
          CoreTypes.BooleanT,
          software.kes.scaletta.api.FunctionImpl.booleanResult(_ => throw new RuntimeException("Should not be called"))
        )
      }
      val customScaletta = Scaletta.create(Scaletta.addModule(module)).asInstanceOf[ScalettaFacade]
      val customNativeFunctions = customScaletta.universe.methodUniverse.dispatchTable

      val expr = And(boolean(false), NativeCall(failingNative, Vector.empty))
      val program = customScaletta.universe.compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0), expr)
      val interpreter = Interpreter.create(program, customNativeFunctions)
      interpreter.run(emptyContextReader).booleanValue() shouldBe false
    }

    it("should handle short-circuiting Or") {
      // true || { sideEffect(); false }
      var failingNative: software.kes.scaletta.api.NativeFunctionId = software.kes.scaletta.api.NativeFunctionId(-1)
      val module = software.kes.scaletta.api.ScalettaModule { setup =>
        failingNative = setup.methodRegistry.addMethod(
          software.kes.scaletta.api.MethodName(software.kes.scaletta.api.ReceiverType.Static(software.kes.scaletta.api.PackagePath.root), software.kes.scaletta.api.Name("failOr")),
          Vector.empty,
          CoreTypes.BooleanT,
          software.kes.scaletta.api.FunctionImpl.booleanResult(_ => throw new RuntimeException("Should not be called"))
        )
      }
      val customScaletta = Scaletta.create(Scaletta.addModule(module)).asInstanceOf[ScalettaFacade]
      val customNativeFunctions = customScaletta.universe.methodUniverse.dispatchTable

      val expr = Or(boolean(true), NativeCall(failingNative, Vector.empty))
      val program = customScaletta.universe.compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0), expr)
      val interpreter = Interpreter.create(program, customNativeFunctions)
      interpreter.run(emptyContextReader).booleanValue() shouldBe true
    }

    it("should handle StringConcat") {
      val expr = StringConcat(Vector(string("Hello, "), string("World!")))
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).value[String]() shouldBe "Hello, World!"
    }

    it("should handle LazyVal") {
      val expr = WithBindings(
        Vector(Binding.LazyVal(int(41), BasicTypes.Int)),
        NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), int(1)))
      )
      // Slot for LazyVal must be ObjectT
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader).intValue() shouldBe 42
    }

    it("should handle LazyVal and evaluate it only once") {
      var callCount = 0
      var incrementId: software.kes.scaletta.api.NativeFunctionId = software.kes.scaletta.api.NativeFunctionId(-1)

      val module = software.kes.scaletta.api.ScalettaModule { setup =>
        incrementId = setup.methodRegistry.addMethod(
          software.kes.scaletta.api.MethodName(software.kes.scaletta.api.ReceiverType.Static(software.kes.scaletta.api.PackagePath.root), software.kes.scaletta.api.Name("increment")),
          Vector.empty,
          CoreTypes.IntT,
          software.kes.scaletta.api.FunctionImpl.intResult { _ =>
            callCount += 1
            callCount
          }
        )
      }
      val customScaletta = Scaletta.create(Scaletta.addModule(module)).asInstanceOf[ScalettaFacade]
      val customNativeFunctions = customScaletta.universe.methodUniverse.dispatchTable

      val expr = WithBindings(
        Vector(Binding.LazyVal(NativeCall(incrementId, Vector.empty), BasicTypes.Int)),
        NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), Reference(0, 0)))
      )

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = customScaletta.universe.compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, customNativeFunctions)

      // (callCount=1) + (cached callCount=1) = 2
      interpreter.run(emptyContextReader).intValue() shouldBe 2
      callCount shouldBe 1
    }

    it("should handle LazyVal referencing outer variables") {
      val expr = WithBindings(
        Vector(Binding.Val(int(10))),
        WithBindings(
          Vector(Binding.LazyVal(NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(2, 0), int(31))), BasicTypes.Int)),
          Reference(0, 0)
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should detect circular dependencies in LazyVal") {
      val expr = WithBindings(
        Vector(Binding.LazyVal(Reference(1, 0), BasicTypes.Int)),
        Reference(0, 0)
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      val exception = the[RuntimeException] thrownBy {
        interpreter.run(emptyContextReader)
      }
      exception.getMessage should include("Circular dependency")
    }
  }
}
