package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader
import software.kes.scaletta.util.VectorTwoPlus

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

    it("should compile UnitValue") {
      val expr = unit()
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).value[Unit]() shouldBe()
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

    it("should handle closures (lambdas and ClosureCall)") {
      // val f = (x: Int) => x + 1; f(40)
      val lambdaSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // param x
        BasicTypes.Int,
        1
      )
      val lambdaBody = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), int(1)))
      val lambdaExpr = Lambda(lambdaSignature, Vector.empty, lambdaBody)

      val expr = WithBindings(
        Vector(Binding.Val(lambdaExpr)),
        ClosureCall(Reference(0, 0), Vector(int(40)), BasicTypes.Int)
      )

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle closures with captures") {
      // val a = 11; val f = (x: Int) => x + a; f(32)
      val aValue = int(11)
      val lambdaSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
          CoreTypes.IntT, // param x
          CoreTypes.IntT // capture a
        ))),
        BasicTypes.Int,
        1 // 1 param
      )
      // Reference(0, 0) is x, Reference(0, 1) is a (capture)
      val lambdaBody = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), Reference(0, 1)))
      // In the context of lambdaExpr definition, a is at (0, 0)
      val lambdaExpr = Lambda(lambdaSignature, Vector(Reference(0, 0)), lambdaBody)

      val expr = WithBindings(
        Vector(Binding.Val(aValue)), // a = 11 (at slot 0 in outer scope)
        ClosureCall(lambdaExpr, Vector(int(32)), BasicTypes.Int)
      )

      val signature = VarSpaceSignature.of(
        FrameSignature.fromSeq(Seq(CoreTypes.IntT)) // a
      )
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 43
    }

    it("should handle short-circuiting And") {
      // false && { sideEffect(); true }
      var failingNative: NativeFunctionId = NativeFunctionId(-1)
      val module = ScalettaModule { setup =>
        failingNative = setup.methodRegistry.addMethod(
          MethodName(ReceiverType.Static(PackagePath.root), Name("failAnd")),
          Vector.empty,
          CoreTypes.BooleanT,
          FunctionImpl.booleanResult(_ => throw new RuntimeException("Should not be called"))
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
      var failingNative: NativeFunctionId = NativeFunctionId(-1)
      val module = ScalettaModule { setup =>
        failingNative = setup.methodRegistry.addMethod(
          MethodName(ReceiverType.Static(PackagePath.root), Name("failOr")),
          Vector.empty,
          CoreTypes.BooleanT,
          FunctionImpl.booleanResult(_ => throw new RuntimeException("Should not be called"))
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

    it("should handle Convert") {
      val expr = Convert(int(41), BasicTypes.Long)
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Long, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).longValue() shouldBe 41L

      val expr2 = Convert(long(43L), BasicTypes.Int)
      val program2 = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr2)
      val interpreter2 = Interpreter.create(program2, nativeFunctions)
      interpreter2.run(emptyContextReader).intValue() shouldBe 43
    }

    it("should handle LazyVal") {
      val expr = WithBindings(
        Vector(Binding.LazyVal(int(41))),
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
      var incrementId: NativeFunctionId = NativeFunctionId(-1)

      val module = ScalettaModule { setup =>
        incrementId = setup.methodRegistry.addMethod(
          MethodName(ReceiverType.Static(PackagePath.root), Name("increment")),
          Vector.empty,
          CoreTypes.IntT,
          FunctionImpl.intResult { _ =>
            callCount += 1
            callCount
          }
        )
      }
      val customScaletta = Scaletta.create(Scaletta.addModule(module)).asInstanceOf[ScalettaFacade]
      val customNativeFunctions = customScaletta.universe.methodUniverse.dispatchTable

      val expr = WithBindings(
        Vector(Binding.LazyVal(NativeCall(incrementId, Vector.empty))),
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
          Vector(Binding.LazyVal(NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(2, 0), int(31))))),
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
        Vector(Binding.LazyVal(Reference(1, 0))),
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

    it("should resolve type of Lambda") {
      val sig = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0)
      val expr = Lambda(sig, Vector.empty, int(41))
      TypeResolver.resolveType(expr, CompileEnv.empty, sig, nativeFunctions) shouldBe BasicTypes.Object
    }

    it("should compile Lambda without captures") {
      val lambdaSig = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0)
      val expr = Lambda(lambdaSig, Vector.empty, int(41))
      val mainSig = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0)
      val program = compiler.compile(mainSig, expr)

      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)
      result.value[AnyRef]() shouldNot be(null)
    }

    it("should compile Lambda with captures") {
      val outerSig = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val lambdaSig = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))),
        BasicTypes.Int,
        0
      )

      val expr = WithBindings(
        Vector(Binding.Val(int(10))),
        Lambda(lambdaSig, Vector(Reference(0, 0)), Reference(0, 0))
      )

      val program = compiler.compile(UserFunctionSignature(outerSig, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)
      result.value[AnyRef]() shouldNot be(null)
    }

    it("should compile Lambda capturing a LazyVal") {
      val outerSig = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val lambdaSig = UserFunctionSignature(
        VarSpaceSignature.of(
          FrameSignature.fromSeq(Seq(CoreTypes.IntT)),
          FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT))
        ),
        BasicTypes.Int,
        1
      )

      val expr = WithBindings(
        Vector(Binding.LazyVal(int(41))),
        Lambda(lambdaSig, Vector(Reference(0, 0)), Reference(0, 1))
      )

      val program = compiler.compile(UserFunctionSignature(outerSig, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      val closure = interpreter.run(emptyContextReader).value[AnyRef]()
      closure shouldNot be(null)
    }

    it("should compile Lambda capturing a mix of Vals and LazyVals") {
      val outerSig = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.AnyRefT)))
      val lambdaSig = UserFunctionSignature(
        VarSpaceSignature.of(
          FrameSignature.empty,
          FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.AnyRefT))
        ),
        BasicTypes.Int,
        0
      )

      val expr = WithBindings(
        Vector(Binding.Val(int(10)), Binding.LazyVal(int(31))),
        Lambda(lambdaSig, Vector(Reference(0, 0), Reference(0, 1)),
          NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), Reference(0, 1)))
        )
      )

      val program = compiler.compile(UserFunctionSignature(outerSig, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      val closure = interpreter.run(emptyContextReader).value[AnyRef]()
      closure shouldNot be(null)
    }

    it("should handle FunctionValue (def as a value)") {
      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // param x
        BasicTypes.Int,
        1
      )
      val fBody = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), int(1)))

      val expr = WithBindings(
        Vector(Binding.Def(fSignature, fBody)),
        ClosureCall(FunctionValue(0, 0, fSignature, Vector.empty), Vector(int(40)), BasicTypes.Int)
      )

      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle FunctionValue with captures") {
      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
          CoreTypes.IntT, // param x
          CoreTypes.IntT // capture a
        ))),
        BasicTypes.Int,
        1
      )
      // Reference(0, 0) is x, Reference(0, 1) is a (capture)
      val fBody = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), Reference(0, 1)))

      val expr = WithBindings(
        Vector(Binding.Val(int(10))), // a = 10
        WithBindings(
          Vector(Binding.Def(fSignature, fBody)),
          ClosureCall(FunctionValue(0, 0, fSignature, Vector(Reference(1, 0))), Vector(int(32)), BasicTypes.Int)
        )
      )

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 42
    }

    it("should handle Tuple") {
      val expr = Tuple(VectorTwoPlus(int(11), string("hello"), boolean(true)))
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader).value[Product]()
      result shouldBe(11, "hello", true)
    }
  }
}
