package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class LazyEvalSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  describe("Lazy Evaluation") {
    it("should evaluate a lazy val only once and cache the result") {
      val frame = FrameSignature.of(CoreTypes.AnyRefT)
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))

      val lazyVarIdx = 0

      val main = builder.mainAssembler()
      main.lazyInit(BasicTypes.Int, lazyVarIdx)

      main.lazyEval(lazyVarIdx, 1) // first call
      main.lazyEval(lazyVarIdx, 1) // second call
      main.callNative(stdLib.arithmetic.int.add.int)
      main.emitReturn()

      val eval = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      eval.pushImmediateInt(43)
      eval.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.initialize(emptyContextReader)

      // Run until the first lazy evaluation is complete
      while (!interpreter.isDone && (interpreter.readAllVariables().length == 0 || interpreter.readAllVariables()(lazyVarIdx) == null || !interpreter.readAllVariables()(lazyVarIdx).asInstanceOf[LazyCell].evaluated)) {
        interpreter.step()
      }

      // Now it's evaluated. Change the cached value to verify that the second call uses the cache.
      val cell = interpreter.readAllVariables()(lazyVarIdx).asInstanceOf[LazyInt]
      cell.value shouldBe 43
      cell.value = 100

      // Run to completion
      interpreter.runUntilDone()

      // Result should be 43 (from first eval) + 100 (from cache) = 143
      interpreter.getResult.intValue() shouldBe 143
    }

    it("should support lazy variables of different types") {
      val frame = FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT, CoreTypes.AnyRefT))
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Object, 0))

      val lazyIntIdx = 0
      val lazyStringIdx = 1

      val main = builder.mainAssembler()
      main.lazyInit(BasicTypes.Int, lazyIntIdx)
      main.lazyInit(BasicTypes.Object, lazyStringIdx)

      main.lazyEval(lazyIntIdx, 1)
      main.pop()
      main.lazyEval(lazyStringIdx, 2)
      main.emitReturn()

      // Eval Int
      val evalInt = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      evalInt.pushImmediateInt(41)
      evalInt.emitReturn()

      // Eval String
      val evalString = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      evalString.pushImmediateObject("Hello")
      evalString.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[String]() shouldBe "Hello"

      val vars = interpreter.readAllVariables()
      vars(lazyIntIdx).asInstanceOf[LazyCell].evaluated shouldBe true
      vars(lazyStringIdx).asInstanceOf[LazyCell].evaluated shouldBe true
    }

    it("should handle nested lazy evaluation") {
      val frame = FrameSignature.of(CoreTypes.AnyRefT)
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))

      val xIdx = 0

      val main = builder.mainAssembler()
      main.lazyInit(BasicTypes.Int, xIdx)

      // return x
      main.lazyEval(xIdx, 1)
      main.emitReturn()

      // eval x = y + 1
      val evalX = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.of(CoreTypes.AnyRefT)), BasicTypes.Int, 0))
      val yIdx = 0
      evalX.lazyInit(BasicTypes.Int, yIdx)
      evalX.lazyEval(yIdx, 2)
      evalX.pushImmediateInt(1)
      evalX.callNative(stdLib.arithmetic.int.add.int)
      evalX.emitReturn()

      // eval y = 41
      val evalY = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      evalY.pushImmediateInt(41)
      evalY.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 42
    }
  }
}
