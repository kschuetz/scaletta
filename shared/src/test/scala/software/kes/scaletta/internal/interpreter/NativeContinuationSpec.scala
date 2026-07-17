package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.NativeStep
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.runtime.{UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class NativeContinuationSpec extends AnyFunSpec with Matchers {
  describe("Native Continuation Frames") {
    val signature = UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0)
    val program = ProgramBuilder.create(signature).build()
    val nativeFunctions = NativeFunctionTable.builder().result()

    it("should push and pop a HigherOrderCont correctly") {
      val interpreter = Interpreter.create(program, nativeFunctions)
      var called = false
      val k: Any => NativeStep = res => {
        called = true
        res shouldBe 41
        NativeStep.done(43)
      }

      interpreter.pushNativeCont(k, BasicTypes.Int)
      interpreter.hasPendingNativeCont shouldBe true

      val (step, tag) = interpreter.resumeNativeCont(41)
      step shouldBe NativeStep.done(43)
      tag shouldBe BasicTypes.Int
      called shouldBe true
      interpreter.hasPendingNativeCont shouldBe false
    }

    it("should unwind multiple nested HigherOrderCont frames in LIFO order") {
      val interpreter = Interpreter.create(program, nativeFunctions)

      val k1: Any => NativeStep = res => NativeStep.done(res.asInstanceOf[Int] + 1)
      val k2: Any => NativeStep = res => NativeStep.done(res.asInstanceOf[Int] + 2)

      interpreter.pushNativeCont(k1, BasicTypes.Int)
      interpreter.pushNativeCont(k2, BasicTypes.Int)

      val (step1, tag1) = interpreter.resumeNativeCont(41)
      step1 shouldBe NativeStep.done(43)
      tag1 shouldBe BasicTypes.Int

      val (step2, tag2) = interpreter.resumeNativeCont(47)
      step2 shouldBe NativeStep.done(48)
      tag2 shouldBe BasicTypes.Int
    }

    it("should clear frames on interpreter reset") {
      val interpreter = Interpreter.create(program, nativeFunctions)
      val k: Any => NativeStep = res => NativeStep.done(res)

      interpreter.pushNativeCont(k, BasicTypes.Int)
      interpreter.hasPendingNativeCont shouldBe true

      interpreter.initialize(emptyContextReader)

      interpreter.hasPendingNativeCont shouldBe false
    }

    it("should not interfere with normal returns when no continuation is present") {
      val builder2 = ProgramBuilder.create(signature)
      val assembler2 = builder2.mainAssembler()
      assembler2.pushImmediateInt(41)
      assembler2.emitReturn()
      val program2 = builder2.build()

      val interpreter = Interpreter.create(program2, nativeFunctions)
      interpreter.initialize(emptyContextReader)

      interpreter.runUntilDone()
      interpreter.isDone shouldBe true
      interpreter.getResult.intValue() shouldBe 41
    }
  }
}
