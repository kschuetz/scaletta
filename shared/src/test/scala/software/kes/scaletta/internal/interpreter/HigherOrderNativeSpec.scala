package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{FunctionImpl, NativeStep}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class HigherOrderNativeSpec extends AnyFunSpec with Matchers {
  describe("Higher-order natives") {
    it("should handle NativeStep.Done with different types") {
      val tableBuilder = NativeFunctionTable.builder()

      val doneInt = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.higherOrder(_ => NativeStep.Done(43))))
      val doneLong = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Long, FunctionImpl.higherOrder(_ => NativeStep.Done(123L))))
      val doneBoolean = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Boolean, FunctionImpl.higherOrder(_ => NativeStep.Done(true))))
      val doneString = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Object, FunctionImpl.higherOrder(_ => NativeStep.Done("hello"))))

      val table = tableBuilder.result()

      def testDone(id: software.kes.scaletta.api.NativeFunctionId, expectedType: Byte, expectedValue: Any): Unit = {
        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, expectedType, 0))
        val assembler = builder.mainAssembler()
        assembler.callNative(id)
        assembler.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)
        val result = interpreter.run(emptyContextReader)

        expectedType match {
          case BasicTypes.Int => result.intValue() shouldBe expectedValue
          case BasicTypes.Long => result.longValue() shouldBe expectedValue
          case BasicTypes.Boolean => result.booleanValue() shouldBe expectedValue
          case BasicTypes.Object => result.value[AnyRef]() shouldBe expectedValue
        }
      }

      testDone(doneInt, BasicTypes.Int, 43)
      testDone(doneLong, BasicTypes.Long, 123L)
      testDone(doneBoolean, BasicTypes.Boolean, true)
      testDone(doneString, BasicTypes.Object, "hello")
    }

    it("should contract arguments before pushing result") {
      val tableBuilder = NativeFunctionTable.builder()
      val params = ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT)
      val addHO = tableBuilder.add(NativeFunction(params, BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val a = args.readInt(0)
        val b = args.readInt(1)
        NativeStep.Done(a + b)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(11)
      assembler.pushImmediateInt(12)
      assembler.callNative(addHO)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 23
    }

    it("should handle single NativeStep.Call round-trip") {
      val tableBuilder = NativeFunctionTable.builder()

      // A native that takes a callback and calls it once
      val applyOnce = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val target = args.readObject(0).asInstanceOf[software.kes.scaletta.api.CallTarget]
        target.setArgument(0, 41)
        NativeStep.Call(target, result => NativeStep.Done(result.asInstanceOf[Int] + 2))
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))

      // A local function that returns its argument
      val identityAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      identityAssembler.pushIntFromVar(0)
      identityAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.makeClosure(identityAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(applyOnce)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43
    }

    it("should reject multi-step NativeStep.Call sequences") {
      val tableBuilder = NativeFunctionTable.builder()
      val applyTwice = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val target = args.readObject(0).asInstanceOf[software.kes.scaletta.api.CallTarget]
        target.setArgument(0, 1)
        NativeStep.Call(target, _ => NativeStep.Call(target, _ => NativeStep.Done(3)))
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val identityAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      identityAssembler.pushIntFromVar(0)
      identityAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.makeClosure(identityAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(applyTwice)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      intercept[UnsupportedOperationException] {
        interpreter.run(emptyContextReader)
      }.getMessage should include("multi-step higher-order not yet supported")
    }

    it("should throw IllegalArgumentException for type mismatch") {
      val tableBuilder = NativeFunctionTable.builder()
      val wrongType = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.higherOrder(_ => NativeStep.Done("not an int"))))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.callNative(wrongType)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      val ex = intercept[IllegalArgumentException] {
        interpreter.run(emptyContextReader)
      }
      ex.getMessage should include("NativeFunctionId(0)")
      ex.getMessage should include("Int")
    }
  }
}
