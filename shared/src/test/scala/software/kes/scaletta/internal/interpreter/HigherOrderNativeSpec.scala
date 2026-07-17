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

    it("should throw UnsupportedOperationException for NativeStep.Call") {
      val tableBuilder = NativeFunctionTable.builder()
      val callHO = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.higherOrder { _ =>
        NativeStep.Call(null, _ => NativeStep.Done(1))
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.callNative(callHO)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      intercept[UnsupportedOperationException] {
        interpreter.run(emptyContextReader)
      }
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
