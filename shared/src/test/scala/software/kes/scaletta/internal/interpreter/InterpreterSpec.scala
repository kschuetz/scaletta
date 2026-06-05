package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.RuntimeContextReader
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, VarSpaceSignature}

class InterpreterSpec extends AnyFunSpec with Matchers {
  private val emptyContextReader = new RuntimeContextReader {
    override def readRuntimeContext[A](runtimeContextId: software.kes.scaletta.api.RuntimeContextId): A =
      throw new UnsupportedOperationException("No contexts")
  }

  private val emptyNativeFunctionTable = NativeFunctionTable.builder().result()

  describe("Interpreter") {
    it("should execute a program that returns a constant integer") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(43)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, emptyNativeFunctionTable)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43
    }


    it("should perform basic arithmetic via a local function call") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val addOneAssembler = builder.addFunction(VarSpaceSignature.empty)
      // TODO: reimplement this using a native call to the plus function
      addOneAssembler.pushImmediateInt(12)
      // addOneAssembler.swap()
      // addOneAssembler.pop() // remove the 11
      addOneAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, emptyNativeFunctionTable)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 12
    }

    // TODO: fix this
    ignore("should handle local variables") {
      val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
      val assembler = builder.mainAssembler()

      val varIdx = 0
      assembler.pushImmediateInt(41)
      assembler.popIntIntoVar(varIdx)
      assembler.pushIntFromVar(varIdx)
      assembler.emitReturn()

      val program = builder.build()
      program.mainFunction.frameSignature.intCount shouldBe 1

      val interpreter = Interpreter.create(program, emptyNativeFunctionTable)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 41
    }
  }
}
