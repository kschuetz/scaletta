package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{RuntimeContextReader, Scaletta}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.library.standard.testsupport.ArithmeticOpsLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, VarSpaceSignature}

class InterpreterSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create()
    .asInstanceOf[ScalettaFacade]
  private val arithmetic = new ArithmeticOpsLookup(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

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


    it("should perform basic arithmetic via a native call") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()

      assembler.pushImmediateInt(11)
      assembler.pushImmediateInt(12)
      assembler.callNative(arithmetic.int.add.int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 23
    }

    it("should perform basic arithmetic via a local function call") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val addTwelveAssembler = builder.addFunction(VarSpaceSignature.empty)
      addTwelveAssembler.pushImmediateInt(12)
      addTwelveAssembler.callNative(arithmetic.int.add.int)
      addTwelveAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 23
    }

    // TODO
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

      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 41
    }

    it("should handle mixed-type arithmetic") {
      val builder = ProgramBuilder.create(BasicTypes.Long, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()

      val largeLong = Int.MaxValue.toLong + 1
      assembler.pushImmediateInt(11)
      assembler.pushImmediateLong(largeLong)
      assembler.callNative(arithmetic.int.add.long)
      assembler.emitReturn()

      val program = builder.build()
      program.constantPool.longs should not be empty

      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.longValue() shouldBe (11L + largeLong)
    }

    it("should handle branching") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()

      val elseLabel = assembler.label()
      val exitLabel = assembler.label()

      assembler.pushImmediateInt(1) // condition (true)
      assembler.branchUnless(elseLabel)
      assembler.pushImmediateInt(41)
      assembler.branch(exitLabel)
      elseLabel.bind()
      assembler.pushImmediateInt(43)
      exitLabel.bind()
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 41
    }

    // TODO
    ignore("should handle nested local function calls with their own variables") {
      val mainFrame = FrameSignature.empty
      val mainSignature = VarSpaceSignature.of(mainFrame)
      val builder = ProgramBuilder.create(BasicTypes.Int, mainSignature)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val funcFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
      val funcSignature = VarSpaceSignature.of(funcFrame)
      val funcAssembler = builder.addFunction(funcSignature)
      // argument (11) is on operand stack
      funcAssembler.popIntIntoVar(0)
      funcAssembler.pushIntFromVar(0)
      funcAssembler.pushImmediateInt(30)
      funcAssembler.callNative(arithmetic.int.add.int)
      funcAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 41
    }
    it("should handle all basic types via Push") {
      val builder = ProgramBuilder.create(BasicTypes.Boolean, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()

      // We'll test push for types that use constant pool in pushImmediate
      assembler.pushImmediateLong(1234567890123L)
      assembler.pop()
      assembler.pushImmediateDouble(1.23456789)
      assembler.pop()
      assembler.pushImmediateFloat(1.23f)
      assembler.pop()
      assembler.pushImmediateObject("test")
      assembler.pop()
      assembler.pushImmediateBoolean(true)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe true
    }
  }
}
