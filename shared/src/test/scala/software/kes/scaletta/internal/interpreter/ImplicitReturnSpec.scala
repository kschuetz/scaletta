package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{RuntimeContextReader, Scaletta}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.library.standard.testsupport.ArithmeticOpsLookup
import software.kes.scaletta.internal.runtime.VarSpaceSignature

class ImplicitReturnSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val arithmetic = new ArithmeticOpsLookup(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  private val emptyContextReader = new RuntimeContextReader {
    override def readRuntimeContext[A](runtimeContextId: software.kes.scaletta.api.RuntimeContextId): A =
      throw new UnsupportedOperationException("No contexts")
  }

  private val emptyNativeFunctionTable = NativeFunctionTable.builder().result()

  describe("Interpreter with implicit returns") {
    it("should allow a top-level function to return without an explicit Return opcode") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(47)
      // No emitReturn() here

      val program = builder.build()
      val interpreter = Interpreter.create(program, emptyNativeFunctionTable)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 47
    }

    it("should allow a local function to return without an explicit Return opcode") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val localAssembler = builder.addFunction(VarSpaceSignature.empty)
      localAssembler.pushImmediateInt(31)
      localAssembler.pushImmediateInt(12)
      localAssembler.callNative(arithmetic.int.add.int)
      // No emitReturn() here

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43 // 31 + 12
    }

    it("should allow a local function with an explicit Return to work correctly alongside implicit return") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      // Implicit return here

      val localAssembler = builder.addFunction(VarSpaceSignature.empty)
      localAssembler.pushImmediateInt(12)
      localAssembler.emitReturn() // Explicit return

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 12
    }
  }
}
