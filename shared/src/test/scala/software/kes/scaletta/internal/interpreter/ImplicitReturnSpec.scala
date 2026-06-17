package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class ImplicitReturnSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)

  import stdLib.arithmetic

  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  describe("Interpreter with implicit returns") {
    it("should allow a top-level function to return without an explicit Return opcode") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(47)
      // No emitReturn() here

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 47
    }

    it("should allow a local function to return without an explicit Return opcode") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val localAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
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
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      // Implicit return here

      val localAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      localAssembler.pushImmediateInt(12)
      localAssembler.emitReturn() // Explicit return

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 12
    }
  }
}
