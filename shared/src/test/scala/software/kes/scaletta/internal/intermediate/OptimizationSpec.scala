package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Opcodes
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{UserFunctionSignature, VarSpaceSignature}

class OptimizationSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("Optimization") {
    it("should not emit redundant Convert opcodes for NativeCall arguments") {
      val expr = NativeCall(stdLib.arithmetic.int.add.int, Vector(int(10), int(31)))
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val instructions = program.mainFunction.instructions

      // Expected opcodes (currently): PushConst, Convert, PushConst, Convert, CallNative, Return
      // Convert opcode is 23.

      val convertOpcodes = instructions.filter(i => (i >>> 24) == Opcodes.Convert)

      // Before fix, this will fail if I expect 0
      convertOpcodes should be(empty)
    }

    it("should not emit redundant Convert opcodes for Convert expressions") {
      val expr = Convert(int(41), BasicTypes.Int)
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val instructions = program.mainFunction.instructions

      val convertOpcodes = instructions.filter(i => (i >>> 24) == Opcodes.Convert)

      convertOpcodes should be(empty)
    }

    it("should emit Convert opcodes when types do not match") {
      val expr = Convert(int(41), BasicTypes.Long)
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Long, 0), expr)
      val instructions = program.mainFunction.instructions

      val convertOpcodes = instructions.filter(i => (i >>> 24) == Opcodes.Convert)

      convertOpcodes.size shouldBe 1
      (convertOpcodes.head & 0x00FF0000) >> 16 shouldBe BasicTypes.Long
    }
  }
}
