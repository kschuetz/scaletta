package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.VarSpaceSignature

class ProgramBuilderSpec extends AnyFunSpec with Matchers {
  describe("ProgramBuilder") {
    it("should build a program with a main function") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(43)

      val program = builder.build()
      program.returnType shouldBe BasicTypes.Int
      program.functions.size shouldBe 1
      program.mainFunction.instructions should not be empty
    }

    it("should build a program with additional functions") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()
      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)

      val otherAssembler = builder.addFunction(VarSpaceSignature.empty)
      otherAssembler.pushImmediateInt(13)

      val program = builder.build()
      program.functions.size shouldBe 2
      program.functions.tail.head.instructions should not be empty
    }

    it("should share the constant pool across all functions") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()
      val longValue = 123456789L
      mainAssembler.pushImmediateLong(longValue)

      val otherAssembler = builder.addFunction(VarSpaceSignature.empty)
      otherAssembler.pushImmediateLong(longValue)

      val program = builder.build()
      program.constantPool.longs.size shouldBe 1
    }
  }
}
