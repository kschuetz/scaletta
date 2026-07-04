package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.runtime.{UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class StringConcatSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  describe("StringConcat opcode") {
    it("should concatenate strings in the order they were pushed") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateObject("A")
      assembler.pushImmediateObject("B")
      assembler.pushImmediateObject("C")
      assembler.stringConcat(3)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[String]() shouldBe "ABC"
    }

    it("should handle zero arguments by returning an empty string") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()
      assembler.stringConcat(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[String]() shouldBe ""
    }

    it("should handle a single argument") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateObject("Foo")
      assembler.stringConcat(1)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[String]() shouldBe "Foo"
    }

    it("should handle mixed types") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateObject("Value: ")
      assembler.pushImmediateInt(41)
      assembler.stringConcat(2)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[String]() shouldBe "Value: 41"
    }

    it("should handle nested concatenation") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateObject("A")
      assembler.pushImmediateObject("B")
      assembler.stringConcat(2) // "AB"
      assembler.pushImmediateObject("C")
      assembler.stringConcat(2) // "ABC"
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[String]() shouldBe "ABC"
    }
  }
}
