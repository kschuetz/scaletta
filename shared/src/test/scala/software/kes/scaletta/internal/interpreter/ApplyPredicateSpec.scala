package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class ApplyPredicateSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  describe("Interpreter ApplyPredicate") {
    it("should correctly apply a predicate to an integer") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0))
      val assembler = builder.mainAssembler()

      val isEven: Any => Boolean = {
        case x: Int => x % 2 == 0
        case _ => false
      }

      assembler.pushImmediateInt(44)
      assembler.pushImmediateObject(isEven)
      assembler.applyPredicate()
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe true
    }

    it("should correctly apply a predicate to a string") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0))
      val assembler = builder.mainAssembler()

      val isLong: Any => Boolean = {
        case s: String => s.length > 5
        case _ => false
      }

      assembler.pushImmediateObject("Hello World")
      assembler.pushImmediateObject(isLong)
      assembler.applyPredicate()
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe true
    }

    it("should correctly apply a predicate that returns false") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0))
      val assembler = builder.mainAssembler()

      val isEven: Any => Boolean = {
        case x: Int => x % 2 == 0
        case _ => false
      }

      assembler.pushImmediateInt(41)
      assembler.pushImmediateObject(isEven)
      assembler.applyPredicate()
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe false
    }
  }
}
