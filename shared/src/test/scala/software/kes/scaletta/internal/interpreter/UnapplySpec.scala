package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{Scaletta, UnapplyResult, UnapplyStrategy}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class UnapplySpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create()
    .asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)

  import stdLib.arithmetic

  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  describe("Unapply opcode") {
    it("should handle Unapply opcode success") {
      val strategy = UnapplyStrategy.unapplyTwo {
        case (a: Int, b: Int) => UnapplyResult.success2(a + 1, b + 1)
        case _ => UnapplyResult.failure
      }

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT)))
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateInt(10)
      assembler.pushImmediateInt(20)
      assembler.makeTuple(2) // value
      assembler.pushImmediateInt(2) // argCount
      assembler.pushImmediateObject(strategy) // strategy
      assembler.unapply()

      // Stack: [11, 21, true]
      assembler.ifTrue {
        assembler.popIntIntoVar(1) // pop 21
        assembler.popIntIntoVar(0) // pop 11
      }

      assembler.pushIntFromVar(0)
      assembler.pushIntFromVar(1)
      assembler.callNative(arithmetic.int.add.int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 32 // 11 + 21
    }

    it("should handle Unapply opcode failure") {
      val strategy = UnapplyStrategy.unapplyTwo {
        case (a: Int, b: Int) => UnapplyResult.success2(a + 1, b + 1)
        case _ => UnapplyResult.failure
      }

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateObject("not a tuple")
      assembler.pushImmediateInt(2)
      assembler.pushImmediateObject(strategy)
      assembler.unapply()

      // Stack: [false]
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe false
    }

    it("should handle unapply with zero arguments") {
      val strategy = UnapplyStrategy.unapplyZero {
        case "correct" => true
        case _ => false
      }

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateObject("correct")
      assembler.pushImmediateInt(0)
      assembler.pushImmediateObject(strategy)
      assembler.unapply()
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe true
    }
  }
}
