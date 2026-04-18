package software.kes.scaletta.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.runtime.VarSpaceSignature

class AssemblerSpec extends AnyFunSpec with Matchers {
  private val defaultSignature = VarSpaceSignature.empty

  describe("Assembler") {
    describe("Labels") {
      it("should handle forward branches") {
        withEnvironment(defaultSignature) { env =>
          import env._
          val label = assembler.label()
          assembler.branch(label) // site 0, offset 0 for now
          assembler.nop() // address 1
          label.bind() // address 2

          val func = userFunctionBuilder.build()

          // Branch instruction at 0 should have offset 2 - 0 - 1 = 1
          val expectedInstruction = (Opcodes.Branch << 24) | 1
          func.instructions(0) shouldBe expectedInstruction
          func.instructions(1) shouldBe Opcodes.Nop
        }
      }

      it("should handle backward branches") {
        withEnvironment(defaultSignature) { env =>
          import env._
          val label = assembler.label()
          label.bind() // address 0
          assembler.nop() // address 0
          assembler.branch(label) // address 1

          val func = userFunctionBuilder.build()

          // Branch instruction at 1 should have offset 0 - 1 - 1 = -2
          // -2 in 24-bit is 0xFFFFFE
          val expectedInstruction = (Opcodes.Branch << 24) | 0xFFFFFE
          func.instructions(1) shouldBe expectedInstruction
        }
      }

      it("should handle multiple branches to the same label") {
        withEnvironment(defaultSignature) { env =>
          import env._
          val label = assembler.label()
          assembler.branchIf(label) // site 0
          assembler.branchIfNot(label) // site 1
          label.bind() // address 2

          val func = userFunctionBuilder.build()

          // site 0: 2 - 0 - 1 = 1
          func.instructions(0) shouldBe ((Opcodes.BranchIf << 24) | 1)
          // site 1: 2 - 1 - 1 = 0
          func.instructions(1) shouldBe ((Opcodes.BranchIfNot << 24) | 0)
        }
      }

      it("should throw IllegalStateException when binding a label twice") {
        withEnvironment(defaultSignature) { env =>
          import env._
          val label = assembler.label()
          label.bind()
          an[IllegalStateException] should be thrownBy {
            label.bind()
          }
        }
      }
    }
  }

  private case class Environment(assembler: Assembler,
                                 userFunctionBuilder: UserFunctionBuilder,
                                 constantPoolBuilder: ConstantPoolBuilder)

  private def withEnvironment(signature: VarSpaceSignature)
                             (f: Environment => Unit): Unit = {
    val userFunctionBuilder = UserFunctionBuilder.create(signature)
    val constantPoolBuilder = ConstantPoolBuilder.create()
    val assembler = new Assembler(userFunctionBuilder, constantPoolBuilder)
    val environment = Environment(assembler, userFunctionBuilder, constantPoolBuilder)
    f(environment)
  }
}
