package software.kes.scaletta.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes
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

    describe("Block-based DSL") {
      it("should correctly assemble ifTrue") {
        withEnvironment(defaultSignature) { env =>
          import env._
          assembler.ifTrue {
            assembler.nop()
          }

          val func = userFunctionBuilder.build()
          // site 0: branchIfNot (12) to site 2 (after nop)
          // address 0: branchIfNot, offset = 2 - 0 - 1 = 1
          // address 1: nop
          // address 2: (bound here)
          func.instructions(0) shouldBe ((Opcodes.BranchIfNot << 24) | 1)
          func.instructions(1) shouldBe Opcodes.Nop
        }
      }

      it("should correctly assemble ifElse") {
        withEnvironment(defaultSignature) { env =>
          import env._
          assembler.ifElse(
            onTrue = {
              assembler.pushImmediateInt(41)
            },
            onFalse = {
              assembler.pushImmediateInt(43)
            }
          )

          val func = userFunctionBuilder.build()
          // address 0: branchIfNot to elseLabel
          // address 1: pushImmediateInt(41) (PushConst, type Int, value 41)
          // address 2: branch to exitLabel
          // elseLabel bound here (address 3)
          // address 3: pushImmediateInt(43) (PushConst, type Int, value 43)
          // exitLabel bound here (address 4)

          // 0: branchIfNot, offset = 3 - 0 - 1 = 2
          func.instructions(0) shouldBe ((Opcodes.BranchIfNot << 24) | 2)
          // 1: pushConst Int 41
          // pushConst is 1, Int is 2
          func.instructions(1) shouldBe ((Opcodes.PushConst << 24) | (BasicTypes.Int << 16) | 41)
          // 2: branch, offset = 4 - 2 - 1 = 1
          func.instructions(2) shouldBe ((Opcodes.Branch << 24) | 1)
          // 3: pushConst Int 43
          func.instructions(3) shouldBe ((Opcodes.PushConst << 24) | (BasicTypes.Int << 16) | 43)
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
