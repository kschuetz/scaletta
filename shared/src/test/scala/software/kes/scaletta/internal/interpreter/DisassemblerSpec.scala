package software.kes.scaletta.internal.interpreter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.{UserFunctionSignature, VarSpaceSignature}

class DisassemblerSpec extends AnyFunSuite with Matchers {
  test("disassemble basic instructions") {
    val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
    val assembler = builder.mainAssembler()

    assembler.pushImmediateInt(43)
    assembler.pushImmediateInt(13)
    assembler.callNative(software.kes.scaletta.api.NativeFunctionId(101))
    assembler.emitReturn()

    val program = builder.build()
    val disassembly = Disassembler.disassemble(program.mainFunction, program.constantPool)

    disassembly should include("0000: PUSH_CONST Int 43")
    disassembly should include("0001: PUSH_CONST Int 13")
    disassembly should include("0002: CALL_NATIVE 101")
    disassembly should include("0003: RETURN")
  }

  test("disassemble branch instructions") {
    val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
    val assembler = builder.mainAssembler()

    val label = assembler.label()
    assembler.pushImmediateBoolean(true)
    assembler.branchIf(label)
    assembler.pushImmediateInt(1)
    assembler.emitReturn()
    label.bind()
    assembler.pushImmediateInt(2)
    assembler.emitReturn()

    val program = builder.build()
    val disassembly = Disassembler.disassemble(program.mainFunction, program.constantPool)

    disassembly should include("0001: BRANCH_IF 4 (2)")
  }

  test("disassemble constant pool resolution") {
    val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
    val assembler = builder.mainAssembler()

    assembler.pushImmediateObject("Hello, Scaletta!")
    assembler.emitReturn()

    val program = builder.build()
    val disassembly = Disassembler.disassemble(program.mainFunction, program.constantPool)

    disassembly should include("PUSH Object 1 (Hello, Scaletta!)")
  }

  test("disassemble makeClosure with empty capture plan") {
    val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
    val assembler = builder.mainAssembler()

    assembler.makeClosure(1, CapturePlan.empty)
    assembler.emitReturn()

    val program = builder.build()
    val disassembly = Disassembler.disassemble(program.mainFunction, program.constantPool)

    disassembly should include("MAKE_CLOSURE f1, cp0 (empty)")
  }
}
