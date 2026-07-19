package software.kes.scaletta.benchmarks

import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter._
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class InterpreterBenchmark extends ScalettaBenchmark {

  private var program: Program = _
  private var nativeFunctions: software.kes.scaletta.internal.builtins.NativeFunctionTable = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
    val stdLib = StandardLibraryLookup.create(scaletta.universe)
    nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

    // Create a program that does a simple addition in a loop (simulated by repeated instructions here for now)
    // Or just a simple addition to start with.
    val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
    val assembler = builder.mainAssembler()

    // 10 + 20 + 30 + 40 + 51 = 151
    assembler.pushImmediateInt(10)
    assembler.pushImmediateInt(20)
    assembler.callNative(stdLib.arithmetic.int.add.int)
    assembler.pushImmediateInt(30)
    assembler.callNative(stdLib.arithmetic.int.add.int)
    assembler.pushImmediateInt(40)
    assembler.callNative(stdLib.arithmetic.int.add.int)
    assembler.pushImmediateInt(51)
    assembler.callNative(stdLib.arithmetic.int.add.int)
    assembler.emitReturn()

    program = builder.build()
  }

  test("simple addition") {
    runBenchmark("simple addition") {
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)
      result.intValue()
    }
  }

}
