package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class InterpreterComplexExampleSpec extends AnyFunSuite with Matchers {
  private val scaletta = Scaletta.create()
    .asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)

  import stdLib.arithmetic
  import stdLib.comparison

  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  test("factorial") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Int, signature)
    val assembler = builder.mainAssembler()

    // Variable layout:
    // var 0: input 'n' (passed via Initializer)
    // var 1: accumulator 'acc'
    val nVar = 0
    val accVar = 1

    val loopStart = assembler.label()
    val loopExit = assembler.label()

    // acc = 1
    assembler.pushImmediateInt(1)
    assembler.popIntIntoVar(accVar)

    // loopStart:
    loopStart.bind()

    // if (n <= 1) goto loopExit
    assembler.pushIntFromVar(nVar)
    assembler.pushImmediateInt(1)
    assembler.callNative(comparison.int.le.int)
    assembler.branchIf(loopExit)

    // acc = acc * n
    assembler.pushIntFromVar(accVar)
    assembler.pushIntFromVar(nVar)
    assembler.callNative(arithmetic.int.multiply.int)
    assembler.popIntIntoVar(accVar)

    // n = n - 1
    assembler.pushIntFromVar(nVar)
    assembler.pushImmediateInt(1)
    assembler.callNative(arithmetic.int.subtract.int)
    assembler.popIntIntoVar(nVar)

    // goto loopStart
    assembler.branch(loopStart)

    // loopExit:
    loopExit.bind()

    // return acc
    assembler.pushIntFromVar(accVar)
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test cases
    val testCases = Seq(
      0 -> 1,
      1 -> 1,
      2 -> 2,
      3 -> 6,
      4 -> 24,
      5 -> 120,
      6 -> 720,
      -1 -> 1
    )

    testCases.foreach { case (input, expected) =>
      val initializer = Initializer { vs =>
        vs.unsafeWriteInt(nVar, input)
        vs
      }
      val result = interpreter.run(emptyContextReader, initializer)
      result.intValue() shouldBe expected
    }
  }

}
