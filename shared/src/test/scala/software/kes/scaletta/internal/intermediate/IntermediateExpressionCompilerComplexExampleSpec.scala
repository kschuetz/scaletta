package software.kes.scaletta.internal.intermediate

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class IntermediateExpressionCompilerComplexExampleSpec extends AnyFunSuite with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  test("factorial") {
    // NOTE: The Scaletta language won't support recursive functions, by design. However,
    // the underlying interpreter will be capable of supporting them.

    // def factorial(n: Int): Int = {
    //   def go(n: Int, acc: Int): Int = if (n <= 1) acc else go(n - 1, n * acc)
    //   go(n, 1)
    // }

    val goSignature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))), // n, acc
      BasicTypes.Int,
      2
    )

    val goBody = Conditional(
      NativeCall(stdLib.comparison.int.le.int, Vector(Reference(0, 0), int(1))),
      Reference(0, 1),
      LocalCall(1, 0, Vector(
        NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(0, 0), int(1))),
        NativeCall(stdLib.arithmetic.int.multiply.int, Vector(Reference(0, 0), Reference(0, 1)))
      ))
    )

    val factorialSignature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // n
      BasicTypes.Int,
      1
    )

    val factorialBody = WithBindings(
      Vector(Binding.Def(goSignature, goBody)),
      LocalCall(0, 0, Vector(Reference(1, 0), int(1)))
    )

    val program = compiler.compile(factorialSignature, factorialBody)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val testCases = Seq(
      -1 -> 1,
      0 -> 1,
      1 -> 1,
      2 -> 2,
      3 -> 6,
      4 -> 24,
      5 -> 120,
      6 -> 720,
    )

    testCases.foreach { case (input, expected) =>
      val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, input))
      result.intValue() shouldBe expected
    }
  }
}
