package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class TailCallOptimizationSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("Tail Call Optimization") {
    it("should optimize self-tail-recursive local function") {
      // def sum(n: Int, acc: Int): Int = if (n == 0) acc else sum(n - 1, acc + n)
      val sumSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))), // n, acc
        BasicTypes.Int,
        2
      )

      val sumBody = Conditional(
        NativeCall(stdLib.equality.int.eq.int, Vector(Reference(0, 0), int(0))),
        Reference(0, 1),
        LocalCall(1, 0, Vector(
          NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(0, 0), int(1))),
          NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 1), Reference(0, 0)))
        ))
      )

      val expr = WithBindings(
        Vector(Binding.Def(sumSignature, sumBody)),
        LocalCall(0, 0, Vector(int(1000), int(0)))
      )

      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      val result = interpreter.run(emptyContextReader)
      result.intValue() shouldBe (1000 * 1001 / 2)
    }

    it("should NOT optimize non-tail-recursive call") {
      // def factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)
      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // param n
        BasicTypes.Int,
        1
      )

      val factorialBody = Conditional(
        NativeCall(stdLib.comparison.int.le.int, Vector(Reference(0, 0), int(1))),
        int(1),
        NativeCall(stdLib.arithmetic.int.multiply.int, Vector(
          Reference(0, 0),
          LocalCall(1, 0, Vector(NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(0, 0), int(1)))))
        ))
      )

      val expr = WithBindings(
        Vector(Binding.Def(fSignature, factorialBody)),
        LocalCall(0, 0, Vector(int(5)))
      )

      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 120
    }

    it("should optimize tail call inside Conditional") {
      // def f(n: Int): Int = if (n > 0) f(n - 1) else 41
      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // n
        BasicTypes.Int,
        1
      )

      val fBody = Conditional(
        NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), int(0))),
        LocalCall(1, 0, Vector(NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(0, 0), int(1))))),
        int(41)
      )

      val expr = WithBindings(
        Vector(Binding.Def(fSignature, fBody)),
        LocalCall(0, 0, Vector(int(10)))
      )

      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should optimize tail call as body of WithBindings") {
      // def f(n: Int): Int = if (n <= 0) 43 else { def dummy = 0; f(n - 1) }
      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), // n
        BasicTypes.Int,
        1
      )

      val fBody = Conditional(
        NativeCall(stdLib.comparison.int.le.int, Vector(Reference(0, 0), int(0))),
        int(43),
        WithBindings(
          Vector(Binding.Def(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), int(0))),
          LocalCall(2, 0, Vector(NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(1, 0), int(1)))))
        )
      )

      val expr = WithBindings(
        Vector(Binding.Def(fSignature, fBody)),
        LocalCall(0, 0, Vector(int(10)))
      )

      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 43
    }
  }
}
