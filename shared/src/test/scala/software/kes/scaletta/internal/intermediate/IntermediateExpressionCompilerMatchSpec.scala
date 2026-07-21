package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{RuntimeTypeInfo, Scaletta, UnapplyResult, UnapplyStrategy}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader
import software.kes.scaletta.util.{NonEmptyVector, VectorTwoPlus}

class IntermediateExpressionCompilerMatchSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("IntermediateExpressionCompiler (Match)") {
    it("should compile a simple literal match") {
      val expr = Match(
        int(41),
        NonEmptyVector(
          Case(Pattern.Literal(int(41)), None, string("matched")),
          Case(Pattern.Wildcard, None, string("not matched"))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).value[String]() shouldBe "matched"
    }

    it("should fall through to the next case if the first one doesn't match") {
      val expr = Match(
        int(43),
        NonEmptyVector(
          Case(Pattern.Literal(int(41)), None, string("first")),
          Case(Pattern.Literal(int(43)), None, string("second")),
          Case(Pattern.Wildcard, None, string("third"))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).value[String]() shouldBe "second"
    }

    it("should handle Slot patterns") {
      val expr = Match(
        int(41),
        NonEmptyVector(
          Case(Pattern.Slot(0, 0), None, Reference(0, 0))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle nested Tuple patterns and multiple cases correctly (stack check)") {
      val expr = Match(
        Tuple(VectorTwoPlus(int(1), int(2))),
        NonEmptyVector(
          // (1, 3) -> should fail at second element
          Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(1)), Pattern.Literal(int(3)))), None, int(10)),
          // (2, 2) -> should fail at first element
          Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(2)), Pattern.Literal(int(2)))), None, int(20)),
          // (1, 2) -> should match
          Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(1)), Pattern.Literal(int(2)))), None, int(30)),
          Case(Pattern.Wildcard, None, int(40))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 30
    }

    it("should handle guards") {
      val expr = Match(
        int(41),
        NonEmptyVector(
          Case(Pattern.Slot(0, 0), Some(boolean(false)), string("fail")),
          Case(Pattern.Slot(0, 0), Some(boolean(true)), string("success"))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).value[String]() shouldBe "success"
    }

    it("should handle As patterns") {
      val expr = Match(
        int(41),
        NonEmptyVector(
          Case(Pattern.As(0, 0, Pattern.Literal(int(41))), None, Reference(0, 0)),
          Case(Pattern.Wildcard, None, int(-1))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    it("should handle Typed patterns") {
      val intTypeInfo = RuntimeTypeInfo(isInstance = _.isInstanceOf[Int])
      val stringTypeInfo = RuntimeTypeInfo(isInstance = _.isInstanceOf[String])

      val expr = Match(
        int(41),
        NonEmptyVector(
          Case(Pattern.Typed(Pattern.Wildcard, stringTypeInfo), None, string("string")),
          Case(Pattern.Typed(Pattern.Wildcard, intTypeInfo), None, string("int"))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Object, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).value[String]() shouldBe "int"
    }

    it("should handle Product patterns") {
      case class Box(value: Int)
      val boxUnapply = UnapplyStrategy.unapplyDynamic { (arity, value) =>
        value match {
          case Box(v) if arity == 1 => UnapplyResult.success(Seq(v))
          case _ => UnapplyResult.failure
        }
      }
      val boxTypeInfo = RuntimeTypeInfo(isInstance = _.isInstanceOf[Box], unapplyStrategy = boxUnapply)

      val expr = Match(
        object_(Box(41)),
        NonEmptyVector(
          Case(Pattern.Product(boxTypeInfo, Vector(Pattern.Slot(0, 0))), None, Reference(0, 0)),
          Case(Pattern.Wildcard, None, int(-1))
        )
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT, CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader).intValue() shouldBe 41
    }

    // TODO: fix
    ignore("should handle tail calls in Match") {
      // def f(n: Int): Int = n match { case 0 => 1; case _ => f(n - 1) }
      // This tests emitMatch with tail = true

      val fSignature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT))),
        BasicTypes.Int,
        1
      )

      val fBody = Match(
        Reference(0, 0),
        NonEmptyVector(
          Case(Pattern.Literal(int(0)), None, int(43)),
          Case(Pattern.Wildcard, None, LocalCall(1, 0, Vector(NativeCall(stdLib.arithmetic.int.subtract.int, Vector(Reference(0, 0), int(1))))))
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
