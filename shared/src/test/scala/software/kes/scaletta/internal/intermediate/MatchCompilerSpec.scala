package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{RuntimeTypeInfo, Scaletta}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader
import software.kes.scaletta.util.{NonEmptyVector, VectorTwoPlus}

class MatchCompilerSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("Match expression compilation") {
    it("should compile a simple match with literals") {
      // x match { case 1 => 10; case 2 => 20; case _ => 30 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Literal(IntValue(1)), None, int(10)),
        Case(Pattern.Literal(IntValue(2)), None, int(20)),
        Case(Pattern.Wildcard, None, int(30))
      ))

      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))), // param x, temp for match
        BasicTypes.Int,
        1
      )

      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 1)).intValue() shouldBe 10
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 2)).intValue() shouldBe 20
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 3)).intValue() shouldBe 30
    }

    it("should handle guards") {
      // x match { case _ if x > 10 => 1; case _ => 0 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Wildcard, Some(NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), int(10)))), int(1)),
        Case(Pattern.Wildcard, None, int(0))
      ))

      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))), // param x, temp
        BasicTypes.Int,
        1
      )

      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 15)).intValue() shouldBe 1
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 5)).intValue() shouldBe 0
    }

    it("should handle Slot patterns (binding)") {
      // x match { case y => y + 1 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Slot(0, 1), None, NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 1), int(1))))
      ))

      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT))), // x, y, temp
        BasicTypes.Int,
        1
      )

      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 40)).intValue() shouldBe 41
    }

    it("should handle Typed patterns") {
      val intType = RuntimeTypeInfo(isInstance = _.isInstanceOf[Int])
      val stringType = RuntimeTypeInfo(isInstance = _.isInstanceOf[String])

      // x match { case _: Int => 1; case _: String => 2; case _ => 3 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Typed(Pattern.Wildcard, intType), None, int(1)),
        Case(Pattern.Typed(Pattern.Wildcard, stringType), None, int(2)),
        Case(Pattern.Wildcard, None, int(3))
      ))

      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT, CoreTypes.AnyRefT))), // x, temp
        BasicTypes.Int,
        1
      )

      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, 42.asInstanceOf[AnyRef])).intValue() shouldBe 1
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, "hello")).intValue() shouldBe 2
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, java.lang.Boolean.TRUE)).intValue() shouldBe 3
    }

    it("should handle Tuple patterns") {
      // (x, y) match { case (1, 2) => 10; case (1, _) => 11; case (_, 2) => 12; case _ => 13 }
      val scrutinee = Tuple(VectorTwoPlus(Reference(0, 0), Reference(0, 1)))
      val expr = Match(scrutinee, NonEmptyVector(
        Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(IntValue(1)), Pattern.Literal(IntValue(2)))), None, int(10)),
        Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(IntValue(1)), Pattern.Wildcard)), None, int(11)),
        Case(Pattern.Tuple(VectorTwoPlus(Pattern.Wildcard, Pattern.Literal(IntValue(2)))), None, int(12)),
        Case(Pattern.Wildcard, None, int(13))
      ))

      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.AnyRefT))), // x, y, temp
        BasicTypes.Int,
        2
      )

      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      def test(x: Int, y: Int) = interpreter.run(emptyContextReader, vs => {
        vs.unsafeWriteInt(0, x)
        vs.unsafeWriteInt(1, y)
      }).intValue()

      test(1, 2) shouldBe 10
      test(1, 3) shouldBe 11
      test(3, 2) shouldBe 12
      test(3, 3) shouldBe 13
    }

    // TODO: fix
    ignore("should handle nested Tuple patterns and bindings") {
      // (x, (y, z)) match { case (1, (2, a)) => a }
      val innerTuple = Tuple(VectorTwoPlus(Reference(0, 1), Reference(0, 2)))
      val scrutinee = Tuple(VectorTwoPlus(Reference(0, 0), innerTuple))

      val innerPattern = Pattern.Tuple(VectorTwoPlus(Pattern.Literal(IntValue(2)), Pattern.Slot(0, 3)))
      val pattern = Pattern.Tuple(VectorTwoPlus(Pattern.Literal(IntValue(1)), innerPattern))

      val expr = Match(scrutinee, NonEmptyVector(
        Case(pattern, None, Reference(0, 3)),
        Case(Pattern.Wildcard, None, int(-1))
      ))

      val signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT, CoreTypes.AnyRefT))), // x, y, z, a, temp
        BasicTypes.Int,
        3
      )

      val program = compiler.compile(signature, expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => {
        vs.unsafeWriteInt(0, 1)
        vs.unsafeWriteInt(1, 2)
        vs.unsafeWriteInt(2, 43)
      }).intValue() shouldBe 43

      interpreter.run(emptyContextReader, vs => {
        vs.unsafeWriteInt(0, 1)
        vs.unsafeWriteInt(1, 3)
        vs.unsafeWriteInt(2, 43)
      }).intValue() shouldBe -1
    }
  }
}
