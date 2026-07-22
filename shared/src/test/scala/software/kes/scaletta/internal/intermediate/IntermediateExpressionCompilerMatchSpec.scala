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

    it("should match against a Reference scrutinee") {
      // x match { case 1 => 10; case 2 => 20; case _ => 30 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Literal(int(1)), None, int(10)),
        Case(Pattern.Literal(int(2)), None, int(20)),
        Case(Pattern.Wildcard, None, int(30))
      ))

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))) // x, temp
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 1), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 1)).intValue() shouldBe 10
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 2)).intValue() shouldBe 20
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 3)).intValue() shouldBe 30
    }

    it("should handle guards with NativeCalls") {
      // x match { case _ if x > 10 => 1; case _ => 0 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Wildcard, Some(NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), int(10)))), int(1)),
        Case(Pattern.Wildcard, None, int(0))
      ))

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))) // x, temp
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 1), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 15)).intValue() shouldBe 1
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 5)).intValue() shouldBe 0
    }

    it("should handle Slot patterns with Reference scrutinee and NativeCall in body") {
      // x match { case y => y + 1 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Slot(0, 1), None, NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 1), int(1))))
      ))

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT))) // x, y, temp
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 1), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 40)).intValue() shouldBe 41
    }

    it("should handle Typed patterns with Reference scrutinee") {
      val intType = RuntimeTypeInfo(isInstance = _.isInstanceOf[Int])
      val stringType = RuntimeTypeInfo(isInstance = _.isInstanceOf[String])

      // x match { case _: Int => 1; case _: String => 2; case _ => 3 }
      val expr = Match(Reference(0, 0), NonEmptyVector(
        Case(Pattern.Typed(Pattern.Wildcard, intType), None, int(1)),
        Case(Pattern.Typed(Pattern.Wildcard, stringType), None, int(2)),
        Case(Pattern.Wildcard, None, int(3))
      ))

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT, CoreTypes.AnyRefT))) // x, temp
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 1), expr)
      val interpreter = Interpreter.create(program, nativeFunctions)

      interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, 42.asInstanceOf[AnyRef])).intValue() shouldBe 1
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, "hello")).intValue() shouldBe 2
      interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, java.lang.Boolean.TRUE)).intValue() shouldBe 3
    }

    it("should handle Tuple patterns with Reference components") {
      // (x, y) match { case (1, 2) => 10; case (1, _) => 11; case (_, 2) => 12; case _ => 13 }
      val scrutinee = Tuple(VectorTwoPlus(Reference(0, 0), Reference(0, 1)))
      val expr = Match(scrutinee, NonEmptyVector(
        Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(1)), Pattern.Literal(int(2)))), None, int(10)),
        Case(Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(1)), Pattern.Wildcard)), None, int(11)),
        Case(Pattern.Tuple(VectorTwoPlus(Pattern.Wildcard, Pattern.Literal(int(2)))), None, int(12)),
        Case(Pattern.Wildcard, None, int(13))
      ))

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.AnyRefT))) // x, y, temp
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 2), expr)
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

    it("should handle nested Tuple patterns and bindings with Reference components") {
      // (x, (y, z)) match { case (1, (2, a)) => a }
      val innerTuple = Tuple(VectorTwoPlus(Reference(0, 1), Reference(0, 2)))
      val scrutinee = Tuple(VectorTwoPlus(Reference(0, 0), innerTuple))

      val innerPattern = Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(2)), Pattern.Slot(0, 3)))
      val pattern = Pattern.Tuple(VectorTwoPlus(Pattern.Literal(int(1)), innerPattern))

      val expr = Match(scrutinee, NonEmptyVector(
        Case(pattern, None, Reference(0, 3)),
        Case(Pattern.Wildcard, None, int(-1))
      ))

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT, CoreTypes.AnyRefT, CoreTypes.IntT))) // x, y, z, temp, a
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 3), expr)
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

    it("should handle tail calls in Match") {
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
