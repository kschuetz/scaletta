package software.kes.scaletta.internal.library.standard

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.intermediate.IntermediateExpression.Value._
import software.kes.scaletta.internal.intermediate.IntermediateExpression._
import software.kes.scaletta.internal.intermediate._
import software.kes.scaletta.internal.interpreter.Interpreter
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

final class CollectionsSpec extends AnyFunSpec with Matchers {
  private val scaletta: ScalettaFacade = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib: StandardLibraryLookup = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions: NativeFunctionTable = scaletta.universe.methodUniverse.dispatchTable
  private val compiler: IntermediateExpressionCompiler = scaletta.universe.compiler

  /**
   * Compiles and evaluates an intermediate expression to its resulting host value.
   */
  private def runExpression(expr: IntermediateExpression): Any = {
    val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0), expr)
    val interpreter = Interpreter.create(program, nativeFunctions)
    interpreter.run(emptyContextReader).value[Any]()
  }

  /**
   * Constructs a unary lambda accepting an Int and returning an Int.
   */
  private def makeIntToIntLambda(bodyFn: Reference => IntermediateExpression): Lambda = {
    val sig = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))),
      BasicTypes.Int,
      1
    )
    Lambda(sig, Vector.empty, bodyFn(Reference(0, 0)))
  }

  /**
   * Constructs a unary lambda accepting an Int and returning a String object.
   */
  private def makeIntToStringLambda(bodyFn: Reference => IntermediateExpression): Lambda = {
    val sig = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))),
      BasicTypes.Object,
      1
    )
    Lambda(sig, Vector.empty, bodyFn(Reference(0, 0)))
  }

  /**
   * Constructs a unary predicate lambda accepting an Int and returning a Boolean.
   */
  private def makeIntPredicateLambda(bodyFn: Reference => IntermediateExpression): Lambda = {
    val sig = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))),
      BasicTypes.Boolean,
      1
    )
    Lambda(sig, Vector.empty, bodyFn(Reference(0, 0)))
  }

  describe("Collections standard library") {

    describe("List") {

      describe("map") {
        it("should return Nil when mapping over an empty List") {
          val lambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val expr = NativeCall(stdLib.collections.list.map, Vector(object_(Nil), lambda))

          runExpression(expr) shouldBe Nil
        }

        it("should perform homogeneous transformation (Int => Int) preserving element order") {
          val inputList = List(11, 23, 31, 41)
          val lambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val expr = NativeCall(stdLib.collections.list.map, Vector(object_(inputList), lambda))

          runExpression(expr) shouldBe List(18, 30, 38, 48)
        }

        it("should perform type transformation (Int => String)") {
          val inputList = List(13, 41, 53)
          val lambda = makeIntToStringLambda(param =>
            StringConcat(Vector(string("val-"), param))
          )
          val expr = NativeCall(stdLib.collections.list.map, Vector(object_(inputList), lambda))

          runExpression(expr) shouldBe List("val-13", "val-41", "val-53")
        }

        it("should support mapping with closures capturing lexical variables") {
          val inputList = List(7, 11, 13)
          val lambdaSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: multiplier
            ))),
            BasicTypes.Int,
            1
          )
          val lambdaBody = NativeCall(stdLib.arithmetic.int.multiply.int, Vector(Reference(0, 0), Reference(0, 1)))
          val lambda = Lambda(lambdaSignature, Vector(Reference(0, 0)), lambdaBody)

          val expr = WithBindings(
            Vector(Binding.Val(int(3))),
            NativeCall(stdLib.collections.list.map, Vector(object_(inputList), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe List(21, 33, 39)
        }
      }

      describe("filter") {
        it("should return Nil when filtering an empty List") {
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20)))
          )
          val expr = NativeCall(stdLib.collections.list.filter, Vector(object_(Nil), lambda))

          runExpression(expr) shouldBe Nil
        }

        it("should retain all elements when predicate matches every element") {
          val inputList = List(11, 23, 31)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(5)))
          )
          val expr = NativeCall(stdLib.collections.list.filter, Vector(object_(inputList), lambda))

          runExpression(expr) shouldBe inputList
        }

        it("should return Nil when predicate matches no elements") {
          val inputList = List(11, 23, 31)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(5)))
          )
          val expr = NativeCall(stdLib.collections.list.filter, Vector(object_(inputList), lambda))

          runExpression(expr) shouldBe Nil
        }

        it("should retain only matching elements in sequence order") {
          val inputList = List(7, 43, 11, 61, 13)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20)))
          )
          val expr = NativeCall(stdLib.collections.list.filter, Vector(object_(inputList), lambda))

          runExpression(expr) shouldBe List(43, 61)
        }

        it("should support filtering with closures capturing lexical variables") {
          val inputList = List(7, 43, 11, 61, 13)
          val lambdaSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Boolean,
            1
          )
          val lambdaBody = NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1)))
          val lambda = Lambda(lambdaSignature, Vector(Reference(0, 0)), lambdaBody)

          val expr = WithBindings(
            Vector(Binding.Val(int(20))),
            NativeCall(stdLib.collections.list.filter, Vector(object_(inputList), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe List(43, 61)
        }
      }
    }

    describe("Vector") {

      describe("map") {
        it("should return empty Vector when mapping over an empty Vector") {
          val lambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val expr = NativeCall(stdLib.collections.vector.map, Vector(object_(Vector.empty), lambda))

          runExpression(expr) shouldBe Vector.empty
        }

        it("should perform homogeneous transformation (Int => Int) preserving element order") {
          val inputVector = Vector(11, 23, 31, 41)
          val lambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val expr = NativeCall(stdLib.collections.vector.map, Vector(object_(inputVector), lambda))

          runExpression(expr) shouldBe Vector(18, 30, 38, 48)
        }

        it("should perform type transformation (Int => String)") {
          val inputVector = Vector(13, 41, 53)
          val lambda = makeIntToStringLambda(param =>
            StringConcat(Vector(string("val-"), param))
          )
          val expr = NativeCall(stdLib.collections.vector.map, Vector(object_(inputVector), lambda))

          runExpression(expr) shouldBe Vector("val-13", "val-41", "val-53")
        }

        it("should support mapping with closures capturing lexical variables") {
          val inputVector = Vector(7, 11, 13)
          val lambdaSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: multiplier
            ))),
            BasicTypes.Int,
            1
          )
          val lambdaBody = NativeCall(stdLib.arithmetic.int.multiply.int, Vector(Reference(0, 0), Reference(0, 1)))
          val lambda = Lambda(lambdaSignature, Vector(Reference(0, 0)), lambdaBody)

          val expr = WithBindings(
            Vector(Binding.Val(int(3))),
            NativeCall(stdLib.collections.vector.map, Vector(object_(inputVector), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Vector(21, 33, 39)
        }
      }

      describe("filter") {
        it("should return empty Vector when filtering an empty Vector") {
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20)))
          )
          val expr = NativeCall(stdLib.collections.vector.filter, Vector(object_(Vector.empty), lambda))

          runExpression(expr) shouldBe Vector.empty
        }

        it("should retain all elements when predicate matches every element") {
          val inputVector = Vector(11, 23, 31)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(5)))
          )
          val expr = NativeCall(stdLib.collections.vector.filter, Vector(object_(inputVector), lambda))

          runExpression(expr) shouldBe inputVector
        }

        it("should return empty Vector when predicate matches no elements") {
          val inputVector = Vector(11, 23, 31)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(5)))
          )
          val expr = NativeCall(stdLib.collections.vector.filter, Vector(object_(inputVector), lambda))

          runExpression(expr) shouldBe Vector.empty
        }

        it("should retain only matching elements in sequence order") {
          val inputVector = Vector(7, 43, 11, 61, 13)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20)))
          )
          val expr = NativeCall(stdLib.collections.vector.filter, Vector(object_(inputVector), lambda))

          runExpression(expr) shouldBe Vector(43, 61)
        }

        it("should support filtering with closures capturing lexical variables") {
          val inputVector = Vector(7, 43, 11, 61, 13)
          val lambdaSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Boolean,
            1
          )
          val lambdaBody = NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1)))
          val lambda = Lambda(lambdaSignature, Vector(Reference(0, 0)), lambdaBody)

          val expr = WithBindings(
            Vector(Binding.Val(int(20))),
            NativeCall(stdLib.collections.vector.filter, Vector(object_(inputVector), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Vector(43, 61)
        }
      }
    }
  }
}
