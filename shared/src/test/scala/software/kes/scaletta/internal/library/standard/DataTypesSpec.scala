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

final class DataTypesSpec extends AnyFunSpec with Matchers {
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
   * Constructs a unary lambda accepting an Int and returning an Object (such as an Option).
   */
  private def makeIntToObjectLambda(bodyFn: Reference => IntermediateExpression): Lambda = {
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

  describe("DataTypes standard library") {

    describe("Option") {

      describe("map") {
        it("should return None when mapping over None") {
          val lambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val expr = NativeCall(stdLib.dataTypes.option.map, Vector(object_(None), lambda))

          runExpression(expr) shouldBe None
        }

        it("should perform homogeneous transformation (Int => Int)") {
          val inputOption = Some(11)
          val lambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val expr = NativeCall(stdLib.dataTypes.option.map, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe Some(18)
        }

        it("should perform type transformation (Int => String)") {
          val inputOption = Some(13)
          val lambda = makeIntToStringLambda(param =>
            StringConcat(Vector(string("val-"), param))
          )
          val expr = NativeCall(stdLib.dataTypes.option.map, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe Some("val-13")
        }

        it("should support mapping with closures capturing lexical variables") {
          val inputOption = Some(7)
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
            NativeCall(stdLib.dataTypes.option.map, Vector(object_(inputOption), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Some(21)
        }
      }

      describe("flatMap") {
        it("should return None when flatMapping over None") {
          val lambda = makeIntToObjectLambda(_ => object_(Some(7)))
          val expr = NativeCall(stdLib.dataTypes.option.flatMap, Vector(object_(None), lambda))

          runExpression(expr) shouldBe None
        }

        it("should return None when function returns None") {
          val inputOption = Some(11)
          val lambda = makeIntToObjectLambda(_ => object_(None))
          val expr = NativeCall(stdLib.dataTypes.option.flatMap, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe None
        }

        it("should perform homogeneous transformation (Int => Option[Int]) returning Some") {
          val inputOption = Some(7)
          val lambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10))),
              object_(Some(31)),
              object_(Some(3))
            )
          )
          val expr = NativeCall(stdLib.dataTypes.option.flatMap, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe Some(3)
        }

        it("should perform type transformation (Int => Option[String])") {
          val inputOption = Some(23)
          val lambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(Some("prime-gt-20")),
              object_(Some("prime-lte-20"))
            )
          )
          val expr = NativeCall(stdLib.dataTypes.option.flatMap, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe Some("prime-gt-20")
        }

        it("should support flatMapping with closures capturing lexical variables") {
          val inputOption = Some(17)
          val lambdaSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Object,
            1
          )
          val lambdaBody = Conditional(
            NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1))),
            object_(Some(61)),
            object_(None)
          )
          val lambda = Lambda(lambdaSignature, Vector(Reference(0, 0)), lambdaBody)

          val expr = WithBindings(
            Vector(Binding.Val(int(10))),
            NativeCall(stdLib.dataTypes.option.flatMap, Vector(object_(inputOption), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Some(61)
        }
      }

      describe("filter") {
        it("should return None when filtering None") {
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20)))
          )
          val expr = NativeCall(stdLib.dataTypes.option.filter, Vector(object_(None), lambda))

          runExpression(expr) shouldBe None
        }

        it("should retain value when predicate matches") {
          val inputOption = Some(11)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(5)))
          )
          val expr = NativeCall(stdLib.dataTypes.option.filter, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe Some(11)
        }

        it("should return None when predicate does not match") {
          val inputOption = Some(11)
          val lambda = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(5)))
          )
          val expr = NativeCall(stdLib.dataTypes.option.filter, Vector(object_(inputOption), lambda))

          runExpression(expr) shouldBe None
        }

        it("should support filtering with closures capturing lexical variables") {
          val inputOption = Some(43)
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
            NativeCall(stdLib.dataTypes.option.filter, Vector(object_(inputOption), lambda))
          )

          val program = compiler.compile(
            UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT))), BasicTypes.Object, 0),
            expr
          )
          val interpreter = Interpreter.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Some(43)
        }
      }
    }
  }
}
