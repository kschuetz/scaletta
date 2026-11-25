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
import software.kes.scaletta.internal.interpreter.testsupport.TestInterpreterFactory
import software.kes.scaletta.internal.library.standard.collection.{ListWithFilter, VectorWithFilter}
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

final class CollectionWithFilterSpec extends AnyFunSpec with Matchers {
  private val interpreterFactory = TestInterpreterFactory.create()
  private val scaletta: ScalettaFacade = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib: StandardLibraryLookup = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions: NativeFunctionTable = scaletta.universe.methodUniverse.dispatchTable
  private val compiler: IntermediateExpressionCompiler = scaletta.universe.compiler

  /**
   * Compiles and evaluates an intermediate expression to its resulting host value.
   */
  private def runExpression(expr: IntermediateExpression): Any = {
    val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0), expr)
    val interpreter = interpreterFactory.create(program, nativeFunctions)
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
   * Constructs a unary lambda accepting an Int and returning an Object (such as a List or Vector).
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

  describe("Collection withFilter standard library") {

    describe("List withFilter") {

      describe("withFilter invocation on List") {
        it("should return a ListWithFilter containing the underlying list and predicate") {
          val inputList = List(11, 23, 31)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val expr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))

          val result = runExpression(expr).asInstanceOf[ListWithFilter]
          result.underlying shouldBe inputList
          result.predicates.size shouldBe 1
        }
      }

      describe("withFilter invocation on ListWithFilter") {
        it("should accumulate multiple predicates into ListWithFilter") {
          val inputList = List(7, 11, 19, 23, 31)
          val predicate1 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val predicate2 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(30)))
          )

          val step1Expr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate1))
          val step2Expr = NativeCall(stdLib.collections.listWithFilter.withFilter, Vector(step1Expr, predicate2))

          val result = runExpression(step2Expr).asInstanceOf[ListWithFilter]
          result.underlying shouldBe inputList
          result.predicates.size shouldBe 2
        }
      }

      describe("map on ListWithFilter") {
        it("should return Nil when mapping over an empty ListWithFilter") {
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(Nil), predicate))
          val mapExpr = NativeCall(stdLib.collections.listWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Nil
        }

        it("should filter and transform elements with a single predicate (Int => Int)") {
          val inputList = List(7, 19, 23, 11, 31)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val mapExpr = NativeCall(stdLib.collections.listWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe List(26, 30, 38)
        }

        it("should filter and perform type transformation (Int => String)") {
          val inputList = List(7, 13, 23, 41)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(11)))
          )
          val mapLambda = makeIntToStringLambda(param =>
            StringConcat(Vector(string("item-"), param))
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val mapExpr = NativeCall(stdLib.collections.listWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe List("item-13", "item-23", "item-41")
        }

        it("should return Nil when single predicate matches no elements") {
          val inputList = List(7, 11, 13)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(50)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val mapExpr = NativeCall(stdLib.collections.listWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Nil
        }

        it("should retain and transform all elements when predicate matches all elements") {
          val inputList = List(11, 13, 17)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(5)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val mapExpr = NativeCall(stdLib.collections.listWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe List(18, 20, 24)
        }

        it("should support mapping with closures capturing lexical variables") {
          val inputList = List(7, 13, 19, 29)
          val predicateSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Boolean,
            1
          )
          val predicateBody = NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1)))
          val predicate = Lambda(predicateSignature, Vector(Reference(0, 0)), predicateBody)

          val mapSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: multiplier
            ))),
            BasicTypes.Int,
            1
          )
          val mapBody = NativeCall(stdLib.arithmetic.int.multiply.int, Vector(Reference(0, 0), Reference(0, 1)))
          val mapLambda = Lambda(mapSignature, Vector(Reference(0, 1)), mapBody)

          val expr = WithBindings(
            Vector(
              Binding.Val(int(10)), // capture 0: threshold
              Binding.Val(int(3)) // capture 1: multiplier
            ),
            NativeCall(
              stdLib.collections.listWithFilter.map,
              Vector(
                NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate)),
                mapLambda
              )
            )
          )

          val program = compiler.compile(
            UserFunctionSignature(
              VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))),
              BasicTypes.Object,
              0
            ),
            expr
          )
          val interpreter = interpreterFactory.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe List(39, 57, 87)
        }

        it("should filter and transform elements when chaining multiple predicates") {
          val inputList = List(3, 7, 11, 19, 23, 31, 37, 43)
          val predicate1 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val predicate2 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(35)))
          )
          val predicate3 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.equality.neq, Vector(param, int(23)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )

          val filter1 = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate1))
          val filter2 = NativeCall(stdLib.collections.listWithFilter.withFilter, Vector(filter1, predicate2))
          val filter3 = NativeCall(stdLib.collections.listWithFilter.withFilter, Vector(filter2, predicate3))
          val mapExpr = NativeCall(stdLib.collections.listWithFilter.map, Vector(filter3, mapLambda))

          runExpression(mapExpr) shouldBe List(18, 26, 38)
        }
      }

      describe("flatMap on ListWithFilter") {
        it("should return Nil when flatMapping over an empty ListWithFilter") {
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val flatMapLambda = makeIntToObjectLambda(_ => object_(List(7, 11)))
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(Nil), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.listWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Nil
        }

        it("should filter and expand elements with a single predicate (Int => List[Int])") {
          val inputList = List(7, 17, 23, 11, 31)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(15)))
          )
          val flatMapLambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(List(31, 37)),
              object_(List(3, 5))
            )
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.listWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe List(3, 5, 31, 37, 31, 37)
        }

        it("should filter and perform type transformation expansion (Int => List[String])") {
          val inputList = List(7, 13, 23)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val flatMapLambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(List("prime-gt-20-a", "prime-gt-20-b")),
              object_(List("prime-lte-20"))
            )
          )
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.listWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe List("prime-lte-20", "prime-gt-20-a", "prime-gt-20-b")
        }

        it("should return Nil when flatMap returns Nil for filtered elements") {
          val inputList = List(7, 19, 23)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val flatMapLambda = makeIntToObjectLambda(_ => object_(Nil))
          val withFilterExpr = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.listWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Nil
        }

        it("should support flatMapping with closures capturing lexical variables") {
          val inputList = List(7, 17, 31)
          val predicateSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Boolean,
            1
          )
          val predicateBody = NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1)))
          val predicate = Lambda(predicateSignature, Vector(Reference(0, 0)), predicateBody)

          val flatMapSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: tag
            ))),
            BasicTypes.Object,
            1
          )
          val flatMapBody = object_(List(61, 67))
          val flatMapLambda = Lambda(flatMapSignature, Vector(Reference(0, 1)), flatMapBody)

          val expr = WithBindings(
            Vector(
              Binding.Val(int(10)),
              Binding.Val(int(3))
            ),
            NativeCall(
              stdLib.collections.listWithFilter.flatMap,
              Vector(
                NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate)),
                flatMapLambda
              )
            )
          )

          val program = compiler.compile(
            UserFunctionSignature(
              VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))),
              BasicTypes.Object,
              0
            ),
            expr
          )
          val interpreter = interpreterFactory.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe List(61, 67, 61, 67)
        }

        it("should filter and expand elements when chaining multiple predicates") {
          val inputList = List(3, 7, 11, 19, 23, 31, 37)
          val predicate1 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val predicate2 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(30)))
          )
          val flatMapLambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(List(41, 43)),
              object_(List(13, 17))
            )
          )

          val filter1 = NativeCall(stdLib.collections.list.withFilter, Vector(object_(inputList), predicate1))
          val filter2 = NativeCall(stdLib.collections.listWithFilter.withFilter, Vector(filter1, predicate2))
          val flatMapExpr = NativeCall(stdLib.collections.listWithFilter.flatMap, Vector(filter2, flatMapLambda))

          runExpression(flatMapExpr) shouldBe List(13, 17, 13, 17, 41, 43)
        }
      }
    }

    describe("Vector withFilter") {

      describe("withFilter invocation on Vector") {
        it("should return a VectorWithFilter containing the underlying vector and predicate") {
          val inputVector = Vector(11, 23, 31)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val expr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))

          val result = runExpression(expr).asInstanceOf[VectorWithFilter]
          result.underlying shouldBe inputVector
          result.predicates.size shouldBe 1
        }
      }

      describe("withFilter invocation on VectorWithFilter") {
        it("should accumulate multiple predicates into VectorWithFilter") {
          val inputVector = Vector(7, 11, 19, 23, 31)
          val predicate1 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val predicate2 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(30)))
          )

          val step1Expr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate1))
          val step2Expr = NativeCall(stdLib.collections.vectorWithFilter.withFilter, Vector(step1Expr, predicate2))

          val result = runExpression(step2Expr).asInstanceOf[VectorWithFilter]
          result.underlying shouldBe inputVector
          result.predicates.size shouldBe 2
        }
      }

      describe("map on VectorWithFilter") {
        it("should return empty Vector when mapping over an empty VectorWithFilter") {
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(Vector.empty), predicate))
          val mapExpr = NativeCall(stdLib.collections.vectorWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Vector.empty
        }

        it("should filter and transform elements with a single predicate (Int => Int)") {
          val inputVector = Vector(7, 19, 23, 11, 31)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val mapExpr = NativeCall(stdLib.collections.vectorWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Vector(26, 30, 38)
        }

        it("should filter and perform type transformation (Int => String)") {
          val inputVector = Vector(7, 13, 23, 41)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(11)))
          )
          val mapLambda = makeIntToStringLambda(param =>
            StringConcat(Vector(string("vec-"), param))
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val mapExpr = NativeCall(stdLib.collections.vectorWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Vector("vec-13", "vec-23", "vec-41")
        }

        it("should return empty Vector when single predicate matches no elements") {
          val inputVector = Vector(7, 11, 13)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(50)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val mapExpr = NativeCall(stdLib.collections.vectorWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Vector.empty
        }

        it("should retain and transform all elements when predicate matches all elements") {
          val inputVector = Vector(11, 13, 17)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(5)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val mapExpr = NativeCall(stdLib.collections.vectorWithFilter.map, Vector(withFilterExpr, mapLambda))

          runExpression(mapExpr) shouldBe Vector(18, 20, 24)
        }

        it("should support mapping with closures capturing lexical variables") {
          val inputVector = Vector(7, 13, 19, 29)
          val predicateSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Boolean,
            1
          )
          val predicateBody = NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1)))
          val predicate = Lambda(predicateSignature, Vector(Reference(0, 0)), predicateBody)

          val mapSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: multiplier
            ))),
            BasicTypes.Int,
            1
          )
          val mapBody = NativeCall(stdLib.arithmetic.int.multiply.int, Vector(Reference(0, 0), Reference(0, 1)))
          val mapLambda = Lambda(mapSignature, Vector(Reference(0, 1)), mapBody)

          val expr = WithBindings(
            Vector(
              Binding.Val(int(10)),
              Binding.Val(int(3))
            ),
            NativeCall(
              stdLib.collections.vectorWithFilter.map,
              Vector(
                NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate)),
                mapLambda
              )
            )
          )

          val program = compiler.compile(
            UserFunctionSignature(
              VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))),
              BasicTypes.Object,
              0
            ),
            expr
          )
          val interpreter = interpreterFactory.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Vector(39, 57, 87)
        }

        it("should filter and transform elements when chaining multiple predicates") {
          val inputVector = Vector(3, 7, 11, 19, 23, 31, 37, 43)
          val predicate1 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val predicate2 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(35)))
          )
          val predicate3 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.equality.neq, Vector(param, int(23)))
          )
          val mapLambda = makeIntToIntLambda(param =>
            NativeCall(stdLib.arithmetic.int.add.int, Vector(param, int(7)))
          )

          val filter1 = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate1))
          val filter2 = NativeCall(stdLib.collections.vectorWithFilter.withFilter, Vector(filter1, predicate2))
          val filter3 = NativeCall(stdLib.collections.vectorWithFilter.withFilter, Vector(filter2, predicate3))
          val mapExpr = NativeCall(stdLib.collections.vectorWithFilter.map, Vector(filter3, mapLambda))

          runExpression(mapExpr) shouldBe Vector(18, 26, 38)
        }
      }

      describe("flatMap on VectorWithFilter") {
        it("should return empty Vector when flatMapping over an empty VectorWithFilter") {
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(17)))
          )
          val flatMapLambda = makeIntToObjectLambda(_ => object_(Vector(7, 11)))
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(Vector.empty), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.vectorWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Vector.empty
        }

        it("should filter and expand elements with a single predicate (Int => Vector[Int])") {
          val inputVector = Vector(7, 17, 23, 11, 31)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(15)))
          )
          val flatMapLambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(Vector(31, 37)),
              object_(Vector(3, 5))
            )
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.vectorWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Vector(3, 5, 31, 37, 31, 37)
        }

        it("should filter and perform type transformation expansion (Int => Vector[String])") {
          val inputVector = Vector(7, 13, 23)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val flatMapLambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(Vector("prime-gt-20-a", "prime-gt-20-b")),
              object_(Vector("prime-lte-20"))
            )
          )
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.vectorWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Vector("prime-lte-20", "prime-gt-20-a", "prime-gt-20-b")
        }

        it("should return empty Vector when flatMap returns empty Vector for filtered elements") {
          val inputVector = Vector(7, 19, 23)
          val predicate = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val flatMapLambda = makeIntToObjectLambda(_ => object_(Vector.empty))
          val withFilterExpr = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate))
          val flatMapExpr = NativeCall(stdLib.collections.vectorWithFilter.flatMap, Vector(withFilterExpr, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Vector.empty
        }

        it("should support flatMapping with closures capturing lexical variables") {
          val inputVector = Vector(7, 17, 31)
          val predicateSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: threshold
            ))),
            BasicTypes.Boolean,
            1
          )
          val predicateBody = NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), Reference(0, 1)))
          val predicate = Lambda(predicateSignature, Vector(Reference(0, 0)), predicateBody)

          val flatMapSignature = UserFunctionSignature(
            VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
              CoreTypes.IntT, // parameter
              CoreTypes.IntT // capture: tag
            ))),
            BasicTypes.Object,
            1
          )
          val flatMapBody = object_(Vector(61, 67))
          val flatMapLambda = Lambda(flatMapSignature, Vector(Reference(0, 1)), flatMapBody)

          val expr = WithBindings(
            Vector(
              Binding.Val(int(10)),
              Binding.Val(int(3))
            ),
            NativeCall(
              stdLib.collections.vectorWithFilter.flatMap,
              Vector(
                NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate)),
                flatMapLambda
              )
            )
          )

          val program = compiler.compile(
            UserFunctionSignature(
              VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))),
              BasicTypes.Object,
              0
            ),
            expr
          )
          val interpreter = interpreterFactory.create(program, nativeFunctions)
          interpreter.run(emptyContextReader).value[Any]() shouldBe Vector(61, 67, 61, 67)
        }

        it("should filter and expand elements when chaining multiple predicates") {
          val inputVector = Vector(3, 7, 11, 19, 23, 31, 37)
          val predicate1 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(10)))
          )
          val predicate2 = makeIntPredicateLambda(param =>
            NativeCall(stdLib.comparison.int.lt.int, Vector(param, int(30)))
          )
          val flatMapLambda = makeIntToObjectLambda(param =>
            Conditional(
              NativeCall(stdLib.comparison.int.gt.int, Vector(param, int(20))),
              object_(Vector(41, 43)),
              object_(Vector(13, 17))
            )
          )

          val filter1 = NativeCall(stdLib.collections.vector.withFilter, Vector(object_(inputVector), predicate1))
          val filter2 = NativeCall(stdLib.collections.vectorWithFilter.withFilter, Vector(filter1, predicate2))
          val flatMapExpr = NativeCall(stdLib.collections.vectorWithFilter.flatMap, Vector(filter2, flatMapLambda))

          runExpression(flatMapExpr) shouldBe Vector(13, 17, 13, 17, 41, 43)
        }
      }
    }
  }
}
