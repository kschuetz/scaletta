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
import software.kes.scaletta.util.{NonEmptyVector, VectorTwoPlus}

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

  test("price calculation with lazy tax and discount") {
    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.DoubleT, // basePrice
        CoreTypes.IntT, // countryCode
        CoreTypes.IntT, // customerStatus
        CoreTypes.AnyRefT, // taxRate (lazy)
        CoreTypes.AnyRefT // discount (lazy)
      ))),
      BasicTypes.Double,
      3
    )

    // lazy val taxRate = if (countryCode == 1) 0.05 else 0.10
    // countryCode is param 1. In LazyVal RHS, params are at scope 2.
    val taxRateExpr = Conditional(
      NativeCall(stdLib.equality.int.eq.int, Vector(Reference(2, 1), int(1))),
      double(0.05),
      double(0.10)
    )

    // lazy val discount = if (customerStatus == 1) 10.0 else 0.0
    // customerStatus is param 2. taxRate is at scope 1, slot 0.
    val discountExpr = Conditional(
      NativeCall(stdLib.equality.int.eq.int, Vector(Reference(2, 2), int(1))),
      double(10.0),
      double(0.0)
    )

    // body
    // basePrice is param 0.
    // taxRate is local 0 (Reference(0, 0))
    // discount is local 1 (Reference(0, 1))
    val body = WithBindings(
      Vector(
        Binding.LazyVal(taxRateExpr),
        Binding.LazyVal(discountExpr)
      ),
      Conditional(
        NativeCall(stdLib.comparison.double.gt.double, Vector(Reference(1, 0), double(100.0))),
        NativeCall(stdLib.arithmetic.double.multiply.double, Vector(
          NativeCall(stdLib.arithmetic.double.subtract.double, Vector(Reference(1, 0), Reference(0, 1))),
          NativeCall(stdLib.arithmetic.double.add.double, Vector(double(1.0), Reference(0, 0)))
        )),
        NativeCall(stdLib.arithmetic.double.multiply.double, Vector(
          Reference(1, 0),
          NativeCall(stdLib.arithmetic.double.add.double, Vector(double(1.0), Reference(0, 0)))
        ))
      )
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test case 1: basePrice = 50.0, countryCode = 1, customerStatus = 1
    // taxRate = 0.05
    // basePrice <= 100.0
    // result = 50.0 * (1.0 + 0.05) = 52.5
    val testCase1 = interpreter.run(emptyContextReader, vs => {
      vs.unsafeWriteDouble(0, 50.0)
      vs.unsafeWriteInt(1, 1)
      vs.unsafeWriteInt(2, 1)
    })
    testCase1.doubleValue() shouldBe (52.5 +- 0.001)

    // Test case 2: basePrice = 150.0, countryCode = 2, customerStatus = 1
    // taxRate = 0.10
    // discount = 10.0
    // basePrice > 100.0
    // result = (150.0 - 10.0) * (1.0 + 0.10) = 140.0 * 1.1 = 154.0
    val testCase2 = interpreter.run(emptyContextReader, vs => {
      vs.unsafeWriteDouble(0, 150.0)
      vs.unsafeWriteInt(1, 2)
      vs.unsafeWriteInt(2, 1)
    })
    testCase2.doubleValue() shouldBe (154.0 +- 0.001)
  }

  test("lazy val depending on another lazy val") {
    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.IntT, // input
        CoreTypes.AnyRefT, // a
        CoreTypes.AnyRefT // b
      ))),
      BasicTypes.Int,
      1
    )

    // lazy val a = input + 1
    // input is at scope 2, slot 0
    val aExpr = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(2, 0), int(1)))

    // lazy val b = a * 2
    // a is at scope 1, slot 0
    val bExpr = NativeCall(stdLib.arithmetic.int.multiply.int, Vector(Reference(1, 0), int(2)))

    val body = WithBindings(
      Vector(
        Binding.LazyVal(aExpr),
        Binding.LazyVal(bExpr)
      ),
      Reference(0, 1) // return b
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 10))
    // a = 11, b = 22
    result.intValue() shouldBe 22
  }

  test("discount eligibility logic with AND and OR") {
    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.BooleanT, // isVip
        CoreTypes.DoubleT, // totalSpent
        CoreTypes.BooleanT // hasCoupon
      ))),
      BasicTypes.Boolean,
      3
    )

    // (isVip || totalSpent > 101.0) && hasCoupon
    val body = And(
      Or(
        Reference(0, 0),
        NativeCall(stdLib.comparison.double.gt.double, Vector(Reference(0, 1), double(101.0)))
      ),
      Reference(0, 2)
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val testCases = Seq(
      // (isVip, totalSpent, hasCoupon) -> expected
      (true, 50.0, true) -> true, // (true || false) && true = true
      (true, 150.0, true) -> true, // (true || true) && true = true
      (false, 150.0, true) -> true, // (false || true) && true = true
      (false, 50.0, true) -> false, // (false || false) && true = false
      (true, 150.0, false) -> false, // (true || true) && false = false
      (false, 150.0, false) -> false, // (false || true) && false = false
      (false, 50.0, false) -> false // (false || false) && false = false
    )

    testCases.foreach { case ((isVip, totalSpent, hasCoupon), expected) =>
      val result = interpreter.run(emptyContextReader, vs => {
        vs.unsafeWriteBoolean(0, isVip)
        vs.unsafeWriteDouble(1, totalSpent)
        vs.unsafeWriteBoolean(2, hasCoupon)
      })
      result.booleanValue() shouldBe expected
    }
  }

  test("closure capturing multiple vals") {
    // def test(x: Int): Int = {
    //   val y = x + 10
    //   val z = y + 20
    //   val f = (a: Int) => a + y + z
    //   f(11)
    // }
    // if x = 1: y = 11, z = 31, f(11) = 11 + 11 + 31 = 53

    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.IntT, // x (param 0)
        CoreTypes.IntT, // y (local 1)
        CoreTypes.IntT, // z (local 2)
        CoreTypes.AnyRefT // f (lambda 3)
      ))),
      BasicTypes.Int,
      1
    )

    // y = x + 10
    // x is Reference(1, 0)
    val yExpr = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(1, 0), int(10)))

    // z = y + 20
    // y is Reference(0, 0)
    val zExpr = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), int(20)))

    // lambda(a: Int) = a + y + z
    // captures: y, z
    // signature inside lambda: (a: Int, captured_y: Int, captured_z: Int)
    val fLambda = Lambda(
      signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
          CoreTypes.IntT, // a
          CoreTypes.IntT, // y
          CoreTypes.IntT // z
        ))),
        BasicTypes.Int,
        1
      ),
      captures = Vector(Reference(0, 0), Reference(0, 1)), // y, z from WithBindings scope
      body = NativeCall(stdLib.arithmetic.int.add.int, Vector(
        NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), Reference(0, 1))),
        Reference(0, 2)
      ))
    )

    val body = WithBindings(
      Vector(
        Binding.Val(yExpr),
        Binding.Val(zExpr),
        Binding.Val(fLambda)
      ),
      ClosureCall(Reference(0, 2), Vector(int(11)), BasicTypes.Int)
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteInt(0, 1))
    result.intValue() shouldBe 53
  }

  test("list map") {
    // def test(xs: List[Int]): List[Int] = xs.map(x => x + 1)

    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.AnyRefT // xs (param 0)
      ))),
      BasicTypes.Object,
      1
    )

    // lambda(x: Int) = x + 1
    val mapLambda = Lambda(
      signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
          CoreTypes.IntT // x
        ))),
        BasicTypes.Int,
        1
      ),
      captures = Vector.empty,
      body = NativeCall(stdLib.arithmetic.int.add.int, Vector(Reference(0, 0), int(1)))
    )

    val body = NativeCall(
      stdLib.collections.list.map,
      Vector(Reference(0, 0), mapLambda)
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val input = List(1, 2, 3)
    val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, input))
    result.value[Any]() shouldBe List(2, 3, 4)
  }

  test("list map to different type") {
    // def test(xs: List[Int]): List[String] = xs.map(x => if (x > 10) "high" else "low")

    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.AnyRefT // xs (param 0)
      ))),
      BasicTypes.Object,
      1
    )

    // lambda(x: Int) = if (x > 10) "high" else "low"
    val mapLambda = Lambda(
      signature = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
          CoreTypes.IntT // x
        ))),
        BasicTypes.Object,
        1
      ),
      captures = Vector.empty,
      body = Conditional(
        NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 0), int(10))),
        string("high"),
        string("low")
      )
    )

    val body = NativeCall(
      stdLib.collections.list.map,
      Vector(Reference(0, 0), mapLambda)
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val input = List(5, 11, 15, 2)
    val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, input))
    result.value[Any]() shouldBe List("low", "high", "high", "low")
  }

  test("complex match with guards and nested patterns") {
    import Pattern._
    import software.kes.scaletta.api.RuntimeTypeInfo

    // def test(x: Any): Int = x match {
    //   case (a: Int, b: Int) if a > b => 1
    //   case (a: Int, b: Int) if a < b => 2
    //   case (a: Int, b: Int) => 3
    //   case s: String if s == "special" => 4
    //   case _: String => 5
    //   case _ => 6
    // }

    // signature for the whole function
    // param 0: x (AnyRef)
    // slot 1: scrutinee temp
    // slot 2: a or s
    // slot 3: b
    val signature = UserFunctionSignature(
      VarSpaceSignature.of(FrameSignature.fromSeq(Seq(
        CoreTypes.AnyRefT, // param 0
        CoreTypes.AnyRefT, // slot 1
        CoreTypes.IntT, // slot 2
        CoreTypes.IntT, // slot 3
        CoreTypes.AnyRefT // slot 4
      ))),
      BasicTypes.Int,
      1
    )

    val intTypeInfo = RuntimeTypeInfo(_.isInstanceOf[Int])
    val stringTypeInfo = RuntimeTypeInfo(_.isInstanceOf[String])

    val case1 = Case(
      pattern = Tuple(VectorTwoPlus(
        Typed(Slot(0, 2), intTypeInfo),
        Typed(Slot(0, 3), intTypeInfo)
      )),
      guard = Some(NativeCall(stdLib.comparison.int.gt.int, Vector(Reference(0, 2), Reference(0, 3)))),
      body = int(1)
    )

    val case2 = Case(
      pattern = Tuple(VectorTwoPlus(
        Typed(Slot(0, 2), intTypeInfo),
        Typed(Slot(0, 3), intTypeInfo)
      )),
      guard = Some(NativeCall(stdLib.comparison.int.lt.int, Vector(Reference(0, 2), Reference(0, 3)))),
      body = int(2)
    )

    val case3 = Case(
      pattern = Tuple(VectorTwoPlus(
        Typed(Slot(0, 2), intTypeInfo),
        Typed(Slot(0, 3), intTypeInfo)
      )),
      guard = None,
      body = int(3)
    )

    val case4 = Case(
      pattern = Typed(Slot(0, 4), stringTypeInfo),
      guard = Some(NativeCall(stdLib.equality.string.eq.any, Vector(Reference(0, 4), string("special")))),
      body = int(4)
    )

    val case5 = Case(
      pattern = Typed(Wildcard, stringTypeInfo),
      guard = None,
      body = int(5)
    )

    val case6 = Case(
      pattern = Wildcard,
      guard = None,
      body = int(6)
    )

    val body = Match(
      Reference(0, 0),
      NonEmptyVector(case1, case2, case3, case4, case5, case6)
    )

    val program = compiler.compile(signature, body)
    val interpreter = Interpreter.create(program, nativeFunctions)

    val testCases = Seq(
      (10, 5) -> 1, // (10, 5) if 10 > 5 -> 1
      (5, 10) -> 2, // (5, 10) if 5 < 10 -> 2
      (7, 7) -> 3, // (7, 7) -> 3
      "special" -> 4, // "special" if "special" == "special" -> 4
      "other" -> 5, // "other" : String -> 5
      1.23 -> 6, // other -> 6
      (null: AnyRef) -> 6 // other -> 6
    )

    testCases.foreach { case (input, expected) =>
      val result = interpreter.run(emptyContextReader, vs => vs.unsafeWriteObject(0, input.asInstanceOf[AnyRef]))
      result.intValue() shouldBe expected
    }
  }
}
