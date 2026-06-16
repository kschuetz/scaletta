package software.kes.scaletta.internal.interpreter

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

  import stdLib.{arithmetic, comparison, equality, math}

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
      val initializer = Initializer { vs =>
        vs.unsafeWriteInt(nVar, input)
      }
      val result = interpreter.run(emptyContextReader, initializer)
      result.intValue() shouldBe expected
    }
  }

  test("Newton's method for square root (Double)") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.DoubleT, CoreTypes.DoubleT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Double, signature)
    val assembler = builder.mainAssembler()

    val nVar = 0
    val guessVar = 1
    val iterVar = 2

    val loopStart = assembler.label()
    val loopExit = assembler.label()

    // guess = n / 2.0
    assembler.pushDoubleFromVar(nVar)
    assembler.pushImmediateDouble(2.0)
    assembler.callNative(arithmetic.double.divide.double)
    assembler.popDoubleIntoVar(guessVar)

    // iter = 10
    assembler.pushImmediateInt(10)
    assembler.popIntIntoVar(iterVar)

    loopStart.bind()

    // if (iter <= 0) goto loopExit
    assembler.pushIntFromVar(iterVar)
    assembler.pushImmediateInt(0)
    assembler.callNative(comparison.int.le.int)
    assembler.branchIf(loopExit)

    // guess = (guess + n / guess) / 2.0
    assembler.pushDoubleFromVar(guessVar)
    assembler.pushDoubleFromVar(nVar)
    assembler.pushDoubleFromVar(guessVar)
    assembler.callNative(arithmetic.double.divide.double)
    assembler.callNative(arithmetic.double.add.double)
    assembler.pushImmediateDouble(2.0)
    assembler.callNative(arithmetic.double.divide.double)
    assembler.popDoubleIntoVar(guessVar)

    // iter = iter - 1
    assembler.pushIntFromVar(iterVar)
    assembler.pushImmediateInt(1)
    assembler.callNative(arithmetic.int.subtract.int)
    assembler.popIntIntoVar(iterVar)

    assembler.branch(loopStart)

    loopExit.bind()
    assembler.pushDoubleFromVar(guessVar)
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test Case: sqrt(25.0)
    val result1 = interpreter.run(emptyContextReader, Initializer { vs =>
      vs.unsafeWriteDouble(nVar, 25.0)
    })
    val d1 = result1.doubleValue()
    d1 shouldBe 5.0 +- 1e-8

    // Test Case: sqrt(2.0)
    val result2 = interpreter.run(emptyContextReader, Initializer { vs =>
      vs.unsafeWriteDouble(nVar, 2.0)
    })
    val d2 = result2.doubleValue()
    d2 shouldBe scala.math.sqrt(2.0) +- 1e-8
  }

  test("short-circuiting logical AND") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Boolean, signature)
    val assembler = builder.mainAssembler()
    val xVar = 0
    val exitLabel = assembler.label()

    // Logic: x > 0 && (10 / x) > 2
    // If x > 0 is false, result is false, short-circuit
    // If x > 0 is true, check second condition

    assembler.pushIntFromVar(xVar)
    assembler.pushImmediateInt(0)
    assembler.callNative(comparison.int.gt.int)

    // Short-circuit: if false, jump to exit (stack still has false)
    // If true, pop true and continue
    assembler.logicalAnd(exitLabel)

    assembler.pushImmediateInt(10)
    assembler.pushIntFromVar(xVar)
    assembler.callNative(arithmetic.int.divide.int)
    assembler.pushImmediateInt(2)
    assembler.callNative(comparison.int.gt.int)

    exitLabel.bind()
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test Case: x = 0 should return false and NOT throw ArithmeticException
    val result = interpreter.run(emptyContextReader, Initializer(_.unsafeWriteInt(xVar, 0)))
    result.booleanValue() shouldBe false

    // Test Case: x = 2 should return true (2 > 0 && (10 / 2) > 2)
    val result2 = interpreter.run(emptyContextReader, Initializer(_.unsafeWriteInt(xVar, 2)))
    result2.booleanValue() shouldBe true

    // Test Case: x = 5 should return false (5 > 0 && (10 / 5) > 2)
    val result3 = interpreter.run(emptyContextReader, Initializer(_.unsafeWriteInt(xVar, 5)))
    result3.booleanValue() shouldBe false
  }

  test("short-circuiting logical OR") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Boolean, signature)
    val assembler = builder.mainAssembler()
    val xVar = 0
    val exitLabel = assembler.label()

    // Logic: x < 1 || (10 / x) < 5
    // If x < 1, result is true, short-circuit
    // If x >= 1, check second condition

    assembler.pushIntFromVar(xVar)
    assembler.pushImmediateInt(1)
    assembler.callNative(comparison.int.lt.int)

    // Short-circuit: if true, jump to exit (stack still has true)
    // If false, pop true and continue
    assembler.logicalOr(exitLabel)

    assembler.pushImmediateInt(10)
    assembler.pushIntFromVar(xVar)
    assembler.callNative(arithmetic.int.divide.int)
    assembler.pushImmediateInt(5)
    assembler.callNative(comparison.int.lt.int)

    exitLabel.bind()
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Case 1: x = 0 should return true and NOT throw ArithmeticException
    val result1 = interpreter.run(emptyContextReader, Initializer(_.unsafeWriteInt(xVar, 0)))
    result1.booleanValue() shouldBe true

    // Case 2: x = 5. (5 < 1) is false. (10 / 5) = 2. 2 < 5 is true. Result true.
    val result2 = interpreter.run(emptyContextReader, Initializer(_.unsafeWriteInt(xVar, 5)))
    result2.booleanValue() shouldBe true

    // Case 3: x = 2. (2 < 1) is false. (10 / 2) = 5. 5 < 5 is false. Result false.
    val result3 = interpreter.run(emptyContextReader, Initializer(_.unsafeWriteInt(xVar, 2)))
    result3.booleanValue() shouldBe false
  }

  test("fibonacci (iterative)") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Int, signature)
    val assembler = builder.mainAssembler()

    // Variable layout:
    // var 0: input 'n' (remaining steps)
    // var 1: 'a' (current Fibonacci number)
    // var 2: 'b' (next Fibonacci number)
    val nVar = 0
    val aVar = 1
    val bVar = 2

    val loopStart = assembler.label()
    val loopExit = assembler.label()

    // a = 0
    assembler.pushImmediateInt(0)
    assembler.popIntIntoVar(aVar)

    // b = 1
    assembler.pushImmediateInt(1)
    assembler.popIntIntoVar(bVar)

    // loopStart:
    loopStart.bind()

    // if (n <= 0) goto loopExit
    assembler.pushIntFromVar(nVar)
    assembler.pushImmediateInt(0)
    assembler.callNative(comparison.int.le.int)
    assembler.branchIf(loopExit)

    // temp = a + b
    assembler.pushIntFromVar(aVar)
    assembler.pushIntFromVar(bVar)
    assembler.callNative(arithmetic.int.add.int)
    // We'll use the stack to hold 'temp' temporarily while we shift a and b

    // a = b
    assembler.pushIntFromVar(bVar)
    assembler.popIntIntoVar(aVar)

    // b = temp (from stack)
    assembler.popIntIntoVar(bVar)

    // n = n - 1
    assembler.pushIntFromVar(nVar)
    assembler.pushImmediateInt(1)
    assembler.callNative(arithmetic.int.subtract.int)
    assembler.popIntIntoVar(nVar)

    // goto loopStart
    assembler.branch(loopStart)

    // loopExit:
    loopExit.bind()

    // return a
    assembler.pushIntFromVar(aVar)
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test cases (n -> fib(n))
    val testCases = Seq(
      -1 -> 0,
      0 -> 0,
      1 -> 1,
      2 -> 1,
      3 -> 2,
      4 -> 3,
      5 -> 5,
      6 -> 8,
      7 -> 13,
      8 -> 21,
      9 -> 34,
      10 -> 55,
    )

    testCases.foreach { case (input, expected) =>
      val initializer = Initializer { vs =>
        vs.unsafeWriteInt(nVar, input)
      }
      val result = interpreter.run(emptyContextReader, initializer)
      result.intValue() shouldBe expected
    }
  }

  test("recursive power (exponentiation by squaring)") {
    /*
       Scala equivalent:
          def power(base: Int, exp: Int): Int = {
            if (exp == 0) 1
            else {
              val half = power(base, exp / 2)
              val squared = half * half  // Use 'dup' here
              if (exp % 2 == 0) squared
              else base * squared       // Use 'swap' here
            }
          }
     */
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Int, signature)

    val powerFuncIdx = 1
    val baseVar = 0
    val expVar = 1

    // Main function: just calls power(base, exp)
    val mainAssembler = builder.mainAssembler()
    mainAssembler.pushIntFromVar(baseVar)
    mainAssembler.pushIntFromVar(expVar)
    mainAssembler.callLocal(powerFuncIdx)
    mainAssembler.emitReturn()

    // Power function implementation
    val powerAssembler = builder.addFunction(signature)
    val oddLabel = powerAssembler.label()

    // pop arguments into local variables
    powerAssembler.popIntIntoVar(expVar)
    powerAssembler.popIntIntoVar(baseVar)

    // if (exp <= 0) return 1
    powerAssembler.pushIntFromVar(expVar)
    powerAssembler.pushImmediateInt(0)
    powerAssembler.callNative(comparison.int.le.int)
    powerAssembler.ifTrue {
      powerAssembler.pushImmediateInt(1)
      powerAssembler.emitReturn()
    }

    // val half = power(base, exp / 2)
    powerAssembler.pushIntFromVar(baseVar)
    powerAssembler.pushIntFromVar(expVar)
    powerAssembler.pushImmediateInt(2)
    powerAssembler.callNative(arithmetic.int.divide.int)
    powerAssembler.callLocal(powerFuncIdx)

    // stack: [half]
    powerAssembler.dup()
    // stack: [half, half]
    powerAssembler.callNative(arithmetic.int.multiply.int)
    // stack: [squared]

    // if (exp % 2 != 0) goto odd
    powerAssembler.pushIntFromVar(expVar)
    powerAssembler.pushImmediateInt(2)
    powerAssembler.callNative(arithmetic.int.modulo.int)
    powerAssembler.pushImmediateInt(0)
    powerAssembler.callNative(comparison.int.gt.int) // exp % 2 > 0
    powerAssembler.branchIf(oddLabel)

    // Even case: return squared
    powerAssembler.emitReturn()

    // Odd case: return base * squared
    oddLabel.bind()
    // stack: [squared]
    powerAssembler.pushIntFromVar(baseVar)
    // stack: [squared, base]
    powerAssembler.swap()
    // stack: [base, squared]
    powerAssembler.callNative(arithmetic.int.multiply.int)
    powerAssembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test cases: (base, exp) -> result
    val testCases = Seq(
      (2, 0) -> 1,
      (2, 1) -> 2,
      (2, 2) -> 4,
      (2, 3) -> 8,
      (2, 10) -> 1024,
      (3, 3) -> 27,
      (5, 3) -> 125,
      (10, 5) -> 100000,
    )

    testCases.foreach { case ((base, exp), expected) =>
      val initializer = Initializer { vs =>
        vs.unsafeWriteInt(baseVar, base)
        vs.unsafeWriteInt(expVar, exp)
      }
      val result = interpreter.run(emptyContextReader, initializer)
      result.intValue() shouldBe expected
    }
  }

  test("GCD (recursive Euclidean algorithm)") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Int, signature)

    val gcdFuncIdx = 1
    val aVar = 0
    val bVar = 1

    // Main function: calls gcd(a, b)
    val mainAssembler = builder.mainAssembler()
    mainAssembler.pushIntFromVar(aVar)
    mainAssembler.pushIntFromVar(bVar)
    mainAssembler.callLocal(gcdFuncIdx)
    mainAssembler.emitReturn()

    // GCD function implementation
    val gcdAssembler = builder.addFunction(signature)

    // pop arguments
    gcdAssembler.popIntIntoVar(bVar)
    gcdAssembler.popIntIntoVar(aVar)

    // if (b == 0) return a
    gcdAssembler.pushIntFromVar(bVar)
    gcdAssembler.pushImmediateInt(0)
    gcdAssembler.callNative(equality.int.eq.int)
    gcdAssembler.ifTrue {
      gcdAssembler.pushIntFromVar(aVar)
      gcdAssembler.emitReturn()
    }

    // return gcd(b, a % b)
    gcdAssembler.pushIntFromVar(aVar)
    gcdAssembler.pushIntFromVar(bVar)
    gcdAssembler.callNative(arithmetic.int.modulo.int) // stack: [a % b]
    gcdAssembler.pushIntFromVar(bVar) // stack: [a % b, b]
    gcdAssembler.swap() // stack: [b, a % b]
    gcdAssembler.callLocal(gcdFuncIdx)
    gcdAssembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test Cases: (a, b) -> result
    val testCases = Seq(
      (48, 18) -> 6,
      (101, 103) -> 1,
      (56, 42) -> 14,
      (7, 0) -> 7,
      (0, 5) -> 5
    )

    testCases.foreach { case ((a, b), expected) =>
      val result = interpreter.run(emptyContextReader, Initializer { vs =>
        vs.unsafeWriteInt(aVar, a)
        vs.unsafeWriteInt(bVar, b)
      })
      result.intValue() shouldBe expected
    }
  }

  test("Quadratic formula stress test (Double)") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.DoubleT, CoreTypes.DoubleT, CoreTypes.DoubleT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Double, signature)
    val assembler = builder.mainAssembler()

    val aVar = 0
    val bVar = 1
    val cVar = 2

    // Goal: (-b + sqrt(b*b - 4*a*c)) / (2*a)

    // 1. Calculate discriminant: b*b - 4*a*c
    assembler.pushDoubleFromVar(bVar)
    assembler.pushDoubleFromVar(bVar)
    assembler.callNative(arithmetic.double.multiply.double) // b^2

    assembler.pushImmediateDouble(4.0)
    assembler.pushDoubleFromVar(aVar)
    assembler.callNative(arithmetic.double.multiply.double) // 4a
    assembler.pushDoubleFromVar(cVar)
    assembler.callNative(arithmetic.double.multiply.double) // 4ac

    assembler.callNative(arithmetic.double.subtract.double) // b^2 - 4ac

    // 2. sqrt(discriminant)
    assembler.callNative(math.sqrt)

    // 3. -b + sqrt(...)
    assembler.pushImmediateDouble(0.0)
    assembler.pushDoubleFromVar(bVar)
    assembler.callNative(arithmetic.double.subtract.double) // -b
    assembler.swap()
    assembler.callNative(arithmetic.double.add.double) // -b + sqrt(...)

    // 4. Divide by 2a
    assembler.pushImmediateDouble(2.0)
    assembler.pushDoubleFromVar(aVar)
    assembler.callNative(arithmetic.double.multiply.double) // 2a

    assembler.callNative(arithmetic.double.divide.double) // (-b + sqrt(...)) / 2a

    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test Case: x^2 - 5x + 6 = 0 -> roots are 2 and 3. Positive root from formula is 3.
    // a=1, b=-5, c=6
    val result = interpreter.run(emptyContextReader, Initializer { vs =>
      vs.unsafeWriteDouble(aVar, 1.0)
      vs.unsafeWriteDouble(bVar, -5.0)
      vs.unsafeWriteDouble(cVar, 6.0)
    })
    result.doubleValue() shouldBe 3.0 +- 1e-9
  }
}
