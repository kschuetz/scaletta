package software.kes.scaletta.internal.interpreter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Double, 0))
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Boolean, 0))
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Boolean, 0))
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))

    val powerFuncIdx = 1
    val baseVar = 0
    val expVar = 1

    // Main function: just calls power(base, exp)
    val mainAssembler = builder.mainAssembler()
    mainAssembler.pushIntFromVar(baseVar)
    mainAssembler.pushIntFromVar(expVar)
    mainAssembler.tailCallLocal(powerFuncIdx)

    // Power function implementation
    val powerAssembler = builder.addFunction(UserFunctionSignature(signature, BasicTypes.Int, 0))
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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))

    val gcdFuncIdx = 1
    val aVar = 0
    val bVar = 1

    // Main function: calls gcd(a, b)
    val mainAssembler = builder.mainAssembler()
    mainAssembler.pushIntFromVar(aVar)
    mainAssembler.pushIntFromVar(bVar)
    mainAssembler.tailCallLocal(gcdFuncIdx)

    // GCD function implementation
    val gcdAssembler = builder.addFunction(UserFunctionSignature(signature, BasicTypes.Int, 0))

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
    gcdAssembler.tailCallLocal(gcdFuncIdx)

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
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Double, 0))
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

  test("Ackermann function A(m, n) stress test") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))

    val ackFuncIdx = 1
    val mVar = 0
    val nVar = 1

    // Main function: calls ack(m, n)
    val mainAssembler = builder.mainAssembler()
    mainAssembler.pushIntFromVar(mVar)
    mainAssembler.pushIntFromVar(nVar)
    mainAssembler.tailCallLocal(ackFuncIdx)

    // Ackermann function implementation
    val ackAssembler = builder.addFunction(UserFunctionSignature(signature, BasicTypes.Int, 0))

    // pop arguments
    ackAssembler.popIntIntoVar(nVar)
    ackAssembler.popIntIntoVar(mVar)

    // if (m == 0) return n + 1
    ackAssembler.pushIntFromVar(mVar)
    ackAssembler.pushImmediateInt(0)
    ackAssembler.callNative(equality.int.eq.int)
    ackAssembler.ifTrue {
      ackAssembler.pushIntFromVar(nVar)
      ackAssembler.pushImmediateInt(1)
      ackAssembler.callNative(arithmetic.int.add.int)
      ackAssembler.emitReturn()
    }

    // if (n == 0) return A(m - 1, 1)
    ackAssembler.pushIntFromVar(nVar)
    ackAssembler.pushImmediateInt(0)
    ackAssembler.callNative(equality.int.eq.int)
    ackAssembler.ifTrue {
      ackAssembler.pushIntFromVar(mVar)
      ackAssembler.pushImmediateInt(1)
      ackAssembler.callNative(arithmetic.int.subtract.int)
      ackAssembler.pushImmediateInt(1)
      ackAssembler.tailCallLocal(ackFuncIdx)
    }

    // default: return A(m - 1, A(m, n - 1))
    // Outer call args: (m - 1, ...)
    ackAssembler.pushIntFromVar(mVar)
    ackAssembler.pushImmediateInt(1)
    ackAssembler.callNative(arithmetic.int.subtract.int)

    // Inner call args: A(m, n - 1)
    ackAssembler.pushIntFromVar(mVar)
    ackAssembler.pushIntFromVar(nVar)
    ackAssembler.pushImmediateInt(1)
    ackAssembler.callNative(arithmetic.int.subtract.int)
    ackAssembler.callLocal(ackFuncIdx)

    // Final call
    ackAssembler.tailCallLocal(ackFuncIdx)

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Test Cases: (m, n) -> result
    val testCases = Seq(
      (0, 5) -> 6,
      (1, 0) -> 2,
      (1, 2) -> 4,
      (2, 2) -> 7,
      (3, 2) -> 29,
      (3, 3) -> 61
    )

    testCases.foreach { case ((m, n), expected) =>
      val result = interpreter.run(emptyContextReader, Initializer { vs =>
        vs.unsafeWriteInt(mVar, m)
        vs.unsafeWriteInt(nVar, n)
      })
      result.intValue() shouldBe expected
    }
  }

  test("financial precision: multi-currency interest calculator (mixed-type stress test)") {
    // 1. Define Frame Signature with all 8 primitive types to stress VarSpace layout
    val frame = FrameSignature.fromSeq(Seq(
      CoreTypes.LongT, // principal
      CoreTypes.DoubleT, // rate
      CoreTypes.FloatT, // multiplier
      CoreTypes.IntT, // term
      CoreTypes.BooleanT, // taxable
      CoreTypes.ByteT, // taxRate (narrow type)
      CoreTypes.CharT, // currencyCode
      CoreTypes.ShortT // periodCounter
    ))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Long, 0))
    val assembler = builder.mainAssembler()

    // Variable indices
    val principalVar = 0
    val rateVar = 1
    val multiplierVar = 2
    val termVar = 3
    val taxableVar = 4
    val taxRateVar = 5
    val currencyCodeVar = 6
    val periodCounterVar = 7

    val loopStart = assembler.label()
    val loopExit = assembler.label()

    // 2. Logic Implementation

    // Initialize periodCounter = 0
    assembler.pushImmediateShort(0)
    assembler.popShortIntoVar(periodCounterVar)

    // loopStart:
    loopStart.bind()

    // if (periodCounter >= term) goto loopExit
    assembler.pushShortFromVar(periodCounterVar)
    assembler.convert(BasicTypes.Int)
    assembler.pushIntFromVar(termVar)
    assembler.callNative(comparison.int.ge.int)
    assembler.branchIf(loopExit)

    // Calculation step: principal = (principal * rate) * multiplier

    // Convert principal (Long) to Double
    assembler.pushLongFromVar(principalVar)
    assembler.convert(BasicTypes.Double)

    // Multiply by rate (Double)
    assembler.pushDoubleFromVar(rateVar)
    assembler.callNative(arithmetic.double.multiply.double)

    // Convert intermediate result to Float
    assembler.convert(BasicTypes.Float)

    // Multiply by multiplier (Float)
    assembler.pushFloatFromVar(multiplierVar)
    assembler.callNative(arithmetic.float.multiply.float)

    // Apply tax if taxable is true
    assembler.pushBooleanFromVar(taxableVar)
    assembler.ifTrue {
      // result = result * (1.0 - taxRate / 100.0)
      // simplified: result = result * 0.95 (if taxRate is 5)
      assembler.pushImmediateFloat(0.95f)
      assembler.callNative(arithmetic.float.multiply.float)
    }

    // Convert final result back to Long and store in principal
    assembler.convert(BasicTypes.Long)
    assembler.popLongIntoVar(principalVar)

    // Increment periodCounter (Short)
    assembler.pushShortFromVar(periodCounterVar)
    assembler.pushImmediateShort(1.toShort)
    assembler.callNative(arithmetic.short.add.short)
    assembler.convert(BasicTypes.Short)
    assembler.popShortIntoVar(periodCounterVar)

    // Loop back
    assembler.branch(loopStart)

    // loopExit:
    loopExit.bind()

    // Simple currency check (verify Char handling)
    // if (currencyCode == 'U') { /* dummy check */ }
    assembler.pushCharFromVar(currencyCodeVar)
    assembler.pushImmediateChar('U')
    assembler.callNative(equality.char.eq.char)
    assembler.pop() // Just consume the result

    // Return final principal
    assembler.pushLongFromVar(principalVar)
    assembler.emitReturn()

    // 3. Execution & Verification
    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    val inputPrincipal = 1000000L // 1,000,000 cents
    val inputRate = 1.05 // 5% growth
    val inputMultiplier = 1.1f // 10% bonus
    val inputTerm = 3 // 3 periods
    val inputTaxable = true
    val inputTaxRate: Byte = 5
    val inputCurrency: Char = 'U'

    val initializer = Initializer { vs =>
      vs.unsafeWriteLong(principalVar, inputPrincipal)
      vs.unsafeWriteDouble(rateVar, inputRate)
      vs.unsafeWriteFloat(multiplierVar, inputMultiplier)
      vs.unsafeWriteInt(termVar, inputTerm)
      vs.unsafeWriteBoolean(taxableVar, inputTaxable)
      vs.unsafeWriteByte(taxRateVar, inputTaxRate)
      vs.unsafeWriteChar(currencyCodeVar, inputCurrency)
    }

    // Expected value calculation in Scala
    var expected = inputPrincipal.toDouble
    for (_ <- 0 until inputTerm) {
      val intermediate = (expected * inputRate).toFloat
      var result = intermediate * inputMultiplier
      if (inputTaxable) {
        result = result * 0.95f
      }
      expected = result.toLong.toDouble
    }
    val finalExpected = expected.toLong

    val result = interpreter.run(emptyContextReader, initializer)
    result.longValue() shouldBe finalExpected
  }

  test("mutual recursion (isEven/isOdd)") {
    val mainFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
    val mainSignature = VarSpaceSignature.of(mainFrame)
    val recursiveFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
    val recursiveSignature = VarSpaceSignature.of(recursiveFrame)

    val builder = ProgramBuilder.create(UserFunctionSignature(mainSignature, BasicTypes.Boolean, 1))

    // Function 0 (Main)
    val mainAssembler = builder.mainAssembler()
    mainAssembler.pushIntFromVar(0)
    mainAssembler.callLocal(1) // Call isEven
    mainAssembler.emitReturn()

    // Function 1 (isEven)
    val isEvenAssembler = builder.addFunction(UserFunctionSignature(recursiveSignature, BasicTypes.Boolean, 1))
    val evenExit = isEvenAssembler.label()
    isEvenAssembler.pushIntFromVar(0)
    isEvenAssembler.pushImmediateInt(0)
    isEvenAssembler.callNative(equality.int.eq.int)
    isEvenAssembler.branchUnless(evenExit)
    isEvenAssembler.pushImmediateBoolean(true)
    isEvenAssembler.emitReturn()
    evenExit.bind()
    isEvenAssembler.pushIntFromVar(0)
    isEvenAssembler.pushImmediateInt(1)
    isEvenAssembler.callNative(arithmetic.int.subtract.int)
    isEvenAssembler.tailCallLocal(2) // Tail call isOdd

    // Function 2 (isOdd)
    val isOddAssembler = builder.addFunction(UserFunctionSignature(recursiveSignature, BasicTypes.Boolean, 1))
    val oddExit = isOddAssembler.label()
    isOddAssembler.pushIntFromVar(0)
    isOddAssembler.pushImmediateInt(0)
    isOddAssembler.callNative(equality.int.eq.int)
    isOddAssembler.branchUnless(oddExit)
    isOddAssembler.pushImmediateBoolean(false)
    isOddAssembler.emitReturn()
    oddExit.bind()
    isOddAssembler.pushIntFromVar(0)
    isOddAssembler.pushImmediateInt(1)
    isOddAssembler.callNative(arithmetic.int.subtract.int)
    isOddAssembler.tailCallLocal(1) // Tail call isEven

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    val testCases = Seq(
      0 -> true,
      1 -> false,
      2 -> true,
      3 -> false,
      41 -> false, // Prime number test
      1000 -> true // Stress test for TailCallLocal
    )

    testCases.foreach { case (n, expected) =>
      val initializer = Initializer { vs =>
        vs.unsafeWriteInt(0, n)
      }
      val result = interpreter.run(emptyContextReader, initializer)
      result.booleanValue() shouldBe expected
    }
  }

  test("wide variable and constant pool access stress test") {
    val varCount = 70000
    val constantCount = 70000

    // 1. Frame Setup: Create 70,000 Int slots
    val frame = FrameSignature.fromSeq(Seq.fill(varCount)(CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Long, 0))
    val assembler = builder.mainAssembler()

    // 2. Constant Loading: Intern 70,000 unique Long values to stress the ConstantPool
    // We use a loop to ensure we reach indices > 65535.
    // The last constant will be at index 69,999.
    (0 until constantCount).foreach { i =>
      assembler.pushImmediateLong(1000000L + i)
      assembler.pop() // Just to intern them, we'll push the one we need later
    }

    val nVarLow = 10
    val nVarHigh = 68000
    val constantToUse = 1000000L + 69999L // The last interned constant

    // 3. Logic:
    // a. Retrieve value from index 10 (stored via Initializer)
    assembler.pushIntFromVar(nVarLow)

    // b. Retrieve value from index 68,000 (stored via Initializer) - Exercises PushFromVarWide
    assembler.pushIntFromVar(nVarHigh)

    // c. Add them
    assembler.callNative(arithmetic.int.add.int)

    // d. Convert sum to Long
    assembler.convert(BasicTypes.Long)

    // e. Push a Long constant from a wide pool index - Exercises Push with 32-bit index
    assembler.pushImmediateLong(constantToUse)

    // f. Add the wide Long constant
    assembler.callNative(arithmetic.long.add.long)

    // g. Return result
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // 4. Verification:
    // Store known prime values at the boundaries
    val valLow = 41
    val valHigh = 43
    val expected = (valLow + valHigh).toLong + constantToUse

    val initializer = Initializer { vs =>
      vs.unsafeWriteInt(nVarLow, valLow)
      vs.unsafeWriteInt(nVarHigh, valHigh)
    }

    val result = interpreter.run(emptyContextReader, initializer)
    result.longValue() shouldBe expected
  }

  test("complex stack reorganization (RPN evaluator)") {
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
    val assembler = builder.mainAssembler()

    // Variable layout:
    // var 0: a
    // var 1: b
    val aVar = 0
    val bVar = 1

    // Evaluate (a + b) * (a - b) using RPN-style stack manipulation
    assembler.pushIntFromVar(aVar) // [a]
    assembler.dup() // [a, a]
    assembler.pushIntFromVar(bVar) // [a, a, b]
    assembler.callNative(arithmetic.int.add.int) // [a, a + b]
    assembler.swap() // [a + b, a]
    assembler.pushIntFromVar(bVar) // [a + b, a, b]
    assembler.callNative(arithmetic.int.subtract.int) // [a + b, a - b]
    assembler.callNative(arithmetic.int.multiply.int) // [(a + b) * (a - b)]
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    val a = 43
    val b = 41
    val expected = (a + b) * (a - b)

    val initializer = Initializer { vs =>
      vs.unsafeWriteInt(aVar, a)
      vs.unsafeWriteInt(bVar, b)
    }

    val result = interpreter.run(emptyContextReader, initializer)
    result.intValue() shouldBe expected
  }
}
