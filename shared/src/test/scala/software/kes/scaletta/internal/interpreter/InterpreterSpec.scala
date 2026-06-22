package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarAddress, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

import scala.collection.immutable.ArraySeq

class InterpreterSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create()
    .asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)

  import stdLib.arithmetic

  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  describe("Interpreter") {
    it("should execute a program that returns a constant integer") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(43)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43
    }

    it("should perform basic arithmetic via a native call") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateInt(11)
      assembler.pushImmediateInt(12)
      assembler.callNative(arithmetic.int.add.int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 23
    }

    it("should perform basic arithmetic via a local function call") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val addTwelveAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      addTwelveAssembler.pushImmediateInt(12)
      addTwelveAssembler.callNative(arithmetic.int.add.int)
      addTwelveAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 23
    }

    it("should handle local variables") {
      val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      val varIdx = 0
      assembler.pushImmediateInt(41)
      assembler.popIntIntoVar(varIdx)
      assembler.pushImmediateInt(0)
      assembler.emitReturn()

      val program = builder.build()
      program.mainFunction.frameSignature.intCount shouldBe 1

      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader)

      val vars = interpreter.readAllVariables()
      vars(0) shouldBe 41
    }

    it("should handle mixed-type arithmetic") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Long, 0))
      val assembler = builder.mainAssembler()

      val largeLong = Int.MaxValue.toLong + 1
      assembler.pushImmediateInt(11)
      assembler.pushImmediateLong(largeLong)
      assembler.callNative(arithmetic.int.add.long)
      assembler.emitReturn()

      val program = builder.build()
      program.constantPool.longs should not be empty

      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.longValue() shouldBe (11L + largeLong)
    }

    it("should handle conversions between primitive types") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateDouble(43.7)
      assembler.convert(BasicTypes.Int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43
    }

    it("should handle boxing conversions") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateInt(47)
      assembler.convert(BasicTypes.Object)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.value[java.lang.Integer]() shouldBe java.lang.Integer.valueOf(47)
    }

    it("should handle unboxing conversions") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Long, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateObject(java.lang.Long.valueOf(53L))
      assembler.convert(BasicTypes.Long)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.longValue() shouldBe 53L
    }

    it("should handle best-effort conversion from non-numeric objects") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateObject("not a number")
      assembler.convert(BasicTypes.Int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 0
    }

    it("should support a single parameter via the initializer") {
      val signature = VarSpaceSignature.of(FrameSignature.of(CoreTypes.IntT))
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushIntFromVar(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)

      val result = interpreter.run(emptyContextReader, (vars: VarSpace) => {
        // Offset 0 in VarSpace corresponds to stack offset 0 in IntStack (which is top).
        // Since FrameSignature.of(IntT) assigns offset 0 to the Int, this is correct.
        vars.unsafeWriteInt(0, 41)
      })

      result.intValue() shouldBe 41
    }

    it("should support multiple parameters of the same type via the initializer") {
      val signature = VarSpaceSignature.of(FrameSignature.of(CoreTypes.LongT, CoreTypes.LongT))
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Long, 0))
      val assembler = builder.mainAssembler()

      // var 0: Long at stack offset 0 (assigned first by FrameSignature)
      // var 1: Long at stack offset 1
      assembler.pushLongFromVar(0)
      assembler.pushLongFromVar(1)
      assembler.callNative(arithmetic.long.add.long)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)

      val result = interpreter.run(emptyContextReader, (vars: VarSpace) => {
        // stack.unsafeWrite(0, v) is TOP.
        // If size is 2, elements(1) is top (offset 0), elements(0) is below top (offset 1).
        // var 0 is offset 0 (TOP), var 1 is offset 1 (BOTTOM).
        vars.unsafeWriteLong(0, 43L)
        vars.unsafeWriteLong(1, 47L)
      })

      result.longValue() shouldBe 90L
    }

    it("should support diverse mixed types via the initializer") {
      val signature = VarSpaceSignature.of(FrameSignature.of(CoreTypes.IntT, CoreTypes.LongT, CoreTypes.StringT))
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Object, 0))
      val assembler = builder.mainAssembler()

      // Int: offset 0
      // Long: offset 0
      // String: offset 0
      assembler.pushObjectFromVar(2)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)

      val testString = "Scaletta"
      val result = interpreter.run(emptyContextReader, (vars: VarSpace) => {
        vars.unsafeWriteInt(0, 41)
        vars.unsafeWriteLong(1, 43L)
        vars.unsafeWriteObject(2, testString)
      })

      result.value[String]() shouldBe testString
    }

    it("should support parameters spanning multiple frames via the initializer") {
      val frame = FrameSignature.of(CoreTypes.IntT, CoreTypes.IntT)
      val signature = VarSpaceSignature.of(frame)

      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushIntFromVar(0)
      assembler.pushIntFromVar(1)
      assembler.callNative(arithmetic.int.add.int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)

      val result = interpreter.run(emptyContextReader, (vars: VarSpace) => {
        vars.unsafeWriteInt(0, 13)
        vars.unsafeWriteInt(1, 17)
      })

      result.intValue() shouldBe 30
    }

    it("should allow running a specific function by index") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val mainAssembler = builder.mainAssembler()
      mainAssembler.pushImmediateInt(1)
      mainAssembler.emitReturn()

      val otherFunctionAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      otherFunctionAssembler.pushImmediateInt(47)
      otherFunctionAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)

      val result0 = interpreter.run(emptyContextReader, initialUserFunctionIndex = 0)
      result0.intValue() shouldBe 1

      val result1 = interpreter.run(emptyContextReader, initialUserFunctionIndex = 1)
      result1.intValue() shouldBe 47
    }

    it("should handle branching") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      val elseLabel = assembler.label()
      val exitLabel = assembler.label()

      assembler.pushImmediateInt(1) // condition (true)
      assembler.branchUnless(elseLabel)
      assembler.pushImmediateInt(41)
      assembler.branch(exitLabel)
      elseLabel.bind()
      assembler.pushImmediateInt(43)
      exitLabel.bind()
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 41
    }

    it("should handle nested local function calls with their own variables") {
      val mainFrame = FrameSignature.empty
      val mainSignature = VarSpaceSignature.of(mainFrame)
      val builder = ProgramBuilder.create(UserFunctionSignature(mainSignature, BasicTypes.Int, 0))
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val funcFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
      val funcSignature = VarSpaceSignature.of(funcFrame)
      val funcAssembler = builder.addFunction(UserFunctionSignature(funcSignature, BasicTypes.Int, 0))
      // argument (11) is on operand stack
      funcAssembler.popIntIntoVar(0)
      funcAssembler.pushIntFromVar(0)
      funcAssembler.pushImmediateInt(30)
      // arguments for arithmetic.int.add.int are (11, 30)
      // ArithmeticOps.add.intInt expects (args(0), args(1))
      // Since ParamsSignature(Int, Int) will map args(0) to stackOffset 1 and args(1) to stackOffset 0
      // when stack is [..., 11, 30] (top is 30)
      funcAssembler.callNative(arithmetic.int.add.int)
      funcAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 41
    }

    it("should handle all basic types via Push") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Boolean, 0))
      val assembler = builder.mainAssembler()

      // We'll test push for types that use constant pool in pushImmediate
      assembler.pushImmediateLong(1234567890123L)
      assembler.pop()
      assembler.pushImmediateDouble(1.23456789)
      assembler.pop()
      assembler.pushImmediateFloat(1.23f)
      assembler.pop()
      assembler.pushImmediateObject("test")
      assembler.pop()
      assembler.pushImmediateBoolean(true)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.booleanValue() shouldBe true
    }

    it("should correctly store variables using PopIntoVar") {
      val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.LongT, CoreTypes.AnyRefT))
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateInt(43)
      assembler.popIntIntoVar(0)
      assembler.pushImmediateLong(1234567890123L)
      assembler.popLongIntoVar(1)
      assembler.pushImmediateObject("test")
      assembler.popObjectIntoVar(2)
      assembler.pushImmediateInt(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader)

      val vars = interpreter.readAllVariables()
      vars(0) shouldBe 43
      vars(1) shouldBe 1234567890123L
      vars(2) shouldBe "test"
    }

    it("should correctly store variables using PopIntoVarWide") {
      val largeFrame = FrameSignature.unsafeCreate(0, 0, 65537, 0, 0, 0, 0, 0, 0)
      val slots = ArraySeq.fill(65537)(VarAddress.encode(BasicTypes.Int, 0))
      // Actually we need different offsets for each slot if we want it to be a valid signature,
      // but for just testing PopIntoVar we only need the slot at 65536 to be valid.
      val updatedSlots = slots.updated(65536, VarAddress.encode(BasicTypes.Int, 65536))

      val signature = VarSpaceSignature.create(updatedSlots, largeFrame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateInt(47)
      assembler.popIntIntoVar(65536)
      assembler.pushImmediateInt(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader)

      val vars = interpreter.readAllVariables()
      vars.length should be > 65536
      vars(65536) shouldBe 47
    }

    it("should handle all basic types in variable storage") {
      val frame = FrameSignature.fromSeq(Seq(
        CoreTypes.BooleanT, CoreTypes.ByteT, CoreTypes.CharT, CoreTypes.ShortT,
        CoreTypes.IntT, CoreTypes.LongT, CoreTypes.FloatT, CoreTypes.DoubleT, CoreTypes.AnyRefT
      ))
      val signature = VarSpaceSignature.of(frame)
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.pushImmediateBoolean(true)
      assembler.popBooleanIntoVar(0)
      assembler.pushImmediateByte(13.toByte)
      assembler.popByteIntoVar(1)
      assembler.pushImmediateChar('A')
      assembler.popCharIntoVar(2)
      assembler.pushImmediateShort(101.toShort)
      assembler.popShortIntoVar(3)
      assembler.pushImmediateInt(43)
      assembler.popIntIntoVar(4)
      assembler.pushImmediateLong(1234567890123L)
      assembler.popLongIntoVar(5)
      assembler.pushImmediateFloat(3.14f)
      assembler.popFloatIntoVar(6)
      assembler.pushImmediateDouble(2.71828)
      assembler.popDoubleIntoVar(7)
      assembler.pushImmediateObject("Scaletta")
      assembler.popObjectIntoVar(8)

      assembler.pushImmediateInt(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader)

      val vars = interpreter.readAllVariables()
      vars(0) shouldBe true
      vars(1) shouldBe 13.toByte
      vars(2) shouldBe 'A'
      vars(3) shouldBe 101.toShort
      vars(4) shouldBe 43
      vars(5) shouldBe 1234567890123L
      vars(6) shouldBe 3.14f
      vars(7) shouldBe 2.71828
      vars(8) shouldBe "Scaletta"
    }

    describe("TailCallLocal") {
      it("should support tail recursion (self-call)") {
        val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
        val signature = VarSpaceSignature.of(frame)
        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))

        // Function 0: Main entry point
        val mainAssembler = builder.mainAssembler()
        mainAssembler.pushImmediateInt(0) // initial acc
        mainAssembler.pushImmediateInt(5) // initial n
        mainAssembler.callLocal(1)
        mainAssembler.emitReturn()

        // Function 1: Recursive sum
        val recAssembler = builder.addFunction(UserFunctionSignature(signature, BasicTypes.Int, 0))
        val nVar = 0
        val accVar = 1

        recAssembler.popIntIntoVar(nVar)
        recAssembler.popIntIntoVar(accVar)

        // if (n == 0) return acc
        recAssembler.pushIntFromVar(nVar)
        recAssembler.pushImmediateInt(0)
        recAssembler.callNative(stdLib.equality.int.eq.int)
        recAssembler.ifTrue {
          recAssembler.pushIntFromVar(accVar)
          recAssembler.emitReturn()
        }

        // return tailCall(acc + n, n - 1)
        recAssembler.pushIntFromVar(accVar)
        recAssembler.pushIntFromVar(nVar)
        recAssembler.callNative(arithmetic.int.add.int) // new acc
        recAssembler.pushIntFromVar(nVar)
        recAssembler.pushImmediateInt(1)
        recAssembler.callNative(arithmetic.int.subtract.int) // new n
        // Stack: [new acc, new n]
        recAssembler.tailCallLocal(1) // self-recursion

        val program = builder.build()
        val interpreter = Interpreter.create(program, nativeFunctions)

        // Sum of numbers from 1 to 5 = 15
        val result = interpreter.run(emptyContextReader)

        result.intValue() shouldBe 15
      }

      it("should support tail calling another function") {
        val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
        val signature = VarSpaceSignature.of(frame)
        val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))

        // Function 0 (main): tail calls function 1
        val mainAssembler = builder.mainAssembler()
        mainAssembler.pushImmediateInt(43)
        mainAssembler.tailCallLocal(1)

        // Function 1: returns the incremented value
        val otherAssembler = builder.addFunction(UserFunctionSignature(signature, BasicTypes.Int, 0))
        otherAssembler.popIntIntoVar(0)
        otherAssembler.pushIntFromVar(0)
        otherAssembler.pushImmediateInt(1)
        otherAssembler.callNative(arithmetic.int.add.int)
        otherAssembler.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, nativeFunctions)
        val result = interpreter.run(emptyContextReader)

        result.intValue() shouldBe 44
      }

      it("should maintain constant stack space for deep recursion") {
        val frame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
        val signature = VarSpaceSignature.of(frame)
        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))

        // Function 0: Main entry point
        val mainAssembler = builder.mainAssembler()
        mainAssembler.pushImmediateInt(1000)
        mainAssembler.callLocal(1)
        mainAssembler.emitReturn()

        // Function 1: Recursive function
        val recAssembler = builder.addFunction(UserFunctionSignature(signature, BasicTypes.Int, 0))
        // Pop n from stack
        recAssembler.popIntIntoVar(0)

        // if (n == 0) return 0
        recAssembler.pushIntFromVar(0)
        recAssembler.pushImmediateInt(0)
        recAssembler.callNative(stdLib.equality.int.eq.int)
        recAssembler.ifTrue {
          recAssembler.pushImmediateInt(0)
          recAssembler.emitReturn()
        }

        // tailCall(n - 1)
        recAssembler.pushIntFromVar(0)
        recAssembler.pushImmediateInt(1)
        recAssembler.callNative(arithmetic.int.subtract.int)
        recAssembler.tailCallLocal(1)

        val program = builder.build()
        val interpreter = Interpreter.create(program, nativeFunctions)

        // Run with a large number of iterations to ensure no stack overflow
        val result = interpreter.run(emptyContextReader)

        result.intValue() shouldBe 0
      }
    }

    it("should correctly store variables using StoreConst, Store, and StoreWide") {
      // Index 17 (prime) for StoreConst (index < 256, value fits in 8 bits)
      // Index 257 (prime) for Store (index < 65536)
      // Index 65537 (prime) for StoreWide (index > 65535)

      // Creating a very large signature using fromSeq might be slow but it's safe.
      // 65538 Ints.
      val types = Seq.fill(65538)(CoreTypes.IntT)
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(types))

      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      // StoreConst: var 17, value 43 (prime)
      assembler.storeImmediateInt(17, 43)

      // Store: var 257, value 41 (prime)
      assembler.storeImmediateInt(257, 41)

      // StoreWide: var 65537, value 47 (prime)
      assembler.storeImmediateInt(65537, 47)

      assembler.pushImmediateInt(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader)

      val vars = interpreter.readAllVariables()
      vars(17) shouldBe 43
      vars(257) shouldBe 41
      vars(65537) shouldBe 47
    }

    it("should correctly store non-primitive types using StoreConst and Store") {
      val types = Seq.fill(258)(CoreTypes.AnyRefT)
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(types))
      val builder = ProgramBuilder.create(UserFunctionSignature(signature, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()

      assembler.storeImmediateObject(17, "SmallIndexObject")
      assembler.storeImmediateObject(257, "LargeIndexObject")

      assembler.pushImmediateInt(0)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.run(emptyContextReader)

      val vars = interpreter.readAllVariables()
      vars(17) shouldBe "SmallIndexObject"
      vars(257) shouldBe "LargeIndexObject"
    }

    it("should allow stepping through instructions") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(41)
      assembler.pushImmediateInt(2)
      assembler.callNative(arithmetic.int.add.int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.initialize(emptyContextReader)

      interpreter.isDone shouldBe false

      // Step 1: push 41
      interpreter.step() shouldBe true
      interpreter.isDone shouldBe false

      // Step 2: push 2
      interpreter.step() shouldBe true
      interpreter.isDone shouldBe false

      // Step 3: call native add
      interpreter.step() shouldBe true
      interpreter.isDone shouldBe false

      // Step 4: return
      interpreter.step() shouldBe false
      interpreter.isDone shouldBe true

      interpreter.getResult.intValue() shouldBe 43
    }

    it("should allow stepping with a batch size") {
      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(41)
      assembler.pushImmediateInt(2)
      assembler.callNative(arithmetic.int.add.int)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      interpreter.initialize(emptyContextReader)

      // Step 2 instructions at once
      interpreter.step(2) shouldBe true
      interpreter.isDone shouldBe false

      // Step until completion
      interpreter.runUntilDone()
      interpreter.isDone shouldBe true

      interpreter.getResult.intValue() shouldBe 43
    }
  }
}
