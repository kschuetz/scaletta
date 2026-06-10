package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, VarAddress, VarSpaceSignature}
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
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(43)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, nativeFunctions)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43
    }


    it("should perform basic arithmetic via a native call") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
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
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val addTwelveAssembler = builder.addFunction(VarSpaceSignature.empty)
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
      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
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
      val builder = ProgramBuilder.create(BasicTypes.Long, VarSpaceSignature.empty)
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

    it("should handle branching") {
      val builder = ProgramBuilder.create(BasicTypes.Int, VarSpaceSignature.empty)
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

    // TODO
    ignore("should handle nested local function calls with their own variables") {
      val mainFrame = FrameSignature.empty
      val mainSignature = VarSpaceSignature.of(mainFrame)
      val builder = ProgramBuilder.create(BasicTypes.Int, mainSignature)
      val mainAssembler = builder.mainAssembler()

      mainAssembler.pushImmediateInt(11)
      mainAssembler.callLocal(1)
      mainAssembler.emitReturn()

      val funcFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
      val funcSignature = VarSpaceSignature.of(funcFrame)
      val funcAssembler = builder.addFunction(funcSignature)
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
      val builder = ProgramBuilder.create(BasicTypes.Boolean, VarSpaceSignature.empty)
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
      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
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
      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
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
      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
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

    it("should correctly store variables using StoreConst, Store, and StoreWide") {
      // Index 17 (prime) for StoreConst (index < 256, value fits in 8 bits)
      // Index 257 (prime) for Store (index < 65536)
      // Index 65537 (prime) for StoreWide (index > 65535)

      // Creating a very large signature using fromSeq might be slow but it's safe.
      // 65538 Ints.
      val types = Seq.fill(65538)(CoreTypes.IntT)
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(types))

      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
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
      val builder = ProgramBuilder.create(BasicTypes.Int, signature)
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
  }
}
