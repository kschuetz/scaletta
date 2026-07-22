package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{CallTarget, FunctionImpl, NativeFunctionId, NativeStep}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class HigherOrderNativeSpec extends AnyFunSpec with Matchers {
  describe("Higher-order natives") {
    it("should handle NativeStep.Done with different types") {
      val tableBuilder = NativeFunctionTable.builder()

      val doneInt = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.higherOrder(_ => NativeStep.Done(43))))
      val doneLong = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Long, FunctionImpl.higherOrder(_ => NativeStep.Done(123L))))
      val doneBoolean = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Boolean, FunctionImpl.higherOrder(_ => NativeStep.Done(true))))
      val doneString = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Object, FunctionImpl.higherOrder(_ => NativeStep.Done("hello"))))

      val table = tableBuilder.result()

      def testDone(id: NativeFunctionId, expectedType: Byte, expectedValue: Any): Unit = {
        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, expectedType, 0))
        val assembler = builder.mainAssembler()
        assembler.callNative(id)
        assembler.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)
        val result = interpreter.run(emptyContextReader)

        expectedType match {
          case BasicTypes.Int => result.intValue() shouldBe expectedValue
          case BasicTypes.Long => result.longValue() shouldBe expectedValue
          case BasicTypes.Boolean => result.booleanValue() shouldBe expectedValue
          case BasicTypes.Object => result.value[AnyRef]() shouldBe expectedValue
        }
      }

      testDone(doneInt, BasicTypes.Int, 43)
      testDone(doneLong, BasicTypes.Long, 123L)
      testDone(doneBoolean, BasicTypes.Boolean, true)
      testDone(doneString, BasicTypes.Object, "hello")
    }

    it("should contract arguments before pushing result") {
      val tableBuilder = NativeFunctionTable.builder()
      val params = ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT)
      val addHO = tableBuilder.add(NativeFunction(params, BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val a = args.readInt(0)
        val b = args.readInt(1)
        NativeStep.Done(a + b)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.pushImmediateInt(11)
      assembler.pushImmediateInt(12)
      assembler.callNative(addHO)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 23
    }

    it("should handle single NativeStep.Call round-trip") {
      val tableBuilder = NativeFunctionTable.builder()

      // A native that takes a callback and calls it once
      val applyOnce = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val target = args.readObject(0).asInstanceOf[CallTarget]
        target.setArgument(0, 41)
        NativeStep.Call(target, result => NativeStep.Done(result.asInstanceOf[Int] + 2))
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))

      // A local function that returns its argument
      val identityAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      identityAssembler.pushIntFromVar(0)
      identityAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.makeClosure(identityAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(applyOnce)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 43
    }

    it("should handle multi-step NativeStep.Call sequences") {
      val tableBuilder = NativeFunctionTable.builder()
      val applyTwice = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val target = args.readObject(0).asInstanceOf[CallTarget]
        target.setArgument(0, 41)
        NativeStep.Call(target, res1 => {
          target.setArgument(0, res1.asInstanceOf[Int] + 2)
          NativeStep.Call(target, res2 => NativeStep.Done(res2.asInstanceOf[Int] + 3))
        })
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val identityAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      identityAssembler.pushIntFromVar(0)
      identityAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.makeClosure(identityAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(applyTwice)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 46 // 41 + 2 + 3
    }

    it("should handle map-like operation over a list") {
      val tableBuilder = NativeFunctionTable.builder()
      val intAddId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int, FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))

      val mapHO = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.AnyRefT), BasicTypes.Object, FunctionImpl.higherOrder { args =>
        val list = args.readObject(0).asInstanceOf[List[Int]]
        val target = args.readObject(1).asInstanceOf[CallTarget]

        def go(remaining: List[Int], acc: Vector[Int]): NativeStep = {
          remaining match {
            case Nil => NativeStep.Done(acc.toList)
            case head :: tail =>
              target.setArgument(0, head)
              NativeStep.Call(target, res => go(tail, acc :+ res.asInstanceOf[Int]))
          }
        }

        go(list, Vector.empty)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val doubleAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      doubleAssembler.pushIntFromVar(0)
      doubleAssembler.pushIntFromVar(0)
      doubleAssembler.callNative(intAddId)
      doubleAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.pushImmediateObject(List(1, 2, 3, 5, 7)) // prime numbers!
      mainAssembler.makeClosure(doubleAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(mapHO)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.value[List[Int]]() shouldBe List(2, 4, 6, 10, 14)
    }

    it("should handle filter-like operation over a list") {
      val tableBuilder = NativeFunctionTable.builder()
      val intModId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int, FunctionImpl.intResult(args => args.readInt(0) % args.readInt(1))))
      val intEqId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Boolean, FunctionImpl.booleanResult(args => args.readInt(0) == args.readInt(1))))

      val filterHO = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.AnyRefT), BasicTypes.Object, FunctionImpl.higherOrder { args =>
        val list = args.readObject(0).asInstanceOf[List[Int]]
        val target = args.readObject(1).asInstanceOf[CallTarget]

        def go(remaining: List[Int], acc: Vector[Int]): NativeStep = {
          remaining match {
            case Nil => NativeStep.Done(acc.toList)
            case head :: tail =>
              target.setArgument(0, head)
              NativeStep.Call(target, res => {
                val nextAcc = if (res.asInstanceOf[Boolean]) acc :+ head else acc
                go(tail, nextAcc)
              })
          }
        }

        go(list, Vector.empty)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val isEvenAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Boolean, 1))
      isEvenAssembler.pushIntFromVar(0)
      isEvenAssembler.pushImmediateInt(2)
      isEvenAssembler.callNative(intModId)
      isEvenAssembler.pushImmediateInt(0)
      isEvenAssembler.callNative(intEqId)
      isEvenAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.pushImmediateObject(List(1, 2, 3, 4, 5, 6))
      mainAssembler.makeClosure(isEvenAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(filterHO)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.value[List[Int]]() shouldBe List(2, 4, 6)
    }

    it("should handle foldLeft-like operation") {
      val tableBuilder = NativeFunctionTable.builder()
      val intAddId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int, FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))

      val foldHO = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.IntT, CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrder { args =>
        val list = args.readObject(0).asInstanceOf[List[Int]]
        val initial = args.readInt(1)
        val target = args.readObject(2).asInstanceOf[CallTarget]

        def go(remaining: List[Int], currentAcc: Int): NativeStep = {
          remaining match {
            case Nil => NativeStep.Done(currentAcc)
            case head :: tail =>
              target.setArgument(0, currentAcc)
              target.setArgument(1, head)
              NativeStep.Call(target, res => go(tail, res.asInstanceOf[Int]))
          }
        }

        go(list, initial)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val addAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int, BasicTypes.Int))), BasicTypes.Int, 2))
      addAssembler.pushIntFromVar(0)
      addAssembler.pushIntFromVar(1)
      addAssembler.callNative(intAddId)
      addAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.pushImmediateObject(List(1, 3, 7, 11))
      mainAssembler.pushImmediateInt(0)
      mainAssembler.makeClosure(addAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(foldHO)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(emptyContextReader)

      result.intValue() shouldBe 22
    }

    it("should throw IllegalArgumentException for type mismatch") {
      val tableBuilder = NativeFunctionTable.builder()
      val wrongType = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.higherOrder(_ => NativeStep.Done("not an int"))))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.callNative(wrongType)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      val ex = intercept[IllegalArgumentException] {
        interpreter.run(emptyContextReader)
      }
      ex.getMessage should include("NativeFunctionId(0)")
      ex.getMessage should include("Int")
    }
  }
}
