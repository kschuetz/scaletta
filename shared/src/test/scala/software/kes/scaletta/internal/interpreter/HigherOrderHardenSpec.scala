package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class HigherOrderHardenSpec extends AnyFunSpec with Matchers {
  val ctxId1 = RuntimeContextId(1)

  final class MapContextReader(contexts: Map[Int, Any]) extends RuntimeContextReader {
    def readRuntimeContext[A](id: RuntimeContextId): A =
      contexts.get(id.value).map(_.asInstanceOf[A]).getOrElse(throw new NoSuchElementException(s"Context $id not found"))
  }

  describe("Higher-order hardening") {
    describe("1. Synthetic 'Done-only' HOFs") {
      it("should return Done for all primitive types and AnyRef") {
        val tableBuilder = NativeFunctionTable.builder()

        val hofDoneInt = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int,
          FunctionImpl.higherOrder(_ => NativeStep.done(41))))
        val hofDoneLong = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Long,
          FunctionImpl.higherOrder(_ => NativeStep.done(43L))))
        val hofDoneFloat = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Float,
          FunctionImpl.higherOrder(_ => NativeStep.done(47.0f))))
        val hofDoneDouble = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Double,
          FunctionImpl.higherOrder(_ => NativeStep.done(53.0))))
        val hofDoneBoolean = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Boolean,
          FunctionImpl.higherOrder(_ => NativeStep.done(true))))
        val hofDoneByte = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Byte,
          FunctionImpl.higherOrder(_ => NativeStep.done(59.toByte))))
        val hofDoneShort = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Short,
          FunctionImpl.higherOrder(_ => NativeStep.done(61.toShort))))
        val hofDoneChar = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Char,
          FunctionImpl.higherOrder(_ => NativeStep.done('A'))))
        val hofDoneObject = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Object,
          FunctionImpl.higherOrder(_ => NativeStep.done("hello"))))
        val hofDoneNull = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Object,
          FunctionImpl.higherOrder(_ => NativeStep.done(null))))

        val table = tableBuilder.result()

        def testDone(id: software.kes.scaletta.api.NativeFunctionId, expectedType: Byte, expectedValue: Any): Unit = {
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
            case BasicTypes.Float => result.floatValue() shouldBe expectedValue
            case BasicTypes.Double => result.doubleValue() shouldBe expectedValue
            case BasicTypes.Boolean => result.booleanValue() shouldBe expectedValue
            case BasicTypes.Byte => result.byteValue() shouldBe expectedValue
            case BasicTypes.Short => result.shortValue() shouldBe expectedValue
            case BasicTypes.Char => result.charValue() shouldBe expectedValue
            case BasicTypes.Object => result.value[AnyRef]() shouldBe expectedValue
          }
        }

        testDone(hofDoneInt, BasicTypes.Int, 41)
        testDone(hofDoneLong, BasicTypes.Long, 43L)
        testDone(hofDoneFloat, BasicTypes.Float, 47.0f)
        testDone(hofDoneDouble, BasicTypes.Double, 53.0)
        testDone(hofDoneBoolean, BasicTypes.Boolean, true)
        testDone(hofDoneByte, BasicTypes.Byte, 59.toByte)
        testDone(hofDoneShort, BasicTypes.Short, 61.toShort)
        testDone(hofDoneChar, BasicTypes.Char, 'A')
        testDone(hofDoneObject, BasicTypes.Object, "hello")
        testDone(hofDoneNull, BasicTypes.Object, null)
      }
    }

    describe("2. Single Call HOFs") {
      it("should handle primitive callback (Int -> Int)") {
        val tableBuilder = NativeFunctionTable.builder()
        val hofCallOnce = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
          FunctionImpl.higherOrder { args =>
            val target = args.readObject(0).asInstanceOf[CallTarget]
            target.setArgument(0, 41)
            NativeStep.Call(target, result => NativeStep.done(result.asInstanceOf[Int] + 2))
          }))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
        val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
          BasicTypes.Int, 1))
        callback.pushIntFromVar(0)
        callback.pushImmediateInt(1)
        // add native
        val intAddId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int,
          FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
        callback.callNative(intAddId)
        callback.emitReturn()

        val main = builder.mainAssembler()
        main.makeClosure(callback.index, CapturePlan.empty)
        main.callNative(hofCallOnce)
        main.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, tableBuilder.result())
        val result = interpreter.run(emptyContextReader)

        result.intValue() shouldBe 44 // 41 + 1 (callback) + 2 (k)
      }

      it("should handle object callback (String -> String)") {
        val tableBuilder = NativeFunctionTable.builder()
        val hofCallOnce = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Object,
          FunctionImpl.higherOrder { args =>
            val target = args.readObject(0).asInstanceOf[CallTarget]
            target.setArgument(0, "world")
            NativeStep.Call(target, result => NativeStep.done(s"hello $result"))
          }))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
        val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Object))),
          BasicTypes.Object, 1))
        callback.pushObjectFromVar(0)
        callback.emitReturn()

        val main = builder.mainAssembler()
        main.makeClosure(callback.index, CapturePlan.empty)
        main.callNative(hofCallOnce)
        main.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)
        val result = interpreter.run(emptyContextReader)

        result.value[String]() shouldBe "hello world"
      }
    }

    describe("3. Repeated Call HOFs (loop)") {
      it("should handle N = 0, 1, 100") {
        val tableBuilder = NativeFunctionTable.builder()
        val hofLoop = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.AnyRefT), BasicTypes.Int,
          FunctionImpl.higherOrder { args =>
            val n = args.readInt(0)
            val target = args.readObject(1).asInstanceOf[CallTarget]

            def go(i: Int, acc: Int): NativeStep = {
              if (i < n) {
                target.setArgument(0, i)
                NativeStep.Call(target, res => go(i + 1, acc + res.asInstanceOf[Int]))
              } else {
                NativeStep.done(acc)
              }
            }

            go(0, 0)
          }))

        val table = tableBuilder.result()

        def testLoop(n: Int, expected: Int): Unit = {
          val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
          val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
            BasicTypes.Int, 1))
          callback.pushIntFromVar(0)
          callback.pushImmediateInt(2)
          val intMulId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int,
            FunctionImpl.intResult(args => args.readInt(0) * args.readInt(1))))
          callback.callNative(intMulId)
          callback.emitReturn()

          val main = builder.mainAssembler()
          main.pushImmediateInt(n)
          main.makeClosure(callback.index, CapturePlan.empty)
          main.callNative(hofLoop)
          main.emitReturn()

          val program = builder.build()
          val interpreter = Interpreter.create(program, tableBuilder.result())
          val result = interpreter.run(emptyContextReader)

          result.intValue() shouldBe expected
        }

        testLoop(0, 0)
        testLoop(1, 0) // 0 * 2 = 0
        testLoop(3, 6) // (0*2) + (1*2) + (2*2) = 0 + 2 + 4 = 6
        testLoop(100, 9900) // sum(0..99) * 2 = (99 * 100 / 2) * 2 = 9900
      }
    }

    describe("4. Context-aware HOFs") {
      it("should see context in body and k") {
        val tableBuilder = NativeFunctionTable.builder()
        val hofContext = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
          FunctionImpl.higherOrderWithContext { (ctx, args) =>
            val multiplier = ctx.readRuntimeContext[Int](ctxId1)
            val target = args.readObject(0).asInstanceOf[CallTarget]
            target.setArgument(0, 10)
            NativeStep.Call(target, res => {
              val extra = ctx.readRuntimeContext[Int](ctxId1)
              NativeStep.done(res.asInstanceOf[Int] * multiplier + extra)
            })
          }))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
        val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
          BasicTypes.Int, 1))
        callback.pushIntFromVar(0)
        callback.emitReturn()

        val main = builder.mainAssembler()
        main.makeClosure(callback.index, CapturePlan.empty)
        main.callNative(hofContext)
        main.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)
        // ctx1 = 5
        // input = 10
        // k: 10 * 5 + 5 = 55
        val result = interpreter.run(new MapContextReader(Map(1 -> 5)))

        result.intValue() shouldBe 55
      }
    }

    describe("5. Error paths") {
      it("should throw IndexOutOfBoundsException for CallTarget arity mismatch") {
        val tableBuilder = NativeFunctionTable.builder()
        val hofError = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
          FunctionImpl.higherOrder { args =>
            val target = args.readObject(0).asInstanceOf[CallTarget]
            target.setArgument(1, 41) // Out of bounds
            NativeStep.done(0)
          }))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
        val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
          BasicTypes.Int, 1))
        callback.pushIntFromVar(0)
        callback.emitReturn()

        val main = builder.mainAssembler()
        main.makeClosure(callback.index, CapturePlan.empty)
        main.callNative(hofError)
        main.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)

        intercept[IndexOutOfBoundsException] {
          interpreter.run(emptyContextReader)
        }
      }

      it("should throw IllegalArgumentException for callback return type mismatch") {
        val tableBuilder = NativeFunctionTable.builder()
        val hofError = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
          FunctionImpl.higherOrder { args =>
            val target = args.readObject(0).asInstanceOf[CallTarget]
            target.setArgument(0, 41)
            NativeStep.Call(target, res => NativeStep.done(res))
          }))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
        // Callback returns String, but HOF expects it to be compatible with Int (via k's use of res)
        // The type mismatch will happen in NativeResultPusher.pushReturn when k returns Done(String)
        // but the HOF return type is Int.
        val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
          BasicTypes.Object, 1))
        callback.pushImmediateObject("not an int")
        callback.emitReturn()

        val main = builder.mainAssembler()
        main.makeClosure(callback.index, CapturePlan.empty)
        main.callNative(hofError)
        main.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)

        val ex = intercept[IllegalArgumentException] {
          interpreter.run(emptyContextReader)
        }
        ex.getMessage should include("Type mismatch")
        ex.getMessage should include("Int")
        ex.getMessage should include("java.lang.String")
      }
    }

    describe("6. Nested HOF scenarios") {
      it("should handle nested HOF calls (map inside map)") {
        val tableBuilder = NativeFunctionTable.builder()
        val mapHO = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.AnyRefT), BasicTypes.Object,
          FunctionImpl.higherOrder { args =>
            val list = args.readObject(0).asInstanceOf[List[Any]]
            val target = args.readObject(1).asInstanceOf[CallTarget]

            def go(remaining: List[Any], acc: Vector[Any]): NativeStep = {
              remaining match {
                case Nil => NativeStep.done(acc.toList)
                case head :: tail =>
                  target.setArgument(0, head)
                  NativeStep.Call(target, res => go(tail, acc :+ res))
              }
            }

            go(list, Vector.empty)
          }))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))

        // Inner callback: doubles its argument
        val intAddId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int,
          FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
        val doubleAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
          BasicTypes.Int, 1))
        doubleAssembler.pushIntFromVar(0)
        doubleAssembler.pushIntFromVar(0)
        doubleAssembler.callNative(intAddId)
        doubleAssembler.emitReturn()

        // Outer callback: calls mapHO on the input list (which is expected to be a list of lists)
        val outerCallbackAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Object))),
          BasicTypes.Object, 1))
        outerCallbackAssembler.pushObjectFromVar(0)
        outerCallbackAssembler.makeClosure(doubleAssembler.index, CapturePlan.empty)
        outerCallbackAssembler.callNative(mapHO)
        outerCallbackAssembler.emitReturn()

        val mainAssembler = builder.mainAssembler()
        mainAssembler.pushImmediateObject(List(List(1, 2), List(3, 4)))
        mainAssembler.makeClosure(outerCallbackAssembler.index, CapturePlan.empty)
        mainAssembler.callNative(mapHO)
        mainAssembler.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, tableBuilder.result())
        val result = interpreter.run(emptyContextReader)

        result.value[List[List[Int]]]() shouldBe List(List(2, 4), List(6, 8))
      }
    }

    describe("7. Stack/Frame invariants") {
      it("should leave only the result on the operand stack") {
        val tableBuilder = NativeFunctionTable.builder()
        val hof = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int,
          FunctionImpl.higherOrder(_ => NativeStep.done(41))))
        val table = tableBuilder.result()

        val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
        val assembler = builder.mainAssembler()
        assembler.pushImmediateInt(100)
        assembler.pushImmediateInt(200)
        assembler.pop()
        assembler.pop()
        assembler.callNative(hof)
        assembler.emitReturn()

        val program = builder.build()
        val interpreter = Interpreter.create(program, table)
        interpreter.run(emptyContextReader)

        // EvalResultContainer.loadFromOperandStack pops the result, so it should be 0 now
        interpreter.operandStack.size() shouldBe 0
      }
    }
  }
}
