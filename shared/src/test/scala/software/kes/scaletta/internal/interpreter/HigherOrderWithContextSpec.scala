package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._

class HigherOrderWithContextSpec extends AnyFunSpec with Matchers {
  val ctxId1 = RuntimeContextId(1)
  val ctxId2 = RuntimeContextId(2)

  class MapContextReader(contexts: Map[Int, Any]) extends RuntimeContextReader {
    override def readRuntimeContext[A](id: RuntimeContextId): A =
      contexts.get(id.value).map(_.asInstanceOf[A]).getOrElse(throw new NoSuchElementException(s"Context $id not found"))
  }

  describe("Higher-order with context natives") {
    it("should read a context value and immediately return Done") {
      val tableBuilder = NativeFunctionTable.builder()
      val nativeId = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.higherOrderWithContext { (ctx, _) =>
        val value = ctx.readRuntimeContext[Int](ctxId1)
        NativeStep.Done(value + 1)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val assembler = builder.mainAssembler()
      assembler.callNative(nativeId)
      assembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(new MapContextReader(Map(1 -> 41)))

      result.intValue() shouldBe 42
    }

    it("should use context inside k to transform callback result") {
      val tableBuilder = NativeFunctionTable.builder()
      val nativeId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrderWithContext { (ctx, args) =>
        val target = args.readObject(0).asInstanceOf[CallTarget]
        target.setArgument(0, 10)
        NativeStep.Call(target, res => {
          val factor = ctx.readRuntimeContext[Int](ctxId1)
          NativeStep.Done(res.asInstanceOf[Int] * factor)
        })
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val identityAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      identityAssembler.pushIntFromVar(0)
      identityAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.makeClosure(identityAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(nativeId)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)
      val result = interpreter.run(new MapContextReader(Map(1 -> 3)))

      result.intValue() shouldBe 30
    }

    it("should consult context in each iteration of a loop") {
      val tableBuilder = NativeFunctionTable.builder()
      val filterHO = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.AnyRefT), BasicTypes.Object, FunctionImpl.higherOrderWithContext { (ctx, args) =>
        val list = args.readObject(0).asInstanceOf[List[Int]]
        val target = args.readObject(1).asInstanceOf[CallTarget]

        def go(remaining: List[Int], acc: Vector[Int]): NativeStep = {
          remaining match {
            case Nil => NativeStep.Done(acc.toList)
            case head :: tail =>
              val cutoff = ctx.readRuntimeContext[Int](ctxId1)
              if (head < cutoff) {
                target.setArgument(0, head)
                NativeStep.Call(target, res => go(tail, acc :+ res.asInstanceOf[Int]))
              } else {
                go(tail, acc)
              }
          }
        }

        go(list, Vector.empty)
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val doubleAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      doubleAssembler.pushIntFromVar(0)
      doubleAssembler.pushIntFromVar(0)
      doubleAssembler.pushIntFromVar(0) // push it thrice to use it in add (wait, I need an add native)
      // I'll just use a simpler double: return arg * 2
      // But I don't have multiplication native yet in this test.
      // I'll just use a local add:
      val intAddId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int, FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
      // Re-build table because I added a new native
      val table2 = tableBuilder.result()

      val builder2 = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Object, 0))
      val doubleAssembler2 = builder2.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      doubleAssembler2.pushIntFromVar(0)
      doubleAssembler2.pushIntFromVar(0)
      doubleAssembler2.callNative(intAddId)
      doubleAssembler2.emitReturn()

      val mainAssembler = builder2.mainAssembler()
      mainAssembler.pushImmediateObject(List(1, 2, 3, 4, 5))
      mainAssembler.makeClosure(doubleAssembler2.index, CapturePlan.empty)
      mainAssembler.callNative(filterHO)
      mainAssembler.emitReturn()

      val program = builder2.build()
      val interpreter = Interpreter.create(program, table2)
      // Cutoff is 4, so it should keep 1, 2, 3 and double them -> 2, 4, 6
      val result = interpreter.run(new MapContextReader(Map(1 -> 4)))

      result.value[List[Int]]() shouldBe List(2, 4, 6)
    }

    it("should handle mixed usage: context-aware HOF calling context-aware natives") {
      val tableBuilder = NativeFunctionTable.builder()
      val getFactor = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int, FunctionImpl.intResultWithContext((ctx, _) => ctx.readRuntimeContext[Int](ctxId1))))

      val nativeId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int, FunctionImpl.higherOrderWithContext { (ctx, args) =>
        val target = args.readObject(0).asInstanceOf[CallTarget]
        target.setArgument(0, 7)
        NativeStep.Call(target, res => {
          val extra = ctx.readRuntimeContext[Int](ctxId2)
          NativeStep.Done(res.asInstanceOf[Int] + extra)
        })
      }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val callbackAssembler = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))), BasicTypes.Int, 1))
      callbackAssembler.pushIntFromVar(0)
      callbackAssembler.callNative(getFactor)
      // add them
      val intAddId = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT), BasicTypes.Int, FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
      val table2 = tableBuilder.result()

      callbackAssembler.callNative(intAddId)
      callbackAssembler.emitReturn()

      val mainAssembler = builder.mainAssembler()
      mainAssembler.makeClosure(callbackAssembler.index, CapturePlan.empty)
      mainAssembler.callNative(nativeId)
      mainAssembler.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table2)
      // ctx1 (factor) = 10, ctx2 (extra) = 5
      // input = 7
      // callback: 7 + 10 = 17
      // HOF: 17 + 5 = 22
      val result = interpreter.run(new MapContextReader(Map(1 -> 10, 2 -> 5)))

      result.intValue() shouldBe 22
    }
  }
}
