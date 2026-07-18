package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class HigherOrderNestingSpec extends AnyFunSpec with Matchers {
  describe("Higher-order nesting") {
    it("should handle deeply nested HOF calls (native recursion)") {
      val tableBuilder = NativeFunctionTable.builder()
      // applyN(f: Int => Int, n: Int, x: Int): Int
      val applyN = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.IntT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val f = args.readObject(0).asInstanceOf[CallTarget]
          val n = args.readInt(1)
          val x = args.readInt(2)

          def go(currentN: Int, currentX: Int): NativeStep = {
            if (currentN <= 0) NativeStep.done(currentX)
            else {
              f.setArgument(0, currentX)
              NativeStep.Call(f, result => go(currentN - 1, result.asInstanceOf[Int]))
            }
          }

          go(n, x)
        }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      // callback(x: Int): Int = x + 1
      val callback = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback.pushIntFromVar(0)
      callback.pushImmediateInt(1)
      // native add
      val add = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
      // Re-create table with add
      val tableWithAdd = tableBuilder.result()

      callback.callNative(add)
      callback.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback.index, CapturePlan.empty)
      main.pushImmediateInt(100) // n = 100
      main.pushImmediateInt(13) // x = 13 (a prime!)
      main.callNative(applyN)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, tableWithAdd)

      interpreter.run(emptyContextReader)
      interpreter.getResult.intValue() shouldBe 113
    }

    it("should handle multiple nested callbacks") {
      val tableBuilder = NativeFunctionTable.builder()
      // combine(f: Int => Int, g: Int => Int, x: Int): Int = f(g(x))
      val combine = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.AnyRefT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val f = args.readObject(0).asInstanceOf[CallTarget]
          val g = args.readObject(1).asInstanceOf[CallTarget]
          val x = args.readInt(2)

          g.setArgument(0, x)
          NativeStep.Call(g, gRes => {
            f.setArgument(0, gRes.asInstanceOf[Int])
            NativeStep.Call(f, fRes => NativeStep.done(fRes))
          })
        }))

      val add1 = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.intResult(args => args.readInt(0) + 1)))

      val mul2 = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.intResult(args => args.readInt(0) * 2)))

      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))

      // f(x) = x + 1
      val f = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      f.pushIntFromVar(0)
      f.callNative(add1)
      f.emitReturn()

      // g(x) = x * 2
      val g = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      g.pushIntFromVar(0)
      g.callNative(mul2)
      g.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(f.index, CapturePlan.empty)
      main.makeClosure(g.index, CapturePlan.empty)
      main.pushImmediateInt(31) // x = 31
      main.callNative(combine)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      interpreter.run(emptyContextReader)
      interpreter.getResult.intValue() shouldBe (31 * 2 + 1)
    }

    it("should handle interleaving with lazy evaluation") {
      val tableBuilder = NativeFunctionTable.builder()
      // callWithCallback(f: Int => Int, x: Int): Int = f(x)
      val callWithCallback = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val f = args.readObject(0).asInstanceOf[CallTarget]
          val x = args.readInt(1)
          f.setArgument(0, x)
          NativeStep.Call(f, result => NativeStep.done(result))
        }))

      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Object))), // var 0 for lazy cell
        BasicTypes.Int, 0))

      // initializer for lazy val: calls callWithCallback
      val initializer = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.empty, BasicTypes.Int, 0))

      // another callback for callWithCallback
      val innerCallback = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      innerCallback.pushIntFromVar(0)
      innerCallback.pushImmediateInt(7)
      // Add a native add function to the table
      val add = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
      val tableWithAdd = tableBuilder.result()

      innerCallback.callNative(add)
      innerCallback.emitReturn()

      initializer.makeClosure(innerCallback.index, CapturePlan.empty)
      initializer.pushImmediateInt(37)
      initializer.callNative(callWithCallback)
      initializer.emitReturn()

      val main = builder.mainAssembler()
      main.lazyInit(BasicTypes.Int, 0)

      // Access lazy val twice
      main.lazyEval(0, initializer.index)
      main.lazyEval(0, initializer.index)

      main.callNative(add)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, tableWithAdd)

      interpreter.run(emptyContextReader)
      // (37 + 7) + (37 + 7) = 44 + 44 = 88
      interpreter.getResult.intValue() shouldBe 88
    }
  }
}
