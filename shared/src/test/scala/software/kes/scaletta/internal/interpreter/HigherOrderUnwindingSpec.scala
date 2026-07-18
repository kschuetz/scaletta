package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class HigherOrderUnwindingSpec extends AnyFunSpec with Matchers {
  describe("Higher-order unwinding") {
    it("should clear stale continuations if dispatch fails") {
      val tableBuilder = NativeFunctionTable.builder()
      // This HOF sets a String argument but the callback expects an Int.
      val hof = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val target = args.readObject(0).asInstanceOf[CallTarget]
          target.setArgument(0, "not an int")
          NativeStep.Call(target, result => NativeStep.done(result))
        }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback.pushIntFromVar(0)
      callback.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback.index, CapturePlan.empty)
      main.callNative(hof)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      val ex = intercept[IllegalArgumentException] {
        interpreter.run(emptyContextReader)
      }
      ex.getMessage should include("Type mismatch")

      // Verify that stacks ARE empty
      interpreter.nativeContStack.size() shouldBe 0
      interpreter.callStack.size() shouldBe 0
    }

    it("should clear stale continuations if callback body throws") {
      val tableBuilder = NativeFunctionTable.builder()
      val hof = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val target = args.readObject(0).asInstanceOf[CallTarget]
          target.setArgument(0, 41)
          NativeStep.Call(target, result => NativeStep.done(result))
        }))
      // A native function that throws an exception
      val thrower = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int,
        FunctionImpl.intResult(_ => throw new RuntimeException("Body error"))))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      val callback = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback.callNative(thrower)
      callback.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback.index, CapturePlan.empty)
      main.callNative(hof)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      intercept[Throwable] {
        interpreter.run(emptyContextReader)
      }

      // Verify that stacks ARE empty
      interpreter.nativeContStack.size() shouldBe 0
      interpreter.callStack.size() shouldBe 0
    }

    it("should clear all stale continuations in nested HO calls if an error occurs") {
      val tableBuilder = NativeFunctionTable.builder()
      val hof = tableBuilder.add(NativeFunction(ParamsSignature.of(CoreTypes.AnyRefT), BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val target = args.readObject(0).asInstanceOf[CallTarget]
          target.setArgument(0, 41)
          NativeStep.Call(target, result => NativeStep.done(result))
        }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))

      // Callback 2 throws
      val callback2 = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback2.pushImmediateObject("not an int")
      callback2.pop() // Just to have something in the body
      // We'll make it throw by returning wrong type to HO result pusher if possible?
      // No, let's just throw a Scala exception.
      val thrower = tableBuilder.add(NativeFunction(ParamsSignature.empty, BasicTypes.Int,
        FunctionImpl.intResult(_ => throw new RuntimeException("Nested error"))))
      callback2.callNative(thrower)
      callback2.emitReturn()

      // Callback 1 calls hof again with callback 2
      val callback1 = builder.addFunction(UserFunctionSignature(VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback1.makeClosure(callback2.index, CapturePlan.empty)
      callback1.callNative(hof)
      callback1.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback1.index, CapturePlan.empty)
      main.callNative(hof)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      intercept[Throwable] {
        interpreter.run(emptyContextReader)
      }

      interpreter.nativeContStack.size() shouldBe 0
      interpreter.callStack.size() shouldBe 0
    }
  }
}
