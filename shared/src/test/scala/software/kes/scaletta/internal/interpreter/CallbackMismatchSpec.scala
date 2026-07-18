package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionTable}
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

class CallbackMismatchSpec extends AnyFunSpec with Matchers {
  describe("Callback arity and type mismatches") {
    it("should fail when callback is called with too many arguments") {
      val tableBuilder = NativeFunctionTable.builder()
      // callWithTooManyArgs(f: Int => Int, x: Int): Int = f(x, x)
      val callWithTooManyArgs = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val f = args.readObject(0).asInstanceOf[CallTarget]
          val x = args.readInt(1)
          f.setArgument(0, x)
          f.setArgument(1, x) // Too many!
          NativeStep.Call(f, result => NativeStep.done(result))
        }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      // callback(x: Int): Int = x + 1
      val callback = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback.pushIntFromVar(0)
      callback.pushImmediateInt(1)

      val add = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
      val tableWithAdd = tableBuilder.result()

      callback.callNative(add)
      callback.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback.index, CapturePlan.empty)
      main.pushImmediateInt(13)
      main.callNative(callWithTooManyArgs)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, tableWithAdd)

      val exception = intercept[IndexOutOfBoundsException] {
        interpreter.run(emptyContextReader)
      }
    }

    it("should fail when callback is called with wrong argument type") {
      val tableBuilder = NativeFunctionTable.builder()
      // callWithWrongType(f: Int => Int, x: String): Int = f(x)
      val callWithWrongType = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.StringT),
        BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val f = args.readObject(0).asInstanceOf[CallTarget]
          val s = args.readObject(1)
          f.setArgument(0, s) // Passing String where Int is expected
          NativeStep.Call(f, result => NativeStep.done(result))
        }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      // callback(x: Int): Int = x + 1
      val callback = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Int, 1))
      callback.pushIntFromVar(0)
      callback.pushImmediateInt(1)

      val add = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.intResult(args => args.readInt(0) + args.readInt(1))))
      val tableWithAdd = tableBuilder.result()

      callback.callNative(add)
      callback.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback.index, CapturePlan.empty)
      main.pushImmediate("not an int")
      main.callNative(callWithWrongType)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, tableWithAdd)

      val exception = intercept[IllegalArgumentException] {
        interpreter.run(emptyContextReader)
      }
      exception.getMessage should include("Type mismatch")
      exception.getMessage should include("expected Int")
      exception.getMessage should include("java.lang.String")
    }

    it("should fail when callback returns wrong type") {
      val tableBuilder = NativeFunctionTable.builder()
      // callAndExpectInt(f: Int => Int, x: Int): Int = f(x)
      val callAndExpectInt = tableBuilder.add(NativeFunction(
        ParamsSignature.of(CoreTypes.AnyRefT, CoreTypes.IntT),
        BasicTypes.Int,
        FunctionImpl.higherOrder { args =>
          val f = args.readObject(0).asInstanceOf[CallTarget]
          val x = args.readInt(1)
          f.setArgument(0, x)
          NativeStep.Call(f, result => {
            // Native implementation expects an Int, but callback will return a String
            // We pass it through to NativeStep.done to see if NativeResultPusher catches the mismatch
            NativeStep.done(result)
          })
        }))
      val table = tableBuilder.result()

      val builder = ProgramBuilder.create(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0))
      // callback(x: Int): String = "result is " + x
      val callback = builder.addFunction(UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromBasicTypes(Seq(BasicTypes.Int))),
        BasicTypes.Object, 1))
      callback.pushImmediate("result is ")
      callback.pushIntFromVar(0)
      callback.box()
      callback.stringConcat(2)
      callback.emitReturn()

      val main = builder.mainAssembler()
      main.makeClosure(callback.index, CapturePlan.empty)
      main.pushImmediateInt(41)
      main.callNative(callAndExpectInt)
      main.emitReturn()

      val program = builder.build()
      val interpreter = Interpreter.create(program, table)

      val exception = intercept[IllegalArgumentException] {
        interpreter.run(emptyContextReader)
      }
      exception.getMessage should include("Type mismatch")
      exception.getMessage should include("expected Int")
      exception.getMessage should include("java.lang.String")
    }
  }
}
