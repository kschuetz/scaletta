package software.kes.scaletta.internal.interpreter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime._
import software.kes.scaletta.testsupport.emptyContextReader

import scala.collection.immutable.ArraySeq

class RuntimeClosureSpec extends AnyFunSuite with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  import stdLib.arithmetic

  test("simple closure capturing an Int") {
    // Child function (index 1): expects 1 Int parameter, 1 Int capture.
    // Logical slots: 0 -> parameter (Int), 1 -> capture (Int)
    // Frame slots: Int 0 -> parameter, Int 1 -> capture
    val childFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT, CoreTypes.IntT))
    val childVarSpace = VarSpaceSignature.of(childFrame)
    val childSig = UserFunctionSignature(childVarSpace, BasicTypes.Int, 1)

    // Main function (index 0):
    val mainFrame = FrameSignature.fromSeq(Seq(CoreTypes.IntT))
    val mainVarSpace = VarSpaceSignature.of(mainFrame)
    val mainSig = UserFunctionSignature(mainVarSpace, BasicTypes.Int, 0)

    val builder = ProgramBuilder.create(mainSig)
    val childAssembler = builder.addFunction(childSig)

    // Child body: return param0 + capture0
    childAssembler.pushIntFromVar(0)
    childAssembler.pushIntFromVar(1)
    childAssembler.callNative(arithmetic.int.add.int)
    childAssembler.emitReturn()

    val mainAssembler = builder.mainAssembler()
    // Var 0 in main: x = 47
    mainAssembler.pushImmediateInt(47)
    mainAssembler.popIntIntoVar(0)

    // Create capture plan: capture logical index 0 from main into CapturedFrame Int index 0
    val captureSignature = CaptureSignature.create(0, 0, 1, 0, 0, 0, 0, 0, 0)
    val capturePlan = CapturePlan.create(captureSignature, ArraySeq(0), ArraySeq(VarAddress.encode(BasicTypes.Int, 0)))

    // Push parameter 13
    mainAssembler.pushImmediateInt(13)

    // Make closure for child function (index 1)
    mainAssembler.makeClosure(1, capturePlan)

    // Call closure
    mainAssembler.callClosure()
    mainAssembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)
    val result = interpreter.run(emptyContextReader)
    result.intValue() shouldBe 60 // 47 + 13
  }

  test("closure capturing an Object") {
    // Child function (index 1): expects 0 parameters, 1 Object capture.
    val childFrame = FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT))
    val childVarSpace = VarSpaceSignature.of(childFrame)
    val childSig = UserFunctionSignature(childVarSpace, BasicTypes.Object, 0)

    val mainFrame = FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT))
    val mainVarSpace = VarSpaceSignature.of(mainFrame)
    val mainSig = UserFunctionSignature(mainVarSpace, BasicTypes.Object, 0)

    val builder = ProgramBuilder.create(mainSig)
    val childAssembler = builder.addFunction(childSig)

    // Child body: return capture0
    childAssembler.pushObjectFromVar(0)
    childAssembler.emitReturn()

    val mainAssembler = builder.mainAssembler()
    val testString = "Hello, closure!"
    mainAssembler.pushImmediateObject(testString)
    mainAssembler.popObjectIntoVar(0)

    val captureSignature = CaptureSignature.create(1, 0, 0, 0, 0, 0, 0, 0, 0)
    val capturePlan = CapturePlan.create(captureSignature, ArraySeq(0), ArraySeq(VarAddress.encode(BasicTypes.Object, 0)))

    mainAssembler.makeClosure(1, capturePlan)
    mainAssembler.callClosure()
    mainAssembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)
    val result = interpreter.run(emptyContextReader)
    result.value[AnyRef]() shouldBe testString
  }
}
