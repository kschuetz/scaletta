package software.kes.scaletta.internal.intermediate

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.interpreter.{Opcodes, Program}
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, UserFunctionSignature, VarSpaceSignature}

class OptimizationSpec extends AnyFunSpec with Matchers {
  private val scaletta = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)
  private val compiler = scaletta.universe.compiler

  import IntermediateExpression.Value._
  import IntermediateExpression._

  describe("Bytecode Optimizations") {
    it("should not emit redundant Convert opcodes for NativeCall arguments") {
      val expr = NativeCall(stdLib.arithmetic.int.add.int, Vector(int(10), int(31)))
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val instructions = program.mainFunction.instructions

      val convertOpcodes = instructions.filter(i => (i >>> 24) == Opcodes.Convert)

      convertOpcodes should be(empty)
    }

    it("should not emit redundant Convert opcodes for Convert expressions") {
      val expr = Convert(int(41), BasicTypes.Int)
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Int, 0), expr)
      val instructions = program.mainFunction.instructions

      val convertOpcodes = instructions.filter(i => (i >>> 24) == Opcodes.Convert)

      convertOpcodes should be(empty)
    }

    it("should emit Convert opcodes when types do not match") {
      val expr = Convert(int(41), BasicTypes.Long)
      val program = compiler.compile(UserFunctionSignature(VarSpaceSignature.empty, BasicTypes.Long, 0), expr)
      val instructions = program.mainFunction.instructions

      val convertOpcodes = instructions.filter(i => (i >>> 24) == Opcodes.Convert)

      convertOpcodes.size shouldBe 1
      (convertOpcodes.head & 0x00FF0000) >> 16 shouldBe BasicTypes.Long
    }

    it("should optimize simple WithBindings where body is the last Val") {
      // { val x = 1; x }
      val expr = WithBindings(
        Vector(Binding.Val(int(1))),
        Reference(0, 0)
      )
      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.IntT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Int, 0), expr)
      val opcodes = getOpcodes(program)

      // Expected without optimization: PushConst, PopIntoVar, PushFromVar, Return
      // Expected with optimization: PushConst, Dup, PopIntoVar, Return
      opcodes should contain(Opcodes.Dup)
      opcodes shouldNot contain(Opcodes.PushFromVar)
    }

    it("should optimize ClosureCall where target is the last argument") {
      // val f = ...; f(f)
      val lambdaSig = UserFunctionSignature(
        VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT))),
        BasicTypes.Object,
        1
      )
      val lambdaExpr = Lambda(lambdaSig, Vector.empty, Reference(0, 0))

      val expr = WithBindings(
        Vector(Binding.Val(lambdaExpr)),
        ClosureCall(Reference(0, 0), Vector(Reference(0, 0)), BasicTypes.Object)
      )

      val signature = VarSpaceSignature.of(FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT)))
      val program = compiler.compile(UserFunctionSignature(signature, BasicTypes.Object, 0), expr)
      val opcodes = getOpcodes(program)

      // pushFromVar(0) (for arg) - No! WithBindings optimization keeps it on stack from the definition!
      // dup (for target)
      // callClosure
      opcodes should contain(Opcodes.Dup)
      // It should have ZERO PushFromVar because the lambda was already on stack from MakeClosure
      opcodes.count(_ == Opcodes.PushFromVar) shouldBe 0
    }
  }

  private def getInstructions(program: Program, functionIndex: Int = 0): Vector[Int] = {
    program.functions(functionIndex).instructions.toVector
  }

  private def getOpcodes(program: Program, functionIndex: Int = 0): Vector[Int] = {
    getInstructions(program, functionIndex).map(i => (i >> 24) & 0xFF)
  }
}
