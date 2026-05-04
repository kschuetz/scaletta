package software.kes.scaletta.interpreter

import software.kes.scaletta.api.{EvalResult, RuntimeContextReader}
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.util.stack.IntStack

object Interpreter {
  def create(program: Program,
             functionTable: NativeFunctionTable): Interpreter = {
    val callStack = IntStack.create()
    val operandStack = OperandStack.create()
    val variableStack = VariableStack.create()
    val varSpace = VarSpaceFromVariableStack.create(variableStack, program.mainFunction.varSpaceSignature)
    val evalResultContainer = EvalResultContainer.create(program.returnType)
    new Interpreter(program, functionTable, callStack, operandStack, variableStack,
      varSpace, evalResultContainer, 0, 0)
  }
}

final class Interpreter private(private val program: Program,
                                private val functionTable: NativeFunctionTable,
                                private val callStack: IntStack,
                                private val operandStack: OperandStack,
                                private val variableStack: VariableStack,
                                private val varSpace: VarSpaceFromVariableStack,
                                private val evalResultContainer: EvalResultContainer,
                                private var userFunctionIndex: Int,
                                private var instructionPointer: Int) {
  def run(runtimeContexts: RuntimeContextReader): EvalResult = {
    reset()
    evalResultContainer
  }

  private def reset(): Unit = {
    userFunctionIndex = 0
    instructionPointer = 0
    callStack.clear()
    operandStack.clear()
    variableStack.clear()
    varSpace.setSignature(program.mainFunction.varSpaceSignature)
  }
}
