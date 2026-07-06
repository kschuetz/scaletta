package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.interpreter.Program
import software.kes.scaletta.internal.runtime.UserFunctionSignature

final class IntermediateExpressionCompiler(nativeFunctionTable: NativeFunctionTable) {
  def compile(mainSignature: UserFunctionSignature,
              expression: IntermediateExpression): Program = ???
}
