package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api.NativeFunctionId

final class MethodUniverse private[builtins](val symbolTable: FunctionSymbolTable,
                                             val dispatchTable: NativeFunctionTable) {
  def getDefinition(id: NativeFunctionId): NativeFunctionDefinition = dispatchTable.getDefinition(id)
}
