package software.kes.scaletta.builtins

final class MethodUniverse private[builtins](val symbolTable: FunctionSymbolTable,
                                             val dispatchTable: NativeFunctionTable)
