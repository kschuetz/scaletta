package software.kes.scaletta.internal.builtins

final class MethodUniverse private[builtins](val symbolTable: FunctionSymbolTable,
                                             val dispatchTable: NativeFunctionTable)
