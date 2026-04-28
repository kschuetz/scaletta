package software.kes.scaletta.builtins

final class Universe private[builtins](val symbolTable: FunctionSymbolTable,
                                       val dispatchTable: NativeFunctionTable)
