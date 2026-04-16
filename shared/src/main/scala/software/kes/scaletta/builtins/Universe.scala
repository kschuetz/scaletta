package software.kes.scaletta.builtins

final class Universe private(val symbolTable: FunctionSymbolTable,
                             val dispatchTable: NativeFunctionTable)
