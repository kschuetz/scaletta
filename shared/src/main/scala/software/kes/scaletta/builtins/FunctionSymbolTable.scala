package software.kes.scaletta.builtins

import software.kes.scaletta.symbols.{ImportScope, QualifiedName, SymbolEntry, SymbolTable}

object FunctionSymbolTable {
  def empty: FunctionSymbolTable = new FunctionSymbolTable(SymbolTable.empty)
}

final class FunctionSymbolTable private(private val table: SymbolTable[OverloadTable]) {
  def get(name: QualifiedName.Full): Option[OverloadTable] = table.get(name)

  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry[OverloadTable]] =
    table.resolve(name, imports)
}
