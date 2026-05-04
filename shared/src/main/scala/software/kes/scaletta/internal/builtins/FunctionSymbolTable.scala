package software.kes.scaletta.internal.builtins

import software.kes.scaletta.internal.symbols._
import software.kes.scaletta.types.{Type, TypeId}

import scala.collection.mutable

object FunctionSymbolTable {
  def empty: FunctionSymbolTable =
    new FunctionSymbolTable(SymbolTable.empty, Map.empty)

  def builder(): Builder = new Builder()

  final class Builder private[FunctionSymbolTable]() {
    private val staticFunctions = mutable.Map.empty[QualifiedName.Full, mutable.Buffer[NativeFunctionDefinition]]
    private val instanceMethods = mutable.Map.empty[Type.Nominal[TypeId], mutable.Map[Name, mutable.Buffer[NativeFunctionDefinition]]]

    def addStatic(name: QualifiedName.Full, definition: NativeFunctionDefinition): Unit = {
      val overloads = staticFunctions.getOrElseUpdate(name, mutable.Buffer.empty)
      overloads += definition
    }

    def addInstance(receiverType: Type.Nominal[TypeId], name: Name, definition: NativeFunctionDefinition): Unit = {
      val methodsForType = instanceMethods.getOrElseUpdate(receiverType, mutable.Map.empty)
      val overloads = methodsForType.getOrElseUpdate(name, mutable.Buffer.empty)
      overloads += definition
    }

    def result(): FunctionSymbolTable = {
      val staticTable = staticFunctions.foldLeft(SymbolTable.empty[OverloadTable]) {
        case (acc, (name, definitions)) =>
          acc.add(name, OverloadTable(definitions.toVector))
      }
      val instanceTable = instanceMethods.view.mapValues { methods =>
        methods.view.mapValues(definitions => OverloadTable(definitions.toVector)).toMap
      }.toMap

      new FunctionSymbolTable(staticTable, instanceTable)
    }
  }
}

final class FunctionSymbolTable private(private val staticFunctions: SymbolTable[OverloadTable],
                                        private val instanceMethods: Map[Type.Nominal[TypeId], Map[Name, OverloadTable]]) {
  /**
   * Performs a direct lookup for a fully qualified static function.
   */
  def getStatic(name: QualifiedName.Full): Option[OverloadTable] =
    staticFunctions.get(name)

  /**
   * Performs a lookup for a method on a specific nominal type.
   */
  def getMethod(typ: Type.Nominal[TypeId], name: Name): Option[OverloadTable] =
    instanceMethods.get(typ).flatMap(_.get(name))

  /**
   * Resolves static functions matching the given name within the context of active imports.
   */
  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry[OverloadTable]] =
    staticFunctions.resolve(name, imports)
}
