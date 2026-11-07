package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api._
import software.kes.scaletta.internal.symbols._

import scala.collection.mutable

object FunctionSymbolTable {
  def empty: FunctionSymbolTable =
    new FunctionSymbolTable(SymbolTable.empty, Map.empty, Map.empty)

  def builder(): Builder = new Builder()

  final class Builder private[FunctionSymbolTable]() {
    private val staticFunctions = mutable.Map.empty[QualifiedName.Full, mutable.Buffer[NativeFunctionDefinition]]
    private val staticFunctionNames = mutable.Map.empty[NativeFunctionId, QualifiedName.Full]
    private val instanceMethods = mutable.Map.empty[Type[TypeId], mutable.Map[Name, mutable.Buffer[NativeFunctionDefinition]]]

    def addStatic(name: QualifiedName.Full, definition: NativeFunctionDefinition): Unit = {
      val overloads = staticFunctions.getOrElseUpdate(name, mutable.Buffer.empty)
      overloads += definition
      staticFunctionNames.put(definition.nativeFunctionId, name)
    }

    def addInstance(receiverType: Type[TypeId], name: Name, definition: NativeFunctionDefinition): Unit = {
      val methodsForType = instanceMethods.getOrElseUpdate(receiverType, mutable.Map.empty)
      val overloads = methodsForType.getOrElseUpdate(name, mutable.Buffer.empty)
      overloads += definition
    }

    def result(): FunctionSymbolTable = {
      val staticTable = staticFunctions.foldLeft(SymbolTable.empty[OverloadTable]) {
        case (acc, (name, definitions)) =>
          acc.add(name, OverloadTable(definitions.toList))
      }
      val instanceTable = instanceMethods.view.mapValues { methods =>
        methods.view.mapValues(definitions => OverloadTable(definitions.toList)).toMap
      }.toMap

      new FunctionSymbolTable(staticTable, staticFunctionNames.toMap, instanceTable)
    }
  }
}

final class FunctionSymbolTable private(private val staticFunctions: SymbolTable[OverloadTable],
                                        private val staticFunctionNames: Map[NativeFunctionId, QualifiedName.Full],
                                        private val instanceMethods: Map[Type[TypeId], Map[Name, OverloadTable]]) {
  /**
   * Retrieves the qualified name of a registered static function by its ID.
   */
  def getStaticFunctionName(id: NativeFunctionId): Option[QualifiedName.Full] =
    staticFunctionNames.get(id)

  /**
   * Performs a direct lookup for a fully qualified static function.
   */
  def getStaticFunction(name: QualifiedName.Full): Option[OverloadTable] =
    staticFunctions.get(name)

  /**
   * Performs a lookup for a method on a specific type.
   */
  def getMethod(typ: Type[TypeId], name: Name): Option[OverloadTable] =
    instanceMethods.get(typ).flatMap(_.get(name))

  /**
   * Returns all method overloads registered under a given name across all receiver types.
   */
  def getMethodsByName(name: Name): List[NativeFunctionDefinition] =
    instanceMethods.values.foldLeft(List.empty[NativeFunctionDefinition]) { (acc, methodsForType) =>
      methodsForType.get(name).fold(acc)(overloads => acc ++ overloads.variations)
    }

  /**
   * Resolves static functions matching the given name within the context of active imports.
   */
  def resolveStaticFunction(name: QualifiedName,
                            imports: ImportScope): List[SymbolEntry[OverloadTable]] =
    staticFunctions.resolve(name, imports)

}
