package software.kes.scaletta.internal.types

import software.kes.scaletta.api.{RuntimeTypeInfo, TypeId, UnapplyStrategy}
import software.kes.scaletta.internal.ast.TermSymbolId

/**
 * An immutable container for the complete type metadata of a Scaletta system.
 * This includes name resolution (via TypeNameIndex) and the inheritance graph (via AdjacencyTypeHierarchy).
 */
final class TypeUniverse private[scaletta](val nameIndex: TypeNameIndex,
                                           val hierarchy: AdjacencyTypeHierarchy[TypeId],
                                           val typeInfoMap: Map[TypeId, RuntimeTypeInfo],
                                           val staticSingletonInfoMap: Map[TermSymbolId.Static, RuntimeTypeInfo]) {
  def getUnapplyStrategy(typeId: TypeId): UnapplyStrategy =
    typeInfoMap.get(typeId).fold(UnapplyStrategy.noUnapply)(_.unapplyStrategy)

  def getIsInstance(typeId: TypeId): Any => Boolean =
    typeInfoMap.get(typeId).fold((_: Any) => false)(_.isInstance)

  def getUnapplyStrategy(symbol: TermSymbolId.Static): UnapplyStrategy =
    staticSingletonInfoMap.get(symbol).fold(UnapplyStrategy.noUnapply)(_.unapplyStrategy)

  def getIsInstance(symbol: TermSymbolId.Static): Any => Boolean =
    staticSingletonInfoMap.get(symbol).fold((_: Any) => false)(_.isInstance)
}
