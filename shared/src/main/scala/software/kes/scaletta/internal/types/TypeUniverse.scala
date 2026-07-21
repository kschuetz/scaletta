package software.kes.scaletta.internal.types

import software.kes.scaletta.api.{RuntimeTypeInfo, TypeId, UnapplyStrategy}

/**
 * An immutable container for the complete type metadata of a Scaletta system.
 * This includes name resolution (via TypeNameIndex) and the inheritance graph (via AdjacencyTypeHierarchy).
 */
final class TypeUniverse private[scaletta](val nameIndex: TypeNameIndex,
                                           val hierarchy: AdjacencyTypeHierarchy[TypeId],
                                           val typeInfoMap: Map[TypeId, RuntimeTypeInfo]) {
  def getUnapplyStrategy(typeId: TypeId): UnapplyStrategy =
    typeInfoMap.get(typeId).fold(UnapplyStrategy.noUnapply)(_.unapplyStrategy)

  def getIsInstance(typeId: TypeId): Any => Boolean =
    typeInfoMap.get(typeId).fold((_: Any) => false)(_.isInstance)
}
