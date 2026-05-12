package software.kes.scaletta.internal.types

import software.kes.scaletta.api.TypeId

/**
 * An immutable container for the complete type metadata of a Scaletta system.
 * This includes name resolution (via TypeNameIndex) and the inheritance graph (via AdjacencyTypeHierarchy).
 */
final class TypeUniverse private[scaletta](val nameIndex: TypeNameIndex,
                                           val hierarchy: AdjacencyTypeHierarchy[TypeId])
