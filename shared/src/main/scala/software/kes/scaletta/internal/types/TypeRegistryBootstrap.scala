package software.kes.scaletta.internal.types

import software.kes.scaletta.api.{Type, TypeId, TypeRegistry}
import software.kes.scaletta.internal.symbols.QualifiedName

trait TypeRegistryBootstrap extends TypeRegistry {
  /**
   * Registers a type name for a given type ID.
   * Used for core types that have pre-assigned type IDs.
   */
  def registerCore(name: QualifiedName.Full,
                   typ: Type.Nominal[TypeId]): Type.Nominal[TypeId]
}
