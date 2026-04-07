package software.kes.scaletta.types

import software.kes.scaletta.api.TypeRegistry
import software.kes.scaletta.symbols.QualifiedName

trait TypeRegistryBootstrap extends TypeRegistry {
  /**
   * Registers a type name for a given type ID.
   * Used for core types that have pre-assigned type IDs.
   */
  def registerCore(name: QualifiedName.Full,
                   typ: Type.Nominal[TypeId]): Type.Nominal[TypeId]
}
