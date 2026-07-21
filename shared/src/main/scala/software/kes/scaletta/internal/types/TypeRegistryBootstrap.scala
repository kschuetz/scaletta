package software.kes.scaletta.internal.types

import software.kes.scaletta.api._

trait TypeRegistryBootstrap extends TypeRegistry {
  /**
   * Registers a type name for a given type ID.
   * Used for core types that have pre-assigned type IDs.
   */
  def registerCoreValueType(name: QualifiedName.Full,
                            typ: Type.Nominal[TypeId],
                            info: RuntimeTypeInfo): Type.Nominal[TypeId]

  def registerCoreRefType(name: QualifiedName.Full,
                          typ: Type.Nominal[TypeId],
                          info: RuntimeTypeInfo): Type.Nominal[TypeId]
}
