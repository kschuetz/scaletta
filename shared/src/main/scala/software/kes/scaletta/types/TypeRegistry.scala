package software.kes.scaletta.types

import software.kes.scaletta.symbols.QualifiedName

trait TypeRegistry {
  def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit
}
