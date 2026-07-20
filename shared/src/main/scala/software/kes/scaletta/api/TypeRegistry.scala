package software.kes.scaletta.api

import software.kes.scaletta.util.NonEmptyVector

trait TypeRegistry {
  def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addTypeConstructor(name: QualifiedName.Full,
                         parameters: NonEmptyVector[TypeParameter[TypeId]]): Type.Constructor[TypeId]

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit

  def addAlias(name: QualifiedName.Full, target: Type[TypeId]): Unit
}
