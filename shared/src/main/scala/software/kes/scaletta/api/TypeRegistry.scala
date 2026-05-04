package software.kes.scaletta.api

import software.kes.scaletta.internal.symbols.QualifiedName
import software.kes.scaletta.internal.types.{TypeConstructor, TypeParameter}

trait TypeRegistry {
  def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addTypeConstructor(name: QualifiedName.Full,
                         first: TypeParameter[TypeId], more: TypeParameter[TypeId]*): TypeConstructor[TypeId]

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit

  def addRelationship(supertype: TypeConstructor[TypeId], subtype: TypeConstructor[TypeId]): Unit

  def addRelationship(subtype: TypeConstructor[TypeId], supertypeApplication: Type.Applied[TypeId]): Unit
}
