package software.kes.scaletta.api

trait TypeRegistry {
  def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addTypeConstructor(name: QualifiedName.Full,
                         first: TypeParameter[TypeId], more: TypeParameter[TypeId]*): Type.Constructor[TypeId]

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit

  def addRelationship(name: QualifiedName.Full, target: Type[TypeId]): Unit
}
