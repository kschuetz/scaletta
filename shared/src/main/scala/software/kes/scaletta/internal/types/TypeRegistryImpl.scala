package software.kes.scaletta.internal.types

import software.kes.scaletta.api._

import scala.collection.mutable

private[scaletta] final class TypeRegistryImpl extends TypeRegistryBootstrap {
  private var nameIndex = TypeNameIndex.empty
  private val supertypeMap = mutable.Map[Type[TypeId], mutable.Set[Type[TypeId]]]()

  def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    Type.Nominal(id)
  }

  def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    Type.Nominal(id)
  }

  def addTypeConstructor(name: QualifiedName.Full,
                         first: TypeParameter[TypeId],
                         more: TypeParameter[TypeId]*): TypeConstructor[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    TypeConstructor.create(id, software.kes.scaletta.util.NonEmptyVector(first, more: _*))
  }

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit = {
    val entries = supertypeMap.getOrElseUpdate(subtype, mutable.Set.empty)
    entries += supertype
  }

  def addRelationship(supertype: TypeConstructor[TypeId], subtype: TypeConstructor[TypeId]): Unit = {
    // For simplicity in this iteration, we treat type constructors as nominal types in the hierarchy
    // but applying them to variables to represent the general relationship.
    // This might need refinement in future iterations.
    val superType = Type.Nominal(supertype.name)
    val subType = Type.Nominal(subtype.name)
    addRelationship(superType, subType)
  }

  def addRelationship(subtype: TypeConstructor[TypeId], supertypeApplication: Type.Applied[TypeId]): Unit = {
    val subType = Type.Nominal(subtype.name)
    addRelationship(supertypeApplication, subType)
  }

  def registerCore(name: QualifiedName.Full,
                   typ: Type.Nominal[TypeId]): Type.Nominal[TypeId] = {
    val (newIndex, _) = nameIndex.intern(name)
    nameIndex = newIndex
    typ
  }

  /**
   * Constructs the final immutable TypeUniverse.
   */
  def build(): TypeUniverse = {
    val hierarchy = AdjacencyTypeHierarchy.fromMap[TypeId](
      supertypeMap.view.mapValues(_.toSet).toMap
    )
    new TypeUniverse(nameIndex, hierarchy)
  }
}
