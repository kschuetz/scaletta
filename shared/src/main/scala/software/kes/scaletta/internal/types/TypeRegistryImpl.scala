package software.kes.scaletta.internal.types

import software.kes.scaletta.api._

import scala.collection.mutable

private[scaletta] final class TypeRegistryImpl extends TypeRegistryBootstrap {
  private var nameIndex = TypeNameIndex.empty
  private val supertypeMap = mutable.Map[Type[TypeId], mutable.Set[Type[TypeId]]]()
  private val valueTypes = mutable.Set[Type.Nominal[TypeId]]()

  def addValueType(name: QualifiedName.Full): Type.Nominal[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    val typ = Type.Nominal(id)
    valueTypes += typ
    typ
  }

  def addRefType(name: QualifiedName.Full): Type.Nominal[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    Type.Nominal(id)
  }

  def addTypeConstructor(name: QualifiedName.Full,
                         first: TypeParameter[TypeId],
                         more: TypeParameter[TypeId]*): Type.Constructor[TypeId] = {
    val params = software.kes.scaletta.util.NonEmptyVector(first, more: _*)
    val (newIndex, id) = nameIndex.internConstructor(name, params)
    nameIndex = newIndex
    Type.Constructor(id, params)
  }

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit = {
    val entries = supertypeMap.getOrElseUpdate(subtype, mutable.Set.empty)
    entries += supertype
  }

  def addAlias(name: QualifiedName.Full, target: Type[TypeId]): Unit = {
    nameIndex = nameIndex.addAlias(name, target)
  }

  def registerCoreValueType(name: QualifiedName.Full,
                            typ: Type.Nominal[TypeId]): Type.Nominal[TypeId] = {
    nameIndex = nameIndex.registerCore(name, typ)
    valueTypes += typ
    typ
  }

  def registerCoreRefType(name: QualifiedName.Full,
                          typ: Type.Nominal[TypeId]): Type.Nominal[TypeId] = {
    nameIndex = nameIndex.registerCore(name, typ)
    typ
  }

  /**
   * Constructs the final immutable TypeUniverse.
   */
  def build(): TypeUniverse = {
    val hierarchy = AdjacencyTypeHierarchy.fromMap[TypeId](
      supertypeMap.view.mapValues(_.toSet).toMap,
      valueTypes.toSet
    )
    new TypeUniverse(nameIndex, hierarchy)
  }
}
