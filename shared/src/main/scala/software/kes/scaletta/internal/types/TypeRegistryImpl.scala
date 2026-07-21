package software.kes.scaletta.internal.types

import software.kes.scaletta.api._
import software.kes.scaletta.util.NonEmptyVector

import scala.collection.mutable

private[scaletta] final class TypeRegistryImpl extends TypeRegistryBootstrap {
  private var nameIndex = TypeNameIndex.empty
  private val supertypeMap = mutable.Map[Type[TypeId], mutable.Set[Type[TypeId]]]()
  private val valueTypes = mutable.Set[Type.Nominal[TypeId]]()
  private val typeInfoMap = mutable.Map[TypeId, RuntimeTypeInfo]()

  def addValueType(name: QualifiedName.Full, info: RuntimeTypeInfo): Type.Nominal[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    val typ = Type.Nominal(id)
    valueTypes += typ
    typeInfoMap += (id -> info)
    typ
  }

  def addRefType(name: QualifiedName.Full, info: RuntimeTypeInfo): Type.Nominal[TypeId] = {
    val (newIndex, id) = nameIndex.intern(name)
    nameIndex = newIndex
    val typ = Type.Nominal(id)
    typeInfoMap += (id -> info)
    typ
  }

  def addTypeConstructor(name: QualifiedName.Full,
                         parameters: NonEmptyVector[TypeParameter[TypeId]],
                         info: RuntimeTypeInfo): Type.Constructor[TypeId] = {
    val (newIndex, id) = nameIndex.internConstructor(name, parameters)
    nameIndex = newIndex
    typeInfoMap += (id -> info)
    Type.Constructor(id, parameters)
  }

  def addRelationship(supertype: Type[TypeId], subtype: Type[TypeId]): Unit = {
    val entries = supertypeMap.getOrElseUpdate(subtype, mutable.Set.empty)
    entries += supertype
  }

  def addAlias(name: QualifiedName.Full, target: Type[TypeId]): Unit = {
    nameIndex = nameIndex.addAlias(name, target)
  }

  def registerCoreValueType(name: QualifiedName.Full,
                            typ: Type.Nominal[TypeId],
                            info: RuntimeTypeInfo): Type.Nominal[TypeId] = {
    nameIndex = nameIndex.registerCore(name, typ)
    valueTypes += typ
    typeInfoMap += (typ.name -> info)
    typ
  }

  def registerCoreRefType(name: QualifiedName.Full,
                          typ: Type.Nominal[TypeId],
                          info: RuntimeTypeInfo): Type.Nominal[TypeId] = {
    nameIndex = nameIndex.registerCore(name, typ)
    typeInfoMap += (typ.name -> info)
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
    new TypeUniverse(nameIndex, hierarchy, typeInfoMap.toMap)
  }
}
