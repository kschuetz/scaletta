package software.kes.scaletta.internal.types

import software.kes.scaletta.api._
import software.kes.scaletta.internal.symbols.{SymbolEntry, SymbolIndex}
import software.kes.scaletta.util.NonEmptyVector

object TypeNameIndex {
  /**
   * Returns an empty TypeNameIndex.
   *
   * @return An empty TypeNameIndex.
   */
  def empty: TypeNameIndex = new TypeNameIndex(SymbolIndex.empty, Vector.empty, 0)
}

/**
 * A canonical registry for type names that provides a stable mapping between
 * human-readable names and efficient internal representations (TypeId).
 *
 * This index ensures that each unique QualifiedName.Full maps to a single, unique TypeId.
 * It is immutable and persistent.
 */
final class TypeNameIndex private(symbolIndex: SymbolIndex[Type[TypeId]],
                                  val allNames: Vector[QualifiedName.Full],
                                  nextId: Int) {
  /**
   * Performs a direct lookup for a fully qualified type name.
   *
   * @param name The fully qualified name to look up.
   * @return Some(Type[TypeId]) if the type name is registered, None otherwise.
   */
  def get(name: QualifiedName.Full): Option[Type[TypeId]] = symbolIndex.get(name)

  /**
   * Finds type entries matching the given query within the context of active imports.
   *
   * @param name    The QualifiedName (Full or Partial) to resolve.
   * @param imports The active implicit imports in the current scope.
   * @return A List of matching SymbolEntry.Global[Type[TypeId]] objects.
   */
  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry.Global[Type[TypeId]]] =
    symbolIndex.resolve(name, imports)

  /**
   * Registers a type constructor and returns its TypeId.
   *
   * @param name       The fully qualified type name to intern.
   * @param parameters The type parameters for the constructor.
   * @return A tuple containing the (potentially updated) TypeNameIndex and the TypeId.
   */
  def internConstructor(name: QualifiedName.Full,
                        parameters: NonEmptyVector[TypeParameter[TypeId]]): (TypeNameIndex, TypeId) = {
    symbolIndex.get(name) match {
      case Some(Type.Constructor(_, existingParams)) if existingParams == parameters =>
        val id = TypeId(allNames.indexOf(name))
        (this, id)
      case Some(_) => throw new IllegalStateException(s"Name $name is already registered with a different definition")
      case None =>
        val id = TypeId(nextId)
        val entry = Type.Constructor(id, parameters)
        (new TypeNameIndex(symbolIndex.add(name, entry), allNames :+ name, nextId + 1), id)
    }
  }

  /**
   * Registers a nominal type name and returns its TypeId.
   * If the name is already registered as a nominal type, the existing TypeId is returned along with this index.
   * If the name is already registered as an alias, an exception is thrown.
   * If the name is new, a new TypeId is assigned and an updated index is returned.
   *
   * @param name The fully qualified type name to intern.
   * @return A tuple containing the (potentially updated) TypeNameIndex and the TypeId.
   */
  def intern(name: QualifiedName.Full): (TypeNameIndex, TypeId) =
    symbolIndex.get(name) match {
      case Some(Type.Nominal(id)) => (this, id)
      case Some(_) => throw new IllegalStateException(s"Name $name is already registered as an alias")
      case None => addName(name)
    }

  /**
   * Adds a type name as an alias to an arbitrary type structure.
   *
   * @param name   The fully qualified name for the alias.
   * @param target The type structure the name should alias.
   * @return An updated TypeNameIndex.
   */
  def addAlias(name: QualifiedName.Full, target: Type[TypeId]): TypeNameIndex = {
    new TypeNameIndex(symbolIndex.add(name, target), allNames, nextId)
  }

  /**
   * Registers a core type with a pre-assigned TypeId.
   *
   * @param name   The fully qualified name for the core type.
   * @param target The type structure (must be Nominal or Constructor to update allNames).
   * @return An updated TypeNameIndex.
   */
  def registerCore(name: QualifiedName.Full, target: Type[TypeId]): TypeNameIndex = {
    symbolIndex.get(name) match {
      case Some(existing) if existing == target => this
      case Some(_) => throw new IllegalStateException(s"Name $name is already registered with a different definition")
      case None =>
        val idOpt = target match {
          case Type.Nominal(id) => Some(id.value)
          case Type.Constructor(id, _) => Some(id.value)
          case _ => None
        }

        idOpt match {
          case Some(id) =>
            val newAllNames =
              if (id < allNames.size) {
                if (allNames(id) != null && allNames(id) != name) {
                  throw new IllegalStateException(s"TypeId $id is already assigned to ${allNames(id)}")
                }
                allNames.updated(id, name)
              } else {
                val padding = Vector.fill(id - allNames.size)(null: QualifiedName.Full)
                allNames ++ padding :+ name
              }
            val newNextId = math.max(nextId, id + 1)
            new TypeNameIndex(symbolIndex.add(name, target), newAllNames, newNextId)
          case None =>
            addAlias(name, target)
        }
    }
  }

  /**
   * Adds a nominal type name only if it doesn't already exist in the index.
   *
   * @param name The fully qualified type name to add.
   * @return Some((updatedIndex, typeId)) if the name was added, None if it already existed.
   */
  def addUnique(name: QualifiedName.Full): Option[(TypeNameIndex, TypeId)] =
    if (symbolIndex.contains(name)) None
    else Some(addName(name))

  /**
   * Retrieves the fully qualified name for a given TypeId.
   *
   * @param typeId The TypeId to look up.
   * @return The fully qualified name associated with the TypeId.
   * @throws IndexOutOfBoundsException if the TypeId is invalid.
   */
  def getName(typeId: TypeId): QualifiedName.Full = allNames(typeId.value)

  /**
   * Returns the number of registered type names.
   *
   * @return The size of the index.
   */
  def size: Int = allNames.size

  private def addName(name: QualifiedName.Full): (TypeNameIndex, TypeId) = {
    val id = TypeId(nextId)
    val entry = Type.Nominal(id)
    (new TypeNameIndex(symbolIndex.add(name, entry), allNames :+ name, nextId + 1), id)
  }
}
