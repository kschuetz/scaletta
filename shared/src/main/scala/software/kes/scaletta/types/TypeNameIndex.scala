package software.kes.scaletta.types

import software.kes.scaletta.internal.symbols.{ImportScope, QualifiedName, SymbolEntry, SymbolIndex}

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
final class TypeNameIndex private(symbolIndex: SymbolIndex[TypeId],
                                  val allNames: Vector[QualifiedName.Full],
                                  nextId: Int) {
  /**
   * Performs a direct lookup for a fully qualified type name.
   *
   * @param name The fully qualified name to look up.
   * @return Some(TypeId) if the type name is registered, None otherwise.
   */
  def get(name: QualifiedName.Full): Option[TypeId] = symbolIndex.get(name)

  /**
   * Finds type entries matching the given query within the context of active imports.
   *
   * @param name    The QualifiedName (Full or Partial) to resolve.
   * @param imports The active implicit imports in the current scope.
   * @return A List of matching SymbolEntry.Global[TypeId] objects.
   */
  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry.Global[TypeId]] =
    symbolIndex.resolve(name, imports)

  /**
   * Registers a type name and returns its TypeId.
   * If the name is already registered, the existing TypeId is returned along with this index.
   * If the name is new, a new TypeId is assigned and an updated index is returned.
   *
   * @param name The fully qualified type name to intern.
   * @return A tuple containing the (potentially updated) TypeNameIndex and the TypeId.
   */
  def intern(name: QualifiedName.Full): (TypeNameIndex, TypeId) =
    symbolIndex.get(name) match {
      case Some(value) => (this, value)
      case None => addName(name)
    }

  /**
   * Adds a type name only if it doesn't already exist in the index.
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
    (new TypeNameIndex(symbolIndex.add(name, id), allNames :+ name, nextId + 1), id)
  }
}
