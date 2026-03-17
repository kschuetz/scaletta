package software.kes.scaletta.symbols

import software.kes.scaletta.common.PackagePath

object SymbolTable {
  /**
   * Represents a resolved entry in the symbol table.
   *
   * @param name      The simple name of the symbol.
   * @param container The absolute package path of the entity containing this symbol.
   * @param value     The generic value associated with the symbol (e.g., a Type or Term definition).
   * @tparam A The type of the stored value.
   */
  final case class Entry[+A](name: String,
                             container: Option[PackagePath.Absolute],
                             value: A)

  /**
   * Creates an empty SymbolTable.
   */
  def empty[A]: SymbolTable[A] = SimpleSymbolTable.empty[A]
}

/**
 * A table mapping identifiers to symbols within the Term or Type namespaces.
 *
 * @tparam A The type of value stored in the table.
 */
sealed trait SymbolTable[A] {

  /**
   * Finds entries matching the given query within the context of active imports.
   *
   * This method follows standard name resolution priority rules:
   * 1. Lexical Scopes (Inner to Outer)
   * 2. Specific Imports (to be implemented)
   * 3. Wildcard Imports and Root Package
   *
   * If an unambiguous match is found at a higher priority level, it is returned immediately
   * and lower priority levels are not searched.
   *
   * Ambiguity (returning multiple entries) only occurs if multiple matches are found
   * at the *same* priority level (e.g., from multiple wildcard imports).
   *
   * @param qualifier The PackagePath representing the qualification (Absolute or Relative), if present.
   *                  This path should not include the symbol name itself.
   * @param name      The specific identifier/symbol name to look up.
   * @param imports   The active implicit imports in the current scope.
   * @return A List of matching Entry[A] objects.
   *         - Nil: No match found.
   *         - List(entry): An unambiguous match found.
   *         - List(entry1, entry2, ...): Multiple matches found at the same priority level (Ambiguity).
   */
  def resolve(qualifier: Option[PackagePath],
              name: String,
              imports: ImportScope): List[SymbolTable.Entry[A]]

  /**
   * Adds a global or package-level symbol.
   *
   * @param container The absolute path of the package/object containing the symbol.
   * @param name      The identifier of the symbol.
   * @param value     The value to associate with the symbol.
   * @return A new SymbolTable instance containing the added entry.
   */
  def add(container: PackagePath.Absolute, name: String, value: A): SymbolTable[A]

  /**
   * Adds a local symbol to the current innermost scope.
   *
   * @param name  The identifier of the symbol.
   * @param value The value to associate with the symbol.
   * @return A new SymbolTable instance containing the added entry.
   */
  def addLocal(name: String, value: A): SymbolTable[A]

  /**
   * Pushes a new local scope onto the stack.
   *
   * @return A new SymbolTable instance with an additional empty local scope.
   */
  def enterScope: SymbolTable[A]
}

private[symbols] final class SimpleSymbolTable[A](private val localScopes: List[Map[String, A]],
                                                  private val globals: Map[PackagePath.Absolute, Map[String, A]])
  extends SymbolTable[A] {

  def resolve(qualifier: Option[PackagePath],
              name: String,
              imports: ImportScope): List[SymbolTable.Entry[A]] = {
    qualifier match {
      case None =>
        // 1. Search local scopes from innermost to outermost
        val localMatch = localScopes.collectFirst {
          case scope if scope.contains(name) => List(SymbolTable.Entry(name, None, scope(name)))
        }

        localMatch.getOrElse {
          // 2. Search Specific Imports (to be implemented)

          // 3. Search Wildcard Imports and Root Package
          // For now, only searching the root package since wildcard imports aren't implemented in the stub
          globals.get(PackagePath.root).flatMap(_.get(name)) match {
            case Some(value) => List(SymbolTable.Entry(name, Some(PackagePath.root), value))
            case None => Nil
          }
        }

      case Some(path: PackagePath.Absolute) =>
        // Direct lookup in an absolute package
        globals.get(path).flatMap(_.get(name)) match {
          case Some(value) => List(SymbolTable.Entry(name, Some(path), value))
          case None => Nil
        }

      case Some(path: PackagePath.Relative) =>
        // Partially qualified lookup (To be fully implemented with ImportScope)
        Nil
    }
  }

  def add(container: PackagePath.Absolute, name: String, value: A): SymbolTable[A] = {
    val packageMap = globals.getOrElse(container, Map.empty)
    new SimpleSymbolTable(localScopes, globals + (container -> (packageMap + (name -> value))))
  }

  def addLocal(name: String, value: A): SymbolTable[A] = {
    localScopes match {
      case head :: tail =>
        new SimpleSymbolTable((head + (name -> value)) :: tail, globals)
      case Nil =>
        // If no local scope exists, treat as global/root
        add(PackagePath.root, name, value)
    }
  }

  def enterScope: SymbolTable[A] =
    new SimpleSymbolTable(Map.empty[String, A] :: localScopes, globals)
}

private[symbols] object SimpleSymbolTable {
  def empty[A]: SimpleSymbolTable[A] = new SimpleSymbolTable(Nil, Map.empty)
}
