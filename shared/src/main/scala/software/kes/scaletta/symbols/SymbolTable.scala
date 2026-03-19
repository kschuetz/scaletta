package software.kes.scaletta.symbols

import software.kes.scaletta.common.PackagePath

object SymbolTable {
  /**
   * Creates an empty SymbolTable.
   */
  def empty[A]: SymbolTable[A] = SimpleSymbolTable.empty[A]
}

/**
 * A base index for resolving symbols. Supports global symbol additions but
 * does not support local scoping.
 *
 * @tparam A The type of value stored in the table.
 */
trait SymbolIndex[A] {

  /**
   * Performs a direct lookup for a fully qualified global name.
   *
   * @param name The fully qualified name to look up.
   * @return Some(value) if the symbol exists, None otherwise.
   */
  def get(name: QualifiedName.Full): Option[A]

  /**
   * Finds entries matching the given query within the context of active imports.
   *
   * @param name    The QualifiedName (Full or Partial) to look up.
   * @param imports The active implicit imports in the current scope.
   * @return A List of matching SymbolEntry.Global[A] objects.
   */
  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry.Global[A]]

  /**
   * Adds a global or package-level symbol.
   *
   * @param name The fully qualified name of the symbol.
   * @param value     The value to associate with the symbol.
   * @return A new SymbolIndex instance containing the added entry.
   */
  def add(name: QualifiedName.Full, value: A): SymbolIndex[A]
}

object SymbolIndex {

  /**
   * Creates an empty SymbolIndex.
   */
  def empty[A]: SymbolIndex[A] = SimpleSymbolIndex.empty[A]
}

/**
 * A table mapping identifiers to symbols within the Term or Type namespaces.
 *
 * @tparam A The type of value stored in the table.
 */
sealed trait SymbolTable[A] {

  /**
   * Performs a direct lookup for a fully qualified global name.
   *
   * @param name The fully qualified name to look up.
   * @return Some(value) if the symbol exists, None otherwise.
   */
  def get(name: QualifiedName.Full): Option[A]

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
   * @param name    The QualifiedName (Full or Partial) to look up.
   * @param imports The active implicit imports in the current scope.
   * @return A List of matching SymbolEntry[A] objects.
   *         - Nil: No match found.
   *         - List(entry): An unambiguous match found.
   *         - List(entry1, entry2, ...): Multiple matches found at the same priority level (Ambiguity).
   */
  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry[A]]

  /**
   * Adds a global or package-level symbol.
   *
   * @param name The fully qualified name of the symbol.
   * @param value     The value to associate with the symbol.
   * @return A new SymbolTable instance containing the added entry.
   */
  def add(name: QualifiedName.Full, value: A): SymbolTable[A]

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

  /**
   * Returns a view of this table as a global SymbolIndex.
   *
   * @return A SymbolIndex representing the global symbols in this table.
   */
  def asSymbolIndex: SymbolIndex[A]
}

private[symbols] abstract class BaseSymbolStore[A] {
  protected def globals: Map[PackagePath.Absolute, Map[String, A]]

  def get(name: QualifiedName.Full): Option[A] = {
    globals.get(name.qualifier).flatMap(_.get(name.name))
  }

  protected def resolveGlobal(name: QualifiedName,
                              imports: ImportScope): List[SymbolEntry.Global[A]] = {
    val (qualifier, identifier) = name match {
      case QualifiedName.Full(q, n) => (Some(q), n)
      case QualifiedName.Partial(q, n) => (q, n)
    }

    qualifier match {
      case None =>
        // Only search the root package for now
        globals.get(PackagePath.root).flatMap(_.get(identifier)) match {
          case Some(value) => List(SymbolEntry.Global(identifier, PackagePath.root, value))
          case None => Nil
        }

      case Some(path: PackagePath.Absolute) =>
        globals.get(path).flatMap(_.get(identifier)) match {
          case Some(value) => List(SymbolEntry.Global(identifier, path, value))
          case None => Nil
        }

      case Some(path: PackagePath.Relative) =>
        Nil
    }
  }
}

private[symbols] final class SimpleSymbolIndex[A](protected val globals: Map[PackagePath.Absolute, Map[String, A]])
  extends BaseSymbolStore[A] with SymbolIndex[A] {

  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry.Global[A]] = {
    resolveGlobal(name, imports)
  }

  def add(name: QualifiedName.Full, value: A): SymbolIndex[A] = {
    val packageMap = globals.getOrElse(name.qualifier, Map.empty)
    new SimpleSymbolIndex(globals + (name.qualifier -> (packageMap + (name.name -> value))))
  }
}

private[symbols] object SimpleSymbolIndex {
  def empty[A]: SimpleSymbolIndex[A] = new SimpleSymbolIndex(Map.empty)
}

private[symbols] final class SimpleSymbolTable[A](private val localScopes: List[Map[String, A]],
                                                  protected val globals: Map[PackagePath.Absolute, Map[String, A]])
  extends BaseSymbolStore[A] with SymbolTable[A] {

  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry[A]] = {
    val (qualifier, identifier) = name match {
      case QualifiedName.Full(q, n) => (Some(q), n)
      case QualifiedName.Partial(q, n) => (q, n)
    }

    qualifier match {
      case None =>
        // 1. Search local scopes from innermost to outermost
        val localMatch = localScopes.collectFirst {
          case scope if scope.contains(identifier) => List(SymbolEntry.Local(identifier, scope(identifier)))
        }

        localMatch.getOrElse {
          // 2. Search Specific Imports (to be implemented)

          // 3. Search Wildcard Imports and Root Package
          resolveGlobal(name, imports)
        }

      case Some(_: PackagePath.Absolute) =>
        // Direct lookup in an absolute package
        resolveGlobal(name, imports)

      case Some(_: PackagePath.Relative) =>
        // Partially qualified lookup (To be fully implemented with ImportScope)
        Nil
    }
  }

  def add(name: QualifiedName.Full, value: A): SymbolTable[A] = {
    val packageMap = globals.getOrElse(name.qualifier, Map.empty)
    new SimpleSymbolTable(localScopes, globals + (name.qualifier -> (packageMap + (name.name -> value))))
  }

  def addLocal(name: String, value: A): SymbolTable[A] = {
    localScopes match {
      case head :: tail =>
        new SimpleSymbolTable((head + (name -> value)) :: tail, globals)
      case Nil =>
        // If no local scope exists, treat as global/root
        add(QualifiedName.full(PackagePath.root, name), value)
    }
  }

  def enterScope: SymbolTable[A] =
    new SimpleSymbolTable(Map.empty[String, A] :: localScopes, globals)

  def asSymbolIndex: SymbolIndex[A] = new SimpleSymbolIndex(globals)
}

private[symbols] object SimpleSymbolTable {
  def empty[A]: SimpleSymbolTable[A] = new SimpleSymbolTable(Nil, Map.empty)
}
