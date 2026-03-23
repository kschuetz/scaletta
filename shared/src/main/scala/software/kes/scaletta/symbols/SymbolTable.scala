package software.kes.scaletta.symbols

import software.kes.scaletta.common.PackagePath

object SymbolTable {
  /**
   * Creates an empty SymbolTable.
   */
  def empty[A]: SymbolTable[A] = SimpleSymbolTable.empty[A]

  def of[A](symbols: (QualifiedName.Full, A)*): SymbolTable[A] =
    symbols.foldLeft(empty[A]) {
      case (acc, (name, value)) => acc.add(name, value)
    }
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
   * Checks if a fully qualified global name exists in the index.
   *
   * @param name The fully qualified name to check.
   * @return true if the symbol exists, false otherwise.
   */
  def contains(name: QualifiedName.Full): Boolean

  /**
   * Adds a global or package-level symbol.
   *
   * @param name  The fully qualified name of the symbol.
   * @param value The value to associate with the symbol.
   * @return A new SymbolIndex instance containing the added entry.
   */
  def add(name: QualifiedName.Full, value: A): SymbolIndex[A]

  /**
   * Merges another SymbolIndex into this one.
   * Symbols in the other index will overwrite symbols with the same name in this index.
   */
  def merge(other: SymbolIndex[A]): SymbolIndex[A]

  /**
   * Converts this index into a SymbolTable with no local scopes.
   */
  def toSymbolTable: SymbolTable[A]
}

object SymbolIndex {

  /**
   * Creates an empty SymbolIndex.
   */
  def empty[A]: SymbolIndex[A] = SimpleSymbolIndex.empty[A]

  def of[A](symbols: (QualifiedName.Full, A)*): SymbolIndex[A] =
    symbols.foldLeft(empty[A]) {
      case (acc, (name, value)) => acc.add(name, value)
    }
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
   * Checks if a fully qualified global name exists in the table.
   *
   * @param name The fully qualified name to check.
   * @return true if the symbol exists, false otherwise.
   */
  def contains(name: QualifiedName.Full): Boolean

  /**
   * Adds a global or package-level symbol.
   *
   * @param name  The fully qualified name of the symbol.
   * @param value The value to associate with the symbol.
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
  def addLocal(name: Name, value: A): SymbolTable[A]

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
  def toSymbolIndex: SymbolIndex[A]
}

private[symbols] abstract class BaseSymbolStore[A] {
  protected def globals: PackageNode[A]

  def get(name: QualifiedName.Full): Option[A] = {
    globals.findNode(name.qualifier).flatMap(_.get(name.name))
  }

  def contains(name: QualifiedName.Full): Boolean = {
    globals.findNode(name.qualifier).exists(_.symbols.contains(name.name))
  }

  protected def resolveGlobal(name: QualifiedName,
                              imports: ImportScope): List[SymbolEntry.Global[A]] = {
    name match {
      case QualifiedName.Full(qualifier, identifier) =>
        // Direct lookup for absolute paths
        globals.findNode(qualifier).flatMap(_.get(identifier)) match {
          case Some(value) => List(SymbolEntry.Global(identifier, qualifier, value))
          case None => Nil
        }

      case QualifiedName.Partial(None, identifier) =>
        // Unqualified name resolution
        // 1. Specific Imports
        val specificMatch = imports.symbols.get(identifier).flatMap { pkgPath =>
          globals.findNode(pkgPath).flatMap(_.get(identifier)).map { value =>
            SymbolEntry.Global(identifier, pkgPath, value)
          }
        }

        specificMatch match {
          case Some(entry) => List(entry)
          case None =>
            // 2. Root Package
            val rootMatch = globals.get(identifier).map { value =>
              SymbolEntry.Global(identifier, PackagePath.root, value)
            }

            rootMatch match {
              case Some(entry) => List(entry)
              case None =>
                // 3. Wildcard Imports
                imports.wildcards.toList.flatMap { pkgPath =>
                  globals.findNode(pkgPath).flatMap(_.get(identifier)).map { value =>
                    SymbolEntry.Global(identifier, pkgPath, value)
                  }
                }.distinct
            }
        }

      case QualifiedName.Partial(Some(rel: PackagePath.Relative), identifier) =>
        // Relative qualifier resolution (Nested paths)
        val components = rel.components
        val firstSegment = Name(components.head.name)
        val remainingRel = PackagePath.relative(components.tail: _*)

        val candidates = imports.packages.get(firstSegment) match {
          case Some(absPath: PackagePath.Absolute) =>
            // If the first segment is an imported package, we look there.
            val fullPkgPath = absPath ++ remainingRel
            globals.findNode(fullPkgPath).flatMap(_.get(identifier)).map { value =>
              SymbolEntry.Global(identifier, fullPkgPath, value)
            }.toList
          case _ =>
            // Otherwise, we look in the root package.
            val fullPkgPath = PackagePath.root ++ rel
            globals.findNode(fullPkgPath).flatMap(_.get(identifier)).map { value =>
              SymbolEntry.Global(identifier, fullPkgPath, value)
            }.toList
        }
        candidates

      case QualifiedName.Partial(Some(abs: PackagePath.Absolute), identifier) =>
        // Treat like QualifiedName.Full
        globals.findNode(abs).flatMap(_.get(identifier)) match {
          case Some(value) => List(SymbolEntry.Global(identifier, abs, value))
          case None => Nil
        }
    }
  }
}

private[symbols] final class SimpleSymbolIndex[A](protected val globals: PackageNode[A])
  extends BaseSymbolStore[A] with SymbolIndex[A] {

  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry.Global[A]] = {
    resolveGlobal(name, imports)
  }

  def add(name: QualifiedName.Full, value: A): SymbolIndex[A] = {
    new SimpleSymbolIndex(globals.add(name.qualifier, name.name, value))
  }

  def merge(other: SymbolIndex[A]): SymbolIndex[A] = {
    val otherIndex = other.asInstanceOf[SimpleSymbolIndex[A]]
    new SimpleSymbolIndex(globals.merge(otherIndex.globals))
  }

  def toSymbolTable: SymbolTable[A] = new SimpleSymbolTable(Nil, globals)
}

private[symbols] object SimpleSymbolIndex {
  def empty[A]: SimpleSymbolIndex[A] = new SimpleSymbolIndex(PackageNode.empty[A])
}

private[symbols] final class SimpleSymbolTable[A](private val localScopes: List[Map[Name, A]],
                                                  protected val globals: PackageNode[A])
  extends BaseSymbolStore[A] with SymbolTable[A] {

  def resolve(name: QualifiedName,
              imports: ImportScope): List[SymbolEntry[A]] = {
    val identifier = name.name

    val qualifier = name match {
      case QualifiedName.Full(q, _) => Some(q)
      case QualifiedName.Partial(q, _) => q
    }

    qualifier match {
      case None =>
        // 1. Search local scopes from innermost to outermost
        var localMatch = Option.empty[List[SymbolEntry[A]]]
        val localScopesIter = localScopes.iterator
        while (localScopesIter.hasNext && localMatch.isEmpty) {
          val scope = localScopesIter.next()
          scope.get(identifier).foreach { value =>
            localMatch = Some(List(SymbolEntry.Local(identifier, value)))
          }
        }

        localMatch.getOrElse {
          // 2. Global resolution (Specific Imports, Root Package, Wildcard Imports)
          resolveGlobal(name, imports)
        }

      case _ =>
        // Partially or fully qualified lookup
        resolveGlobal(name, imports)
    }
  }

  def add(name: QualifiedName.Full, value: A): SymbolTable[A] = {
    new SimpleSymbolTable(localScopes, globals.add(name.qualifier, name.name, value))
  }

  def addLocal(name: Name, value: A): SymbolTable[A] = {
    localScopes match {
      case head :: tail =>
        new SimpleSymbolTable((head + (name -> value)) :: tail, globals)
      case Nil =>
        // If no local scope exists, treat as global/root
        add(QualifiedName.full(PackagePath.root, name), value)
    }
  }

  def enterScope: SymbolTable[A] =
    new SimpleSymbolTable(Map.empty[Name, A] :: localScopes, globals)

  def toSymbolIndex: SymbolIndex[A] = new SimpleSymbolIndex(globals)
}

private[symbols] object SimpleSymbolTable {
  def empty[A]: SimpleSymbolTable[A] = new SimpleSymbolTable(Nil, PackageNode.empty[A])
}
