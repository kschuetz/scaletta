package software.kes.scaletta.symbols

import software.kes.scaletta.common.PackagePath

object ImportScope {
  /**
   * Returns an empty ImportScope.
   */
  val empty: ImportScope = new ImportScope(Map.empty, Set.empty, Map.empty)

  def importPackage(paths: PackagePath.Absolute*): ImportScope =
    empty.importPackage(paths: _*)

  def importSymbol(names: QualifiedName.Full*): ImportScope =
    empty.importSymbol(names: _*)

  def importSymbols(path: PackagePath.Absolute, names: Set[String]): ImportScope =
    empty.importSymbols(path, names)

  def importWildcard(paths: PackagePath.Absolute*): ImportScope =
    empty.importWildcard(paths: _*)

  def fromRules(rule: ImportRule*): ImportScope =
    rule.foldLeft(empty)(_.add(_))

  def merge(scopes: ImportScope*): ImportScope =
    scopes.foldLeft(empty)(_.merge(_))

}

/**
 * A structure representing the set of active implicit imports in a given scope.
 *
 * symbols: Maps a local name (e.g., "Quux") to its fully qualified package (e.g., "foo.bar.baz")
 * wildcards: A set of packages where all members are visible without qualification
 * packages: Maps the last segment of an imported package (e.g., "baz") to the full path (e.g., "foo.bar.baz")
 */
final class ImportScope private(val symbols: Map[String, PackagePath.Absolute],
                                val wildcards: Set[PackagePath.Absolute],
                                val packages: Map[String, PackagePath.Absolute]) {

  def importPackage(paths: PackagePath.Absolute*): ImportScope = {
    paths.foldLeft(this) { (acc, path) =>
      path.components.lastOption match {
        case Some(segment) =>
          new ImportScope(acc.symbols, acc.wildcards, acc.packages + (segment.name -> path))
        case None => acc // Ignore root package as an explicit package import
      }
    }
  }

  def importSymbol(names: QualifiedName.Full*): ImportScope = {
    names.foldLeft(this) {
      case (acc, name) => acc.importSymbols(name.qualifier, Set(name.name))
    }
  }

  def importSymbols(path: PackagePath.Absolute, names: Set[String]): ImportScope = {
    // Only add specific symbols if the package is NOT already covered by a wildcard
    if (wildcards.contains(path)) {
      this
    } else {
      val newSymbols = names.foldLeft(symbols)((acc, name) => acc + (name -> path))
      new ImportScope(newSymbols, wildcards, packages)
    }
  }

  def importWildcard(paths: PackagePath.Absolute*): ImportScope = {
    paths.foldLeft(this) { (acc, path) =>
      // Remove any existing specific symbols that belong to this package, as they are now redundant
      val filteredSymbols = acc.symbols.filter { case (_, pkgPath) => pkgPath != path }
      new ImportScope(filteredSymbols, acc.wildcards + path, acc.packages)
    }
  }

  def add(rule: ImportRule): ImportScope = rule match {
    case ImportRule.Package(path) => importPackage(path)
    case ImportRule.Symbols(path, names) => importSymbols(path, names)
    case ImportRule.Wildcard(path) => importWildcard(path)
  }

  def merge(other: ImportScope): ImportScope = {
    val newWildcards = this.wildcards ++ other.wildcards
    val newPackages = this.packages ++ other.packages
    val mergedSymbols = this.symbols ++ other.symbols
    val filteredSymbols = mergedSymbols.filter { case (_, pkgPath) => !newWildcards.contains(pkgPath) }
    new ImportScope(filteredSymbols, newWildcards, newPackages)
  }

  override def equals(other: Any): Boolean = other match {
    case that: ImportScope =>
      symbols == that.symbols &&
        wildcards == that.wildcards &&
        packages == that.packages
    case _ => false
  }

  override def hashCode(): Int = {
    var result = symbols.hashCode()
    result = 31 * result + wildcards.hashCode()
    result = 31 * result + packages.hashCode()
    result
  }

  override def toString: String =
    s"ImportScope(symbols=$symbols, wildcards=$wildcards, packages=$packages)"
}


