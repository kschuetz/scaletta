package software.kes.scaletta.symbols

/**
 * A stub representing the set of active implicit imports in a given scope.
 * Currently represents an empty import scope.
 */
final case class ImportScope()

object ImportScope {
  /**
   * Returns an empty ImportScope.
   */
  val empty: ImportScope = ImportScope()
}
