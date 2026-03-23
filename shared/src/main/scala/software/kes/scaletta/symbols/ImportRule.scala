package software.kes.scaletta.symbols

import software.kes.scaletta.common.PackagePath

sealed trait ImportRule

object ImportRule {
  /**
   * Represents "import foo.bar.baz"
   * Allows "foo.bar.baz.Quux" to be referenced as "baz.Quux".
   */
  final case class Package(path: PackagePath.Absolute) extends ImportRule

  /**
   * Represents "import foo.bar.baz.{Quux, Corge}"
   * Allows these symbols to be referenced without qualification.
   */
  final case class Symbols(path: PackagePath.Absolute,
                           names: Set[Name]) extends ImportRule

  /**
   * Represents "import foo.bar._"
   * Allows any symbol in "foo.bar" to be referenced without qualification.
   */
  final case class Wildcard(path: PackagePath.Absolute) extends ImportRule
}
