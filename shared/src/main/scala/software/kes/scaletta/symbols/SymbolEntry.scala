package software.kes.scaletta.symbols

import software.kes.scaletta.common.PackagePath

/**
 * Represents a resolved entry in the symbol table.
 *
 * @tparam A The type of the stored value.
 */
sealed trait SymbolEntry[+A] {
  def name: String

  def value: A

  def container: Option[PackagePath.Absolute]
}

object SymbolEntry {
  final case class Global[+A](name: String,
                              qualifier: PackagePath.Absolute,
                              value: A) extends SymbolEntry[A] {
    def container: Option[PackagePath.Absolute] = Some(qualifier)
  }

  final case class Local[+A](name: String,
                             value: A) extends SymbolEntry[A] {
    def container: Option[PackagePath.Absolute] = None
  }
}
