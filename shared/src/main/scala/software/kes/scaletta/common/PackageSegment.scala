package software.kes.scaletta.common

import software.kes.scaletta.internal.scanner.CharacterClass.{isIdentifierInner, isIdentifierStart}
import software.kes.scaletta.internal.scanner.Token
import software.kes.scaletta.symbols.Name

object PackageSegment {
  /**
   * Creates a PackageSegment from a string.
   * If the name is invalid, an IllegalArgumentException is thrown.
   * Use parse() if you want to handle errors.
   */
  def apply(name: String): PackageSegment =
    parse(name) match {
      case Right(result) => result
      case Left(error) => throw new IllegalArgumentException(error)
    }

  def parse(name: String): Either[String, PackageSegment] = {
    if (name.isEmpty) {
      Left("Package name component cannot be empty")
    } else if (Token.reservedWordByName.contains(name)) {
      Left(s"'$name' is a reserved keyword and cannot be used as a package name component")
    } else {
      val firstChar = name.charAt(0)
      if (isIdentifierStart(firstChar) && name.forall(isIdentifierInner)) {
        Right(new PackageSegment(Name(name)))
      } else {
        Left(s"Invalid package name component: '$name'")
      }
    }
  }

  // For use in tests only
  private[common] def _unsafeCreate(name: String): PackageSegment =
    new PackageSegment(Name(name))
}

final class PackageSegment private(val name: Name) {
  override def equals(other: Any): Boolean = other match {
    case that: PackageSegment =>
      name == that.name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode()

  override def toString: String = name.toString
}
