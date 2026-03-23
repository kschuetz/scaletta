package software.kes.scaletta.symbols

import software.kes.scaletta.common.{PackagePath, PackageSegment}

sealed trait QualifiedName {
  def name: Name
}

object QualifiedName {

  /**
   * Can throw IllegalArgumentException if the path is invalid.
   * Use tryParseFull() if you want to handle errors.
   */
  def parseFull(path: String): Full =
    tryParseFull(path) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  def full(qualifier: PackagePath.Absolute, name: Name): Full =
    Full(qualifier, name)

  /**
   * Can throw IllegalArgumentException if the path is invalid.
   * Use tryParsePartial() if you want to handle errors.
   */
  def parsePartial(path: String): Partial =
    tryParsePartial(path) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  def partial(qualifier: PackagePath.Relative): Partial =
    Partial(Some(qualifier), qualifier.components.last.toName)

  def local(name: Name): Partial =
    Partial(None, name)

  def tryParseFull(path: String): Either[String, Full] = {
    val trimmed = path.trim
    val lastDot = trimmed.lastIndexOf('.')
    if (lastDot == -1) {
      PackageSegment.parse(trimmed).map(name => Full(PackagePath.root, name.toName))
    } else {
      val (qualifierStr, nameStr) = trimmed.splitAt(lastDot)
      for {
        name <- PackageSegment.parse(nameStr.tail)
        qualifier <- PackagePath.tryParseAbsolute(qualifierStr)
      } yield Full(qualifier, Name(name.name))
    }
  }

  def tryParsePartial(path: String): Either[String, Partial] = {
    val trimmed = path.trim
    val lastDot = trimmed.lastIndexOf('.')
    if (lastDot == -1) {
      PackageSegment.parse(trimmed).map(name => Partial(None, name.toName))
    } else {
      val (qualifierStr, nameStr) = trimmed.splitAt(lastDot)
      for {
        name <- PackageSegment.parse(nameStr.tail)
        qualifier <- PackagePath.tryParse(qualifierStr)
      } yield Partial(Some(qualifier), name.toName)
    }
  }

  case class Full(qualifier: PackagePath.Absolute,
                  name: Name) extends QualifiedName

  /**
   * @param qualifier if None, it's a local name. If Some, the package path can be either Absolute or Relative.
   */
  case class Partial(qualifier: Option[PackagePath],
                     name: Name) extends QualifiedName
}
