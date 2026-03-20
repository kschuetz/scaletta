package software.kes.scaletta.symbols

import software.kes.scaletta.common.{PackagePath, PackageSegment}

sealed trait QualifiedName {
  def name: String
}

object QualifiedName {

  /**
   * Can throw IllegalArgumentException if the path is invalid.
   * Use tryParseFull() if you want to handle errors.
   */
  def full(path: String): Full =
    tryParseFull(path) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  def full(qualifier: PackagePath.Absolute, name: String): Full =
    Full(qualifier, name)

  /**
   * Can throw IllegalArgumentException if the path is invalid.
   * Use tryParsePartial() if you want to handle errors.
   */
  def partial(path: String): Partial =
    tryParsePartial(path) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  def local(name: String): Partial =
    Partial(None, name)

  def tryParsePartial(path: String): Either[String, Partial] = {
    val trimmed = path.trim
    val lastDot = trimmed.lastIndexOf('.')
    if (lastDot == -1) {
      PackageSegment.parse(trimmed).map(name => Partial(None, name.name))
    } else {
      val (qualifierStr, nameStr) = trimmed.splitAt(lastDot)
      for {
        name <- PackageSegment.parse(nameStr.tail)
        qualifier <- PackagePath.tryParse(qualifierStr)
      } yield Partial(Some(qualifier), name.name)
    }
  }

  def tryParseFull(path: String): Either[String, Full] = {
    val trimmed = path.trim
    val lastDot = trimmed.lastIndexOf('.')
    if (lastDot == -1) {
      PackageSegment.parse(trimmed).map(name => Full(PackagePath.root, name.name))
    } else {
      val (qualifierStr, nameStr) = trimmed.splitAt(lastDot)
      for {
        name <- PackageSegment.parse(nameStr.tail)
        qualifier <- PackagePath.tryParseAbsolute(qualifierStr)
      } yield Full(qualifier, name.name)
    }
  }

  case class Full(qualifier: PackagePath.Absolute,
                  name: String) extends QualifiedName

  /**
   * @param qualifier if None, it's a local name. If Some, the package path can be either Absolute or Relative.
   */
  case class Partial(qualifier: Option[PackagePath],
                     name: String) extends QualifiedName
}
