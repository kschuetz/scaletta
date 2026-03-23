package software.kes.scaletta.common

import software.kes.scaletta.symbols.{Name, QualifiedName}

sealed trait PackagePath {
  def components: Vector[PackageSegment]

  def isAbsolute: Boolean

  def isRelative: Boolean = !isAbsolute

  /**
   * Appends a single component to this path.
   */
  def /(segment: PackageSegment): PackagePath

  /**
   * Returns a QualifiedName representing a member of this path.
   */
  def qualify(name: Name): QualifiedName

  /**
   * Concatenates this path with a relative path.
   */
  def ++(other: PackagePath.Relative): PackagePath

  /**
   * Returns a string representation of the path.
   * Absolute paths do not start with "_root_", except for the root package itself.
   */
  def fullName: String
}

object PackagePath {
  final val RootPrefix = "_root_"

  final class Absolute private[PackagePath](val components: Vector[PackageSegment]) extends PackagePath {
    def isAbsolute: Boolean = true

    def /(segment: PackageSegment): Absolute =
      new Absolute(this.components :+ segment)

    def qualify(name: Name): QualifiedName.Full =
      QualifiedName.full(this, name)

    def ++(other: Relative): Absolute =
      new Absolute(this.components ++ other.components)

    lazy val fullName: String =
      if (components.isEmpty) RootPrefix
      else components.map(_.name.value).mkString(".")

    override def toString: String = fullName

    override def equals(other: Any): Boolean = other match {
      case that: Absolute =>
        components == that.components
      case _ => false
    }

    override def hashCode(): Int =
      components.foldLeft(0)((a, b) => 31 * a + b.hashCode())
  }

  final class Relative private[PackagePath](val components: Vector[PackageSegment]) extends PackagePath {
    def isAbsolute: Boolean = false

    def /(segment: PackageSegment): Relative =
      new Relative(this.components :+ segment)

    def qualify(name: Name): QualifiedName.Partial =
      QualifiedName.Partial(Some(this), name)

    def ++(other: Relative): Relative =
      new Relative(this.components ++ other.components)

    lazy val fullName: String = components.map(_.name.value).mkString(".")

    override def toString: String = fullName

    override def equals(other: Any): Boolean = other match {
      case that: Relative =>
        components == that.components
      case _ => false
    }

    override def hashCode(): Int =
      components.foldLeft(0)((a, b) => 31 * a + b.hashCode())
  }

  /**
   * Creates the root absolute path.
   */
  val root: Absolute = new Absolute(Vector.empty)

  /**
   * Creates an absolute path from components.
   */
  def absolute(components: PackageSegment*): Absolute = new Absolute(components.toVector)

  /**
   * Creates a relative path from components.
   */
  def relative(components: PackageSegment*): Relative = new Relative(components.toVector)

  /**
   * Parses a string into an absolute PackagePath.
   * If the path is invalid, an IllegalArgumentException is thrown.
   * Use tryParseAbsolute() if you want to handle errors.
   */
  def parseAbsolute(input: String): Absolute =
    tryParseAbsolute(input) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  /**
   * Parses a string into an absolute PackagePath.
   * The path MAY start with "_root_". If it does, "_root_" will be ignored.
   */
  def tryParseAbsolute(input: String): Either[String, Absolute] = {
    val trimmed = input.trim
    if (trimmed == RootPrefix) {
      Right(root)
    } else if (trimmed.startsWith(s"$RootPrefix.")) {
      val remaining = trimmed.stripPrefix(s"$RootPrefix.")
      if (remaining.isEmpty) {
        Left(s"Absolute path must have components after '$RootPrefix.'")
      } else {
        parseComponents(remaining).map(new Absolute(_))
      }
    } else {
      parseComponents(trimmed).map(new Absolute(_))
    }
  }

  /**
   * Parses a string into a relative PackagePath.
   * If the path is invalid, an IllegalArgumentException is thrown.
   * Use tryParseRelative() if you want to handle errors.
   */
  def parseRelative(input: String): Relative =
    tryParseRelative(input) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  /**
   * Parses a string into a relative PackagePath.
   */
  def tryParseRelative(input: String): Either[String, Relative] = {
    val trimmed = input.trim
    if (trimmed.isEmpty) {
      Right(new Relative(Vector.empty))
    } else if (trimmed.startsWith(RootPrefix)) {
      Left(s"Relative path cannot start with '$RootPrefix'")
    } else {
      parseComponents(trimmed).map(new Relative(_))
    }
  }

  /**
   * Parses a string into a PackagePath.
   * If it starts with "_root_", it is absolute. Otherwise, it's relative.
   */
  def tryParse(input: String): Either[String, PackagePath] = {
    val trimmed = input.trim
    if (trimmed.startsWith(RootPrefix)) {
      tryParseAbsolute(trimmed)
    } else {
      tryParseRelative(trimmed)
    }
  }

  private def parseComponents(input: String): Either[String, Vector[PackageSegment]] = {
    val parts = input.split('.').toVector
    val results = parts.map(PackageSegment.parse)

    val (errors, names) = results.foldLeft((Vector.empty[String], Vector.empty[PackageSegment])) { (acc, res) =>
      res match {
        case Left(err) => (acc._1 :+ err, acc._2)
        case Right(nm) => (acc._1, acc._2 :+ nm)
      }
    }

    if (errors.nonEmpty) {
      Left(errors.mkString("; "))
    } else {
      Right(names)
    }
  }
}
