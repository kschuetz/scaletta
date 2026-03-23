package software.kes.scaletta.symbols

object Identifier {
  /**
   * Creates an Identifier from a string.
   * If the name is invalid, an IllegalArgumentException is thrown.
   * Use tryParse() if you want to handle errors.
   */
  def apply(name: String): Identifier =
    tryParse(name) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  def tryParse(name: String): Either[String, Identifier] =
    if (name.isEmpty) Left("Identifier cannot be empty")
    else Right(new Identifier(name))

  def unapply(arg: Identifier): Option[String] = Some(arg.name)
}

final class Identifier private(val name: String) extends AnyVal {
  override def toString: String = name
}
