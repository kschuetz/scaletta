package software.kes.scaletta.api

object Name {
  /**
   * Creates an Name from a string.
   * If the name is invalid, an IllegalArgumentException is thrown.
   * Use tryParse() if you want to handle errors.
   */
  def apply(name: String): Name =
    tryParse(name) match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(result) => result
    }

  def tryParse(name: String): Either[String, Name] =
    if (name.isEmpty) Left("Name cannot be empty")
    else Right(new Name(name))

  def unapply(arg: Name): Option[String] = Some(arg.value)
}

final class Name private(val value: String) extends AnyVal {
  override def toString: String = value
}
