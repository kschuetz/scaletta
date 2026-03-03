package software.kes.scaletta.common

object TypeName {
  def apply(value: String): TypeName = new TypeName(value)
}

final class TypeName(val value: String) extends AnyVal {
  override def toString: String = value
}
