package software.kes.scaletta.types

object TypeId {
  def apply(value: Int): TypeId = new TypeId(value)
}

final class TypeId(val value: Int) extends AnyVal {
  override def toString = s"TypeId($value)"
}
