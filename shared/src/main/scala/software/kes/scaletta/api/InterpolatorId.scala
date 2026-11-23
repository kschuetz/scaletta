package software.kes.scaletta.api

object InterpolatorId {
  def apply(value: Int): TypeId = new TypeId(value)
}

final class InterpolatorId(val value: Int) extends AnyVal {
  override def toString = s"InterpolatorId($value)"
}
