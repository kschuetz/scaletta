package software.kes.scaletta.api

object InterpolatorId {
  def apply(value: Int): InterpolatorId = new InterpolatorId(value)
}

final class InterpolatorId(val value: Int) extends AnyVal {
  override def toString = s"InterpolatorId($value)"
}
