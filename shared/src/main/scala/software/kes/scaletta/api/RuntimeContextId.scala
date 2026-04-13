package software.kes.scaletta.api

object RuntimeContextId {
  def apply(value: Int): RuntimeContextId = new RuntimeContextId(value)
}

final class RuntimeContextId(val value: Int) extends AnyVal {
  override def toString = s"RuntimeContextId($value)"
}
