package software.kes.scaletta.api

object NativeFunctionId {
  def apply(value: Int): NativeFunctionId = new NativeFunctionId(value)
}

final class NativeFunctionId(val value: Int) extends AnyVal {
  override def toString = s"NativeFunctionId($value)"
}
