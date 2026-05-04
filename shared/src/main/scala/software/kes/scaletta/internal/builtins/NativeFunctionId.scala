package software.kes.scaletta.internal.builtins

object NativeFunctionId {
  def apply(value: Int): NativeFunctionId = new NativeFunctionId(value)
}

final class NativeFunctionId(val value: Int) extends AnyVal {
  override def toString = s"NativeFunctionId($value)"
}
