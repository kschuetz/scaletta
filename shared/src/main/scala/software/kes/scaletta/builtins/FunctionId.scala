package software.kes.scaletta.builtins

object FunctionId {
  def apply(value: Int): FunctionId = new FunctionId(value)
}

final class FunctionId(val value: Int) extends AnyVal {
  override def toString = s"FunctionId($value)"
}
