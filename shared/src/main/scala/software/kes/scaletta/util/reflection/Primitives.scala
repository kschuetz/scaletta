package software.kes.scaletta.util.reflection

object Primitives {
  def isAnyVal(value: Any): Boolean =
    PrimitivesPlatform.isAnyVal(value)
}
