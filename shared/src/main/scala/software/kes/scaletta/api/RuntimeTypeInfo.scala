package software.kes.scaletta.api

object RuntimeTypeInfo {
  val any: RuntimeTypeInfo = RuntimeTypeInfo(_ => true)
}

case class RuntimeTypeInfo(isInstance: Any => Boolean,
                           unapplyStrategy: UnapplyStrategy = UnapplyStrategy.noUnapply) {
  def withUnapplyStrategy(unapplyStrategy: UnapplyStrategy): RuntimeTypeInfo =
    copy(unapplyStrategy = unapplyStrategy)
}
