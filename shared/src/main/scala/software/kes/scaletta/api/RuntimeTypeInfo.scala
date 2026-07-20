package software.kes.scaletta.api

case class RuntimeTypeInfo(isInstance: Any => Boolean,
                           unapplyStrategy: UnapplyStrategy = UnapplyStrategy.noUnapply) {
  def withUnapplyStrategy(unapplyStrategy: UnapplyStrategy): RuntimeTypeInfo =
    copy(unapplyStrategy = unapplyStrategy)
}
