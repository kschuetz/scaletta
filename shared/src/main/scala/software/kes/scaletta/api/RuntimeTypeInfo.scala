package software.kes.scaletta.api

object RuntimeTypeInfo {
  val any: RuntimeTypeInfo = RuntimeTypeInfo(_ => true)
}

case class RuntimeTypeInfo(isInstance: Any => Boolean,
                           unapplyStrategy: UnapplyStrategy = UnapplyStrategy.noUnapply,
                           patternTypeInfo: Option[PatternTypeInfo] = None) {
  def withUnapplyStrategy(unapplyStrategy: UnapplyStrategy): RuntimeTypeInfo =
    copy(unapplyStrategy = unapplyStrategy)

  def withPatternTypeInfo(patternTypeInfo: PatternTypeInfo): RuntimeTypeInfo =
    copy(patternTypeInfo = Some(patternTypeInfo))
}

sealed trait PatternTypeInfo

object PatternTypeInfo {
  final case class Product(fields: Vector[ProperType[TypeId]]) extends PatternTypeInfo

  final case class Sum(
                        constructors: Vector[ProperType[TypeId]],
                        labels: Vector[String] = Vector.empty
                      ) extends PatternTypeInfo

  case object BooleanDomain extends PatternTypeInfo
}
