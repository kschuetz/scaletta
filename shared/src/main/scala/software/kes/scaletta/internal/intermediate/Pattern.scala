package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.api.RuntimeTypeInfo
import software.kes.scaletta.util.VectorTwoPlus

sealed trait Pattern

object Pattern {
  case object Wildcard extends Pattern

  case class Literal(value: IntermediateExpression.Value) extends Pattern

  case class Slot(scope: Int, slot: Int) extends Pattern

  case class Typed(inner: Pattern, runtimeType: RuntimeTypeInfo) extends Pattern

  case class Tuple(elements: VectorTwoPlus[Pattern]) extends Pattern

  case class Product(runtimeType: RuntimeTypeInfo, args: Vector[Pattern]) extends Pattern

  case class As(scope: Int, slot: Int, inner: Pattern) extends Pattern
}
