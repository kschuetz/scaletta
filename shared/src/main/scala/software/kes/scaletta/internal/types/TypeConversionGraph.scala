package software.kes.scaletta.internal.types

import software.kes.scaletta.api.{Type, TypeId}
import software.kes.scaletta.internal.runtime.CoreTypes

object TypeConversionGraph {
  def conversionRelation(from: Type[TypeId], to: Type[TypeId]): TypeConversion = {
    if (from == to) TypeConversion.Identity
    else from match {
      case CoreTypes.FloatT =>
        to match {
          case CoreTypes.DoubleT => TypeConversion.Widening
          case _ => TypeConversion.None
        }
      case CoreTypes.LongT =>
        to match {
          case CoreTypes.FloatT | CoreTypes.DoubleT => TypeConversion.Widening
          case _ => TypeConversion.None
        }
      case CoreTypes.IntT =>
        to match {
          case CoreTypes.LongT | CoreTypes.FloatT | CoreTypes.DoubleT => TypeConversion.Widening
          case _ => TypeConversion.None
        }
      case CoreTypes.ShortT =>
        to match {
          case CoreTypes.IntT | CoreTypes.LongT | CoreTypes.FloatT | CoreTypes.DoubleT => TypeConversion.Widening
          case _ => TypeConversion.None
        }
      case CoreTypes.CharT =>
        to match {
          case CoreTypes.IntT | CoreTypes.LongT | CoreTypes.FloatT | CoreTypes.DoubleT => TypeConversion.Widening
          case _ => TypeConversion.None
        }
      case CoreTypes.ByteT =>
        to match {
          case CoreTypes.ShortT | CoreTypes.IntT | CoreTypes.LongT | CoreTypes.FloatT | CoreTypes.DoubleT => TypeConversion.Widening
          case _ => TypeConversion.None
        }
      case _ => TypeConversion.None
    }
  }
}
