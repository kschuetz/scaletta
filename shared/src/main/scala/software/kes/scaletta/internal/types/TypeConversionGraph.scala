package software.kes.scaletta.internal.types

import software.kes.scaletta.api.{Type, TypeId}
import software.kes.scaletta.internal.runtime.CoreTypes

object TypeConversionGraph {
  def conversionRelation(from: Type[TypeId], to: Type[TypeId]): TypeConversion = {
    if (from == to) TypeConversion.Identity
    else from match {
      case CoreTypes.FloatT =>
        to match {
          case CoreTypes.DoubleT => TypeConversion.Widening1
          case _ => TypeConversion.None
        }
      case CoreTypes.LongT =>
        to match {
          case CoreTypes.DoubleT => TypeConversion.Widening1 // We should prefer double over float for longs
          case CoreTypes.FloatT => TypeConversion.Widening2
          case _ => TypeConversion.None
        }
      case CoreTypes.IntT =>
        to match {
          case CoreTypes.LongT => TypeConversion.Widening1
          case CoreTypes.FloatT => TypeConversion.Widening2
          case CoreTypes.DoubleT => TypeConversion.Widening3
          case _ => TypeConversion.None
        }
      case CoreTypes.ShortT | CoreTypes.CharT =>
        to match {
          case CoreTypes.IntT => TypeConversion.Widening1
          case CoreTypes.LongT => TypeConversion.Widening2
          case CoreTypes.FloatT => TypeConversion.Widening3
          case CoreTypes.DoubleT => TypeConversion.Widening4
          case _ => TypeConversion.None
        }
      case CoreTypes.ByteT =>
        to match {
          case CoreTypes.ShortT => TypeConversion.Widening1
          case CoreTypes.IntT => TypeConversion.Widening2
          case CoreTypes.LongT => TypeConversion.Widening3
          case CoreTypes.FloatT => TypeConversion.Widening4
          case CoreTypes.DoubleT => TypeConversion.Widening5
          case _ => TypeConversion.None
        }
      case _ => TypeConversion.None
    }
  }
}
