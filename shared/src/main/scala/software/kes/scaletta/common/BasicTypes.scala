package software.kes.scaletta.common

import software.kes.scaletta.api.{Type, TypeId}
import software.kes.scaletta.internal.runtime.CoreTypes

object BasicTypes {
  final val Object: BasicType = 0
  final val Boolean: BasicType = 1
  final val Int: BasicType = 2
  final val Long: BasicType = 3
  final val Short: BasicType = 4
  final val Byte: BasicType = 5
  final val Char: BasicType = 6
  final val Double: BasicType = 7
  final val Float: BasicType = 8

  def fromType(typ: Type[TypeId]): BasicType =
    typ match {
      case CoreTypes.BooleanT => Boolean
      case CoreTypes.IntT => Int
      case CoreTypes.LongT => Long
      case CoreTypes.ShortT => Short
      case CoreTypes.ByteT => Byte
      case CoreTypes.CharT => Char
      case CoreTypes.DoubleT => Double
      case CoreTypes.FloatT => Float
      case _ => Object
    }

  def friendlyName(value: BasicType): String =
    value match {
      case Boolean => "Boolean"
      case Int => "Int"
      case Long => "Long"
      case Short => "Short"
      case Byte => "Byte"
      case Char => "Char"
      case Double => "Double"
      case Float => "Float"
      case _ => "Object"
    }

  private[scaletta] val MaxValue: BasicType = Float
}

