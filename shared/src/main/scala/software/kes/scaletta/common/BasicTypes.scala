package software.kes.scaletta.common

import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.types.{Type, TypeId}

object BasicTypes {
  final val Object: Byte = 0
  final val Boolean: Byte = 1
  final val Int: Byte = 2
  final val Long: Byte = 3
  final val Short: Byte = 4
  final val Byte: Byte = 5
  final val Char: Byte = 6
  final val Double: Byte = 7
  final val Float: Byte = 8

  def fromType(typ: Type[TypeId]): Byte =
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

  def friendlyName(value: Byte): String =
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

  private[scaletta] val MaxValue: Byte = Float
}

