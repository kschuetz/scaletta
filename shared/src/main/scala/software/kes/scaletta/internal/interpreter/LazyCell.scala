package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.{BasicType, BasicTypes}

object LazyCell {
  def create(typ: BasicType): LazyCell =
    typ match {
      case BasicTypes.Boolean => boolean()
      case BasicTypes.Int => int()
      case BasicTypes.Long => long()
      case BasicTypes.Short => short()
      case BasicTypes.Byte => byte()
      case BasicTypes.Char => char()
      case BasicTypes.Double => double()
      case BasicTypes.Float => float()
      case _ => object_()
    }

  def object_(): LazyObject = new LazyObject()

  def boolean(): LazyBoolean = new LazyBoolean()

  def int(): LazyInt = new LazyInt()

  def long(): LazyLong = new LazyLong()

  def short(): LazyShort = new LazyShort()

  def byte(): LazyByte = new LazyByte()

  def char(): LazyChar = new LazyChar()

  def double(): LazyDouble = new LazyDouble()

  def float(): LazyFloat = new LazyFloat()
}

trait LazyCell {
  def evaluated: Boolean
}

final class LazyObject(var evaluated: Boolean = false,
                       var value: AnyRef = null) extends LazyCell

final class LazyBoolean(var evaluated: Boolean = false,
                        var value: Boolean = false) extends LazyCell

final class LazyInt(var evaluated: Boolean = false,
                    var value: Int = 0) extends LazyCell

final class LazyLong(var evaluated: Boolean = false,
                     var value: Long = 0L) extends LazyCell

final class LazyShort(var evaluated: Boolean = false,
                      var value: Short = 0) extends LazyCell

final class LazyByte(var evaluated: Boolean = false,
                     var value: Byte = 0) extends LazyCell

final class LazyChar(var evaluated: Boolean = false,
                     var value: Char = '\u0000') extends LazyCell

final class LazyDouble(var evaluated: Boolean = false,
                       var value: Double = 0.0) extends LazyCell

final class LazyFloat(var evaluated: Boolean = false,
                      var value: Float = 0.0f) extends LazyCell
