package software.kes.scaletta.util.conversions

object ObjectToPrimitive {
  def objectToBoolean(obj: AnyRef): Boolean = obj match {
    case value: java.lang.Boolean => value.booleanValue()
    case _ => false
  }

  def objectToInt(obj: AnyRef): Int = obj match {
    case number: java.lang.Number => number.intValue()
    case char: java.lang.Character => char.charValue()
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }

  def objectToLong(obj: AnyRef): Long = obj match {
    case number: java.lang.Number => number.longValue()
    case char: java.lang.Character => char.charValue()
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }

  def objectToShort(obj: AnyRef): Short = obj match {
    case number: java.lang.Number => number.shortValue()
    case char: java.lang.Character => char.charValue().toShort
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }

  def objectToByte(obj: AnyRef): Byte = obj match {
    case number: java.lang.Number => number.byteValue()
    case char: java.lang.Character => char.charValue().toByte
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }

  def objectToChar(obj: AnyRef): Char = obj match {
    case number: java.lang.Number => number.intValue().toChar
    case char: java.lang.Character => char.charValue()
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }

  def objectToDouble(obj: AnyRef): Double = obj match {
    case number: java.lang.Number => number.doubleValue()
    case char: java.lang.Character => char.charValue().toDouble
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }

  def objectToFloat(obj: AnyRef): Float = obj match {
    case number: java.lang.Number => number.floatValue()
    case char: java.lang.Character => char.charValue().toFloat
    case boolean: java.lang.Boolean => if (boolean.booleanValue()) 1 else 0
    case _ => 0
  }
}
