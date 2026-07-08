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

  /**
   * Reads the value from the top of the operand stack (without popping it)
   * and stores the value in this cell.
   */
  def update(operandStack: OperandStack): Unit

  def pushValue(operandStack: OperandStack): Unit
}

final class LazyObject(var evaluated: Boolean = false,
                       var value: AnyRef = null) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadObject(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushObject(value)
}

final class LazyBoolean(var evaluated: Boolean = false,
                        var value: Boolean = false) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadBoolean(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushBoolean(value)
}

final class LazyInt(var evaluated: Boolean = false,
                    var value: Int = 0) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadInt(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushInt(value)
}

final class LazyLong(var evaluated: Boolean = false,
                     var value: Long = 0L) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadLong(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushLong(value)
}

final class LazyShort(var evaluated: Boolean = false,
                      var value: Short = 0) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadShort(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushShort(value)
}

final class LazyByte(var evaluated: Boolean = false,
                     var value: Byte = 0) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadByte(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushByte(value)
}

final class LazyChar(var evaluated: Boolean = false,
                     var value: Char = '\u0000') extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadChar(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushChar(value)
}

final class LazyDouble(var evaluated: Boolean = false,
                       var value: Double = 0.0) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadDouble(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushDouble(value)
}

final class LazyFloat(var evaluated: Boolean = false,
                      var value: Float = 0.0f) extends LazyCell {
  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadFloat(0)
    evaluated = true
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushFloat(value)
}
