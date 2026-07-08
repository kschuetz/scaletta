package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.{BasicType, BasicTypes}

object LazyCell {
  final val NotEvaluated: Byte = 0
  final val Evaluating: Byte = 1
  final val Evaluated: Byte = 2

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
  def state: Byte

  def evaluated: Boolean = state == LazyCell.Evaluated

  def evaluating: Boolean = state == LazyCell.Evaluating

  def markEvaluating(): Unit

  /**
   * Reads the value from the top of the operand stack (without popping it)
   * and stores the value in this cell.
   */
  def update(operandStack: OperandStack): Unit

  def pushValue(operandStack: OperandStack): Unit
}

final class LazyObject(var state: Byte = LazyCell.NotEvaluated,
                       var value: AnyRef = null) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadObject(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushObject(value)
}

final class LazyBoolean(var state: Byte = LazyCell.NotEvaluated,
                        var value: Boolean = false) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadBoolean(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushBoolean(value)
}

final class LazyInt(var state: Byte = LazyCell.NotEvaluated,
                    var value: Int = 0) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadInt(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushInt(value)
}

final class LazyLong(var state: Byte = LazyCell.NotEvaluated,
                     var value: Long = 0L) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadLong(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushLong(value)
}

final class LazyShort(var state: Byte = LazyCell.NotEvaluated,
                      var value: Short = 0) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadShort(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushShort(value)
}

final class LazyByte(var state: Byte = LazyCell.NotEvaluated,
                     var value: Byte = 0) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadByte(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushByte(value)
}

final class LazyChar(var state: Byte = LazyCell.NotEvaluated,
                     var value: Char = '\u0000') extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadChar(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushChar(value)
}

final class LazyDouble(var state: Byte = LazyCell.NotEvaluated,
                       var value: Double = 0.0) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadDouble(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushDouble(value)
}

final class LazyFloat(var state: Byte = LazyCell.NotEvaluated,
                      var value: Float = 0.0f) extends LazyCell {
  def markEvaluating(): Unit = {
    state = LazyCell.Evaluating
  }

  def update(operandStack: OperandStack): Unit = {
    value = operandStack.unsafeReadFloat(0)
    state = LazyCell.Evaluated
  }

  def pushValue(operandStack: OperandStack): Unit =
    operandStack.pushFloat(value)
}
