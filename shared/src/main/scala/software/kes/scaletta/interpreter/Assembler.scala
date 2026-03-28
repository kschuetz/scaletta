package software.kes.scaletta.interpreter

final class Assembler(emitter: OpcodeEmitter,
                      interner: ConstantInterner) {

  def pushImmediate(value: Any): Unit =
    value match {
      case null => pushNull()
      case x: Boolean => pushImmediateBoolean(x)
      case x: Int => pushImmediateInt(x)
      case x: Long => pushImmediateLong(x)
      case x: Short => pushImmediateShort(x)
      case x: Byte => pushImmediateByte(x)
      case x: Char => pushImmediateChar(x)
      case x: Double => pushImmediateDouble(x)
      case x: Float => pushImmediateFloat(x)
      case x: AnyRef => pushImmediateObject(x)
    }

  def pushImmediateInt(value: Int): Unit =
    if (value >= Short.MinValue && value <= Short.MaxValue) {
      pushConst(Opcodes.Types.Int, value.toShort)
    } else push(Opcodes.Types.Int, value)

  def pushImmediateLong(value: Long): Unit =
    if (value >= Short.MinValue && value <= Short.MaxValue) {
      pushConst(Opcodes.Types.Long, value.toShort)
    } else push(Opcodes.Types.Long, interner.internLong(value))

  def pushImmediateBoolean(value: Boolean): Unit =
    pushConst(Opcodes.Types.Boolean, if (value) 1 else 0)

  def pushImmediateByte(value: Byte): Unit =
    pushConst(Opcodes.Types.Byte, value)

  def pushImmediateShort(value: Short): Unit =
    pushConst(Opcodes.Types.Short, value)

  def pushImmediateDouble(value: Double): Unit =
    if (value.isWhole) {
      if (value >= Short.MinValue && value <= Short.MaxValue) {
        pushConst(Opcodes.Types.Double, value.toShort)
      } else {
        push(Opcodes.Types.Double, interner.internDouble(value))
      }
    } else push(Opcodes.Types.Double, interner.internDouble(value))

  def pushImmediateFloat(value: Float): Unit =
    if (value.isWhole) {
      if (value >= Short.MinValue && value <= Short.MaxValue) {
        pushConst(Opcodes.Types.Float, value.toShort)
      } else {
        push(Opcodes.Types.Float, interner.internFloat(value))
      }
    } else push(Opcodes.Types.Float, interner.internFloat(value))

  def pushImmediateChar(value: Char): Unit =
    if (value >= Short.MinValue && value <= Short.MaxValue) pushConst(Opcodes.Types.Char, value.toShort)
    else push(Opcodes.Types.Char, value)

  def pushImmediateObject(value: AnyRef): Unit =
    if (value == null) pushNull()
    else push(Opcodes.Types.Object, interner.internObject(value))

  def pushNull(): Unit =
    pushConst(Opcodes.Types.Object, 0)

  def pushIntFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Int, varIndex)

  def pushLongFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Long, varIndex)

  def pushDoubleFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Double, varIndex)

  def pushFloatFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Float, varIndex)

  def pushByteFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Byte, varIndex)

  def pushShortFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Short, varIndex)

  def pushCharFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Char, varIndex)

  def pushBooleanFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Boolean, varIndex)

  def pushObjectFromVar(varIndex: Int): Unit =
    pushFromVar(Opcodes.Types.Object, varIndex)

  def popIntIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Int, varIndex)

  def popLongIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Long, varIndex)

  def popDoubleIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Double, varIndex)

  def popFloatIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Float, varIndex)

  def popByteIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Byte, varIndex)

  def popShortIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Short, varIndex)

  def popCharIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Char, varIndex)

  def popBooleanIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Boolean, varIndex)

  def popObjectIntoVar(varIndex: Int): Unit =
    popIntoVar(Opcodes.Types.Object, varIndex)

  def storeImmediate(varIndex: Int, value: Any): Unit = value match {
    case null => storeNull(varIndex)
    case x: Boolean => storeImmediateBoolean(varIndex, x)
    case x: Int => storeImmediateInt(varIndex, x)
    case x: Long => storeImmediateLong(varIndex, x)
    case x: Short => storeImmediateShort(varIndex, x)
    case x: Byte => storeImmediateByte(varIndex, x)
    case x: Char => storeImmediateChar(varIndex, x)
    case x: Double => storeImmediateDouble(varIndex, x)
    case x: Float => storeImmediateFloat(varIndex, x)
    case x: AnyRef => storeImmediateObject(varIndex, x)
  }

  def storeImmediateObject(varIndex: Int, value: AnyRef): Unit =
    if (value == null) storeNull(varIndex)
    else store(Opcodes.Types.Object, varIndex, interner.internObject(value), allowConst = false)

  def storeImmediateBoolean(varIndex: Int, value: Boolean): Unit =
    store(Opcodes.Types.Boolean, varIndex, if (value) 1 else 0)

  def storeImmediateInt(varIndex: Int, value: Int): Unit =
    store(Opcodes.Types.Int, varIndex, value)

  def storeImmediateLong(varIndex: Int, value: Long): Unit =
    storeInternable(Opcodes.Types.Int, varIndex, maybeConst(value), interner.internLong(value))

  def storeImmediateShort(varIndex: Int, value: Short): Unit =
    store(Opcodes.Types.Short, varIndex, value)

  def storeImmediateByte(varIndex: Int, value: Byte): Unit =
    store(Opcodes.Types.Byte, varIndex, value)

  def storeImmediateChar(varIndex: Int, value: Char): Unit =
    store(Opcodes.Types.Char, varIndex, value)

  def storeImmediateDouble(varIndex: Int, value: Double): Unit =
    storeInternable(Opcodes.Types.Int, varIndex, maybeConst(value), interner.internDouble(value))

  def storeImmediateFloat(varIndex: Int, value: Float): Unit =
    storeInternable(Opcodes.Types.Int, varIndex, maybeConst(value), interner.internFloat(value))

  def storeNull(varIndex: Int): Unit =
    store(Opcodes.Types.Object, varIndex, 0)

  private def pushConst(typ: Byte, value: Short): Unit = {
    val opcode = makeOpcode(Opcodes.PushConst, typ, value)
    emitter.emit(opcode)
  }

  private def push(typ: Byte, value: Int): Unit = {
    val opcode = makeOpcode(Opcodes.Push, typ, 0)
    emitter.emit(opcode)
    emitter.emit(value)
  }

  private def store(typ: Byte,
                    varIndex: Int,
                    value: Int,
                    allowConst: Boolean = true): Unit = {
    val s = if (varIndex < 0) 0 else varIndex
    if (s < 65536) {
      if (!allowConst || s > 255 || value < -128 || value > 127) {
        val opcode = makeOpcode(Opcodes.Store, typ, (s & 0xFF).toShort)
        emitter.emit(opcode)
        emitter.emit(value)
      } else {
        val opcode = makeOpcode(Opcodes.StoreConst, typ, (s & 0xFF).toByte, value.toByte)
        emitter.emit(opcode)
      }
    } else {
      val opcode = makeOpcode(Opcodes.StoreWide, typ, 0)
      emitter.emit(opcode)
      emitter.emit(s)
      emitter.emit(value)
    }
  }

  private def storeInternable(typ: Byte,
                              varIndex: Int,
                              constValue: Option[Byte],
                              intern: => Int): Unit = {
    val s = if (varIndex < 0) 0 else varIndex
    constValue match {
      case Some(value) if s <= 255 =>
        val opcode = makeOpcode(Opcodes.StoreConst, typ, (s & 0xFF).toByte, value)
        emitter.emit(opcode)
      case None =>
        if (s < 65536) {
          val opcode = makeOpcode(Opcodes.Store, typ, (s & 0xFF).toShort)
          emitter.emit(opcode)
          emitter.emit(intern)
        } else {
          val opcode = makeOpcode(Opcodes.StoreWide, typ, 0)
          emitter.emit(opcode)
          emitter.emit(s)
          emitter.emit(intern)
        }
    }
  }

  private def pushFromVar(typ: Byte, varIndex: Int): Unit =
    pushPop(Opcodes.PushFromVar, Opcodes.PushFromVarWide, typ, varIndex)

  private def popIntoVar(typ: Byte, varIndex: Int): Unit =
    pushPop(Opcodes.PopIntoVar, Opcodes.PopIntoVarWide, typ, varIndex)

  private def pushPop(narrow: Int, wide: Int, typ: Byte, varIndex: Int): Unit = {
    val s = if (varIndex < 0) 0 else varIndex
    if (s < 65536) {
      val opcode = makeOpcode(narrow, typ, (s & 0xFFFF).toShort)
      emitter.emit(opcode)
    } else {
      val opcode = makeOpcode(wide, typ, 0)
      emitter.emit(opcode)
      emitter.emit(s)
    }
  }

  private def maybeConst(value: Long): Option[Byte] =
    if (value >= Byte.MinValue && value <= Byte.MaxValue) Some(value.toByte)
    else None

  private def maybeConst(value: Double): Option[Byte] =
    if (value.isWhole && value >= Byte.MinValue && value <= Byte.MaxValue) Some(value.toByte)
    else None

  private def maybeConst(value: Float): Option[Byte] =
    if (value.isWhole && value >= Byte.MinValue && value <= Byte.MaxValue) Some(value.toByte)
    else None

  private def makeOpcode(instruction: Int, typ: Byte, value: Short): Int =
    (instruction << 24) | ((typ & 0xFF) << 16) | (value & 0xFFFF)

  private def makeOpcode(instruction: Int, typ: Byte, varIndex: Byte, value: Byte): Int =
    (instruction << 24) | ((typ & 0xFF) << 16) | (varIndex << 8) | value

}
