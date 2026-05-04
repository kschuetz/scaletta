package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionId
import software.kes.scaletta.util.stack.IntStack

object Assembler {
  trait Label {

    /**
     * Binds this label to the current address in the Assembler.
     * Any previous forward branches to this label will be patched.
     * Any subsequent branches to this label will use this address immediately.
     */
    def bind(): Unit
  }

}

final class Assembler(private val writer: OpcodeWriter,
                      private val interner: ConstantInterner) {

  def nop(): Unit =
    writer.writeAndAdvance(Opcodes.Nop)

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
      pushConst(BasicTypes.Int, value.toShort)
    } else push(BasicTypes.Int, value)

  def pushImmediateLong(value: Long): Unit =
    if (value >= Short.MinValue && value <= Short.MaxValue) {
      pushConst(BasicTypes.Long, value.toShort)
    } else push(BasicTypes.Long, interner.internLong(value))

  def pushImmediateBoolean(value: Boolean): Unit =
    pushConst(BasicTypes.Boolean, if (value) 1 else 0)

  def pushImmediateByte(value: Byte): Unit =
    pushConst(BasicTypes.Byte, value)

  def pushImmediateShort(value: Short): Unit =
    pushConst(BasicTypes.Short, value)

  def pushImmediateDouble(value: Double): Unit =
    if (value.isWhole) {
      if (value >= Short.MinValue && value <= Short.MaxValue) {
        pushConst(BasicTypes.Double, value.toShort)
      } else {
        push(BasicTypes.Double, interner.internDouble(value))
      }
    } else push(BasicTypes.Double, interner.internDouble(value))

  def pushImmediateFloat(value: Float): Unit =
    if (value.isWhole) {
      if (value >= Short.MinValue && value <= Short.MaxValue) {
        pushConst(BasicTypes.Float, value.toShort)
      } else {
        push(BasicTypes.Float, interner.internFloat(value))
      }
    } else push(BasicTypes.Float, interner.internFloat(value))

  def pushImmediateChar(value: Char): Unit =
    if (value >= Short.MinValue && value <= Short.MaxValue) pushConst(BasicTypes.Char, value.toShort)
    else push(BasicTypes.Char, value)

  def pushImmediateObject(value: AnyRef): Unit =
    if (value == null) pushNull()
    else push(BasicTypes.Object, interner.internObject(value))

  def pushNull(): Unit =
    pushConst(BasicTypes.Object, 0)

  def pushIntFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Int, varIndex)

  def pushLongFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Long, varIndex)

  def pushDoubleFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Double, varIndex)

  def pushFloatFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Float, varIndex)

  def pushByteFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Byte, varIndex)

  def pushShortFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Short, varIndex)

  def pushCharFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Char, varIndex)

  def pushBooleanFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Boolean, varIndex)

  def pushObjectFromVar(varIndex: Int): Unit =
    pushFromVar(BasicTypes.Object, varIndex)

  def popIntIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Int, varIndex)

  def popLongIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Long, varIndex)

  def popDoubleIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Double, varIndex)

  def popFloatIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Float, varIndex)

  def popByteIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Byte, varIndex)

  def popShortIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Short, varIndex)

  def popCharIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Char, varIndex)

  def popBooleanIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Boolean, varIndex)

  def popObjectIntoVar(varIndex: Int): Unit =
    popIntoVar(BasicTypes.Object, varIndex)

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
    else store(BasicTypes.Object, varIndex, interner.internObject(value), allowConst = false)

  def storeImmediateBoolean(varIndex: Int, value: Boolean): Unit =
    store(BasicTypes.Boolean, varIndex, if (value) 1 else 0)

  def storeImmediateInt(varIndex: Int, value: Int): Unit =
    store(BasicTypes.Int, varIndex, value)

  def storeImmediateLong(varIndex: Int, value: Long): Unit =
    storeInternable(BasicTypes.Int, varIndex, maybeConst(value), interner.internLong(value))

  def storeImmediateShort(varIndex: Int, value: Short): Unit =
    store(BasicTypes.Short, varIndex, value)

  def storeImmediateByte(varIndex: Int, value: Byte): Unit =
    store(BasicTypes.Byte, varIndex, value)

  def storeImmediateChar(varIndex: Int, value: Char): Unit =
    store(BasicTypes.Char, varIndex, value)

  def storeImmediateDouble(varIndex: Int, value: Double): Unit =
    storeInternable(BasicTypes.Int, varIndex, maybeConst(value), interner.internDouble(value))

  def storeImmediateFloat(varIndex: Int, value: Float): Unit =
    storeInternable(BasicTypes.Int, varIndex, maybeConst(value), interner.internFloat(value))

  def storeNull(varIndex: Int): Unit =
    store(BasicTypes.Object, varIndex, 0)

  /**
   * Unconditionally branches to the specified label.
   */
  def branch(label: Assembler.Label): Unit =
    emitBranch(Opcodes.Branch, label)

  /**
   * Value on the top of the operand stack is popped. If it was truthy, the branch is taken.
   */
  def branchIf(label: Assembler.Label): Unit =
    emitBranch(Opcodes.BranchIf, label)

  /**
   * Value on the top of the operand stack is popped. If it was not truthy, the branch is taken.
   */
  def branchUnless(label: Assembler.Label): Unit =
    emitBranch(Opcodes.BranchUnless, label)

  /**
   * Peeks the value at the top of the operand stack. If truthy, the branch is taken, and the
   * stack is unchanged. If not truthy, the value on the stack is popped.
   */
  def logicalAnd(label: Assembler.Label): Unit =
    emitBranch(Opcodes.LogicalAnd, label)

  /**
   * Peeks the value at the top of the operand stack. If not truthy, the branch is taken, and the
   * stack is unchanged. If truthy, the value on the stack is popped.
   */
  def logicalOr(label: Assembler.Label): Unit =
    emitBranch(Opcodes.LogicalOr, label)

  def dup(): Unit =
    writer.writeAndAdvance(makeOpcode(Opcodes.Dup, 0, 0))

  def swap(): Unit =
    writer.writeAndAdvance(makeOpcode(Opcodes.Swap, 0, 0))

  def callNative(nativeFunctionId: NativeFunctionId): Unit =
    writer.writeAndAdvance(makeOpcode24(Opcodes.CallNative, nativeFunctionId.value))

  def callLocal(userFunctionIndex: Int): Unit =
    writer.writeAndAdvance(makeOpcode24(Opcodes.CallLocal, userFunctionIndex))

  def label(): Assembler.Label = new LabelImpl()

  /**
   * Creates a new label and immediately binds it to the current address.
   * This is primarily used for backward branches (loops).
   *
   * @return A label bound to the current address.
   */
  def mark(): Assembler.Label = {
    val l = label()
    l.bind()
    l
  }

  /**
   * Performs an unconditional branch to the specified label.
   * This is a semantic alias for `branch(label)`, intended for loops.
   *
   * @param label The target label to jump back to.
   */
  def loop(label: Assembler.Label): Unit = {
    branch(label)
  }

  def ifTrue(body: => Unit): Unit = {
    val exitLabel = label()
    branchUnless(exitLabel)
    body
    exitLabel.bind()
  }

  def ifElse(onTrue: => Unit,
             onFalse: => Unit): Unit = {
    val elseLabel = label()
    val exitLabel = label()
    branchUnless(elseLabel)
    onTrue
    branch(exitLabel)
    elseLabel.bind()
    onFalse
    exitLabel.bind()
  }

  private def emitBranch(baseInstruction: Int, label: Assembler.Label): Unit = {
    val impl = label.asInstanceOf[LabelImpl]
    if (impl.isBound) {
      val offset = impl.address - writer.currentAddress - 1
      writer.writeAndAdvance(encodeBranch(baseInstruction, offset))
    } else {
      val site = writer.currentAddress
      impl.addPatchSite(site)
      writer.writeAndAdvance(encodeBranch(baseInstruction, 0))
    }
  }

  private def pushConst(typ: Byte, value: Short): Unit = {
    val opcode = makeOpcode(Opcodes.PushConst, typ, value)
    writer.writeAndAdvance(opcode)
  }

  private def push(typ: Byte, value: Int): Unit = {
    val opcode = makeOpcode(Opcodes.Push, typ, 0)
    writer.writeAndAdvance(opcode)
    writer.writeAndAdvance(value)
  }

  private def store(typ: Byte,
                    varIndex: Int,
                    value: Int,
                    allowConst: Boolean = true): Unit = {
    val s = if (varIndex < 0) 0 else varIndex
    if (s < 65536) {
      if (!allowConst || s > 255 || value < -128 || value > 127) {
        val opcode = makeOpcode(Opcodes.Store, typ, (s & 0xFF).toShort)
        writer.writeAndAdvance(opcode)
        writer.writeAndAdvance(value)
      } else {
        val opcode = makeOpcode(Opcodes.StoreConst, typ, (s & 0xFF).toByte, value.toByte)
        writer.writeAndAdvance(opcode)
      }
    } else {
      val opcode = makeOpcode(Opcodes.StoreWide, typ, 0)
      writer.writeAndAdvance(opcode)
      writer.writeAndAdvance(s)
      writer.writeAndAdvance(value)
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
        writer.writeAndAdvance(opcode)
      case None =>
        if (s < 65536) {
          val opcode = makeOpcode(Opcodes.Store, typ, (s & 0xFF).toShort)
          writer.writeAndAdvance(opcode)
          writer.writeAndAdvance(intern)
        } else {
          val opcode = makeOpcode(Opcodes.StoreWide, typ, 0)
          writer.writeAndAdvance(opcode)
          writer.writeAndAdvance(s)
          writer.writeAndAdvance(intern)
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
      writer.writeAndAdvance(opcode)
    } else {
      val opcode = makeOpcode(wide, typ, 0)
      writer.writeAndAdvance(opcode)
      writer.writeAndAdvance(s)
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

  private def makeOpcode24(baseInstruction: Int, operand: Int): Int =
    (baseInstruction << 24) | (operand & 0xFFFFFF)

  private def encodeBranch(baseInstruction: Int, offset: Int): Int =
    makeOpcode24(baseInstruction, offset)

  private class LabelImpl extends Assembler.Label {
    private var boundAddress: Option[Int] = None
    private val patchSites = IntStack.create()

    def isBound: Boolean = boundAddress.isDefined

    def address: Int = boundAddress.getOrElse(0)

    def bind(): Unit = {
      if (boundAddress.isDefined) {
        throw new IllegalStateException("Label is already bound")
      }
      val target = writer.currentAddress
      boundAddress = Some(target)
      while (!patchSites.isEmpty) {
        val site = patchSites.pop()
        val offset = target - site - 1
        writer.write(site, offset, 0xFFFFFF)
      }
    }

    def addPatchSite(address: Int): Unit = {
      patchSites.push(address)
    }
  }

}
