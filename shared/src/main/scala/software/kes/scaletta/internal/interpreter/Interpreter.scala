package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.{ArgumentReader, EvalResult, FunctionImpl, RuntimeContextReader}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.runtime.ParamsSignature
import software.kes.scaletta.util.stack.IntStack

import scala.collection.immutable.ArraySeq

object Interpreter {
  def create(program: Program,
             functionTable: NativeFunctionTable): Interpreter = {
    val callStack = IntStack.create()
    val operandStack = OperandStack.create()
    val variableStack = VariableStack.create()
    val varSpace = VarSpaceFromVariableStack.create(variableStack, program.mainFunction.varSpaceSignature)
    val evalResultContainer = EvalResultContainer.create(program.returnType)
    new Interpreter(program, functionTable, callStack, operandStack, variableStack,
      varSpace, evalResultContainer, 0, 0)
  }
}

final class Interpreter private(private val program: Program,
                                private val functionTable: NativeFunctionTable,
                                private val callStack: IntStack,
                                private val operandStack: OperandStack,
                                private val variableStack: VariableStack,
                                private val varSpace: VarSpaceFromVariableStack,
                                private val evalResultContainer: EvalResultContainer,
                                private var userFunctionIndex: Int,
                                private var instructionPointer: Int) {
  def run(runtimeContexts: RuntimeContextReader,
          initializer: Initializer = Initializer.none): EvalResult = {
    val argumentReader = new InterpreterArgumentReader(operandStack, ParamsSignature.empty)

    reset(initializer)
    var currentFunction = program.mainFunction
    var done = false

    var rawOpcode = 0
    while (!done) {
      val opcode = if (instructionPointer < currentFunction.instructions.length) {
        rawOpcode = currentFunction.fetch(instructionPointer)
        val op = (rawOpcode >> 24) & 0xFF
        instructionPointer += 1
        op
      } else {
        rawOpcode = Opcodes.Return << 24
        Opcodes.Return
      }

      (opcode: @annotation.switch) match {
        case Opcodes.Nop => ()

        case Opcodes.PushConst =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val value = (rawOpcode & 0xFFFF).toShort
          (typeTag: @annotation.switch) match {
            case BasicTypes.Long => operandStack.pushLong(value.toLong)
            case BasicTypes.Double => operandStack.pushDouble(value.toDouble)
            case BasicTypes.Float => operandStack.pushFloat(value.toFloat)
            case BasicTypes.Boolean => operandStack.pushBoolean(value != 0)
            case BasicTypes.Int => operandStack.pushInt(value.toInt)
            case BasicTypes.Short => operandStack.pushShort(value)
            case BasicTypes.Byte => operandStack.pushByte(value.toByte)
            case BasicTypes.Char => operandStack.pushChar(value.toChar)
            case _ => operandStack.pushObject(program.constantPool.getObject(value & 0xFFFF))
          }

        case Opcodes.Push =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val value = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          (typeTag: @annotation.switch) match {
            case BasicTypes.Long => operandStack.pushLong(program.constantPool.getLong(value))
            case BasicTypes.Double => operandStack.pushDouble(program.constantPool.getDouble(value))
            case BasicTypes.Float => operandStack.pushFloat(program.constantPool.getFloat(value))
            case BasicTypes.Boolean => operandStack.pushBoolean(value != 0)
            case BasicTypes.Int => operandStack.pushInt(value)
            case BasicTypes.Short => operandStack.pushShort(value.toShort)
            case BasicTypes.Byte => operandStack.pushByte(value.toByte)
            case BasicTypes.Char => operandStack.pushChar(value.toChar)
            case _ => operandStack.pushObject(program.constantPool.getObject(value))
          }

        case Opcodes.StoreConst =>
          // bits 16-23:  type tag
          // bits 8-15:   var index
          // bits 0-7:
          //   if type is object, long, float, or double: constant pool index
          //   otherwise, value
          // no operands
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = (rawOpcode >> 8) & 0xFF
          val value = rawOpcode & 0xFF
          storeInVar(typeTag, varIndex, value)

        case Opcodes.Store =>
          // bits 16-23:  type tag
          // bits 0-15:   var index
          // operand 1:
          //   if type is object, long, float, or double: constant pool index
          //   otherwise, value
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = rawOpcode & 0xFFFF
          val value = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          storeInVar(typeTag, varIndex, value)

        case Opcodes.StoreWide =>
          // bits 16-23:  type tag
          // bits 0-15:   ignored
          // operand 1:   var index
          // operand 2:
          //   if type is object, long, float, or double: constant pool index
          //   otherwise, value
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          val value = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          storeInVar(typeTag, varIndex, value)

        case Opcodes.Pop =>
          operandStack.pop()

        case Opcodes.Dup =>
          operandStack.duplicate()

        case Opcodes.Swap =>
          operandStack.swap()

        case Opcodes.PushFromVar =>
          val varIndex = rawOpcode & 0xFFFF
          varSpace.pushIntoOperandStack(varIndex, operandStack)

        case Opcodes.PushFromVarWide =>
          val varIndex = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          varSpace.pushIntoOperandStack(varIndex, operandStack)

        case Opcodes.PopIntoVar =>
          // bits 16-23:  type tag
          // bits 0-15:   var index
          // no operands
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = rawOpcode & 0xFFFF
          popIntoVar(typeTag, varIndex)

        case Opcodes.PopIntoVarWide =>
          // bits 16-23:    type tag
          // bits 0-15:     ignored
          // operand 1:     var index
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          popIntoVar(typeTag, varIndex)

        case Opcodes.Branch =>
          val offset = rawOpcode & 0xFFFFFF
          // need to handle sign extension if offset can be negative
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          instructionPointer += signedOffset

        case Opcodes.BranchIf =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          val cond = operandStack.popCondition()
          if (cond) {
            instructionPointer += signedOffset
          }

        case Opcodes.BranchUnless =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          val cond = operandStack.popCondition()
          if (!cond) {
            instructionPointer += signedOffset
          }

        case Opcodes.CallNative =>
          val nativeId = rawOpcode & 0xFFFFFF
          val nativeFunction = functionTable.get(software.kes.scaletta.api.NativeFunctionId(nativeId))
          argumentReader.params = nativeFunction.params
          nativeFunction.impl match {
            case FunctionImpl.ObjectResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushObject(result)
            case FunctionImpl.BooleanResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushBoolean(result)
            case FunctionImpl.IntResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushInt(result)
            case FunctionImpl.LongResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushLong(result)
            case FunctionImpl.ShortResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushShort(result)
            case FunctionImpl.ByteResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushByte(result)
            case FunctionImpl.CharResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushChar(result)
            case FunctionImpl.DoubleResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushDouble(result)
            case FunctionImpl.FloatResult(body) =>
              val result = body(argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushFloat(result)
            case FunctionImpl.ObjectResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushObject(result)
            case FunctionImpl.BooleanResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushBoolean(result)
            case FunctionImpl.IntResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushInt(result)
            case FunctionImpl.LongResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushLong(result)
            case FunctionImpl.ShortResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushShort(result)
            case FunctionImpl.ByteResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushByte(result)
            case FunctionImpl.CharResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushChar(result)
            case FunctionImpl.DoubleResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushDouble(result)
            case FunctionImpl.FloatResultWithContext(body) =>
              val result = body(runtimeContexts, argumentReader)
              operandStack.contract(nativeFunction.params)
              operandStack.pushFloat(result)
          }

        case Opcodes.CallLocal =>
          val functionIndex = rawOpcode & 0xFFFFFF
          callStack.push(userFunctionIndex)
          callStack.push(instructionPointer)
          userFunctionIndex = functionIndex
          instructionPointer = 0
          currentFunction = program.functions(userFunctionIndex)
          variableStack.expandFrame(currentFunction.frameSignature)
          varSpace.setSignature(currentFunction.varSpaceSignature)
          transferParameters(currentFunction.frameSignature, currentFunction.parameterCount)

        case Opcodes.TailCallLocal =>
          val functionIndex = rawOpcode & 0xFFFFFF
          instructionPointer = 0

          if (functionIndex != userFunctionIndex) {
            val prevFunction = currentFunction
            userFunctionIndex = functionIndex
            currentFunction = program.functions(userFunctionIndex)

            // Only manipulate the stack if we are changing functions
            variableStack.contractFrame(prevFunction.frameSignature)
            variableStack.expandFrame(currentFunction.frameSignature)
            varSpace.setSignature(currentFunction.varSpaceSignature)
          }
          transferParameters(currentFunction.frameSignature, currentFunction.parameterCount)

        case Opcodes.Return =>
          if (callStack.isEmpty) {
            evalResultContainer.loadFromOperandStack(operandStack)
            done = true
          } else {
            val prevFunction = currentFunction
            instructionPointer = callStack.pop()
            userFunctionIndex = callStack.pop()
            currentFunction = program.functions(userFunctionIndex)
            variableStack.contractFrame(prevFunction.frameSignature)
            varSpace.setSignature(currentFunction.varSpaceSignature)
          }

        case Opcodes.LogicalAnd =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          if (!operandStack.maybePopCondition(true)) {
            instructionPointer += signedOffset
          }

        case Opcodes.LogicalOr =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          if (operandStack.maybePopCondition(false)) {
            instructionPointer += signedOffset
          }

        case Opcodes.Box =>
          operandStack.box()

        case Opcodes.Convert =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          operandStack.convert(typeTag.toByte)

        case _ =>
          throw new RuntimeException(s"Unknown opcode: $opcode")
      }
    }
    evalResultContainer
  }

  private def transferParameters(frameSignature: software.kes.scaletta.internal.runtime.FrameSignature,
                                 parameterCount: Int): Unit = {
    var i = parameterCount - 1
    while (i >= 0) {
      val typeTag = frameSignature.basicTypeOf(i)
      popIntoVar(typeTag, i)
      i -= 1
    }
  }

  private def popIntoVar(typeTag: Int, varIndex: Int): Unit =
    (typeTag: @annotation.switch) match {
      case BasicTypes.Long => varSpace.unsafeWriteLong(varIndex, operandStack.unsafePopLong())
      case BasicTypes.Double => varSpace.unsafeWriteDouble(varIndex, operandStack.unsafePopDouble())
      case BasicTypes.Float => varSpace.unsafeWriteFloat(varIndex, operandStack.unsafePopFloat())
      case BasicTypes.Boolean => varSpace.unsafeWriteBoolean(varIndex, operandStack.unsafePopBoolean())
      case BasicTypes.Int => varSpace.unsafeWriteInt(varIndex, operandStack.unsafePopInt())
      case BasicTypes.Short => varSpace.unsafeWriteShort(varIndex, operandStack.unsafePopShort())
      case BasicTypes.Byte => varSpace.unsafeWriteByte(varIndex, operandStack.unsafePopByte())
      case BasicTypes.Char => varSpace.unsafeWriteChar(varIndex, operandStack.unsafePopChar())
      case _ => varSpace.unsafeWriteObject(varIndex, operandStack.unsafePopObject())
    }

  private def storeInVar(typeTag: Int, varIndex: Int, value: Int): Unit =
    (typeTag: @annotation.switch) match {
      case BasicTypes.Long => varSpace.unsafeWriteLong(varIndex, program.constantPool.getLong(value))
      case BasicTypes.Double => varSpace.unsafeWriteDouble(varIndex, program.constantPool.getDouble(value))
      case BasicTypes.Float => varSpace.unsafeWriteFloat(varIndex, program.constantPool.getFloat(value))
      case BasicTypes.Boolean => varSpace.unsafeWriteBoolean(varIndex, value != 0)
      case BasicTypes.Int => varSpace.unsafeWriteInt(varIndex, value)
      case BasicTypes.Short => varSpace.unsafeWriteShort(varIndex, value.toShort)
      case BasicTypes.Byte => varSpace.unsafeWriteByte(varIndex, value.toByte)
      case BasicTypes.Char => varSpace.unsafeWriteChar(varIndex, value.toChar)
      case _ => varSpace.unsafeWriteObject(varIndex, program.constantPool.getObject(value))
    }

  private def reset(initializer: Initializer): Unit = {
    userFunctionIndex = 0
    instructionPointer = 0
    callStack.clear()
    operandStack.clear()
    variableStack.clear()
    variableStack.expandFrame(program.mainFunction.frameSignature)
    varSpace.setSignature(program.mainFunction.varSpaceSignature)
    initializer(varSpace)
  }

  private[interpreter] def readAllVariables(): Array[Any] =
    varSpace.readAll()
}

/**
 * Mutable. The same instance will be reused for every native call.
 */
private[interpreter] final class InterpreterArgumentReader(operandStack: OperandStack,
                                                           var params: ParamsSignature) extends ArgumentReader {
  def argCount: Int = params.paramCount

  def read(index: Int): Any = {
    val basicType = params.basicTypeOf(index)
    val stackOffset = params.stackOffsetOf(index)
    basicType match {
      case BasicTypes.Boolean => unsafeReadBoolean(stackOffset)
      case BasicTypes.Int => unsafeReadInt(stackOffset)
      case BasicTypes.Long => unsafeReadLong(stackOffset)
      case BasicTypes.Short => unsafeReadShort(stackOffset)
      case BasicTypes.Byte => unsafeReadByte(stackOffset)
      case BasicTypes.Char => unsafeReadChar(stackOffset)
      case BasicTypes.Double => unsafeReadDouble(stackOffset)
      case BasicTypes.Float => unsafeReadFloat(stackOffset)
      case _ => unsafeReadObject(stackOffset)
    }
  }

  def toVector: Vector[Any] =
    (0 until argCount).map(read).toVector

  def toArray: Array[Any] =
    (0 until argCount).map(read).toArray

  def unsafeReadBoolean(index: Int): Boolean =
    operandStack.booleans.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadByte(index: Int): Byte =
    operandStack.bytes.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadChar(index: Int): Char =
    operandStack.chars.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadDouble(index: Int): Double =
    operandStack.doubles.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadFloat(index: Int): Float =
    operandStack.floats.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadInt(index: Int): Int =
    operandStack.ints.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadLong(index: Int): Long =
    operandStack.longs.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadShort(index: Int): Short =
    operandStack.shorts.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadObject(index: Int): AnyRef =
    operandStack.objects.unsafeRead(params.stackOffsetOf(index))

  def unsafeReadBooleanArray(index: Int): ArraySeq[Boolean] =
    read(index).asInstanceOf[ArraySeq[Boolean]]

  def unsafeReadByteArray(index: Int): ArraySeq[Byte] =
    read(index).asInstanceOf[ArraySeq[Byte]]

  def unsafeReadCharArray(index: Int): ArraySeq[Char] =
    read(index).asInstanceOf[ArraySeq[Char]]

  def unsafeReadDoubleArray(index: Int): ArraySeq[Double] =
    read(index).asInstanceOf[ArraySeq[Double]]

  def unsafeReadFloatArray(index: Int): ArraySeq[Float] =
    read(index).asInstanceOf[ArraySeq[Float]]

  def unsafeReadIntArray(index: Int): ArraySeq[Int] =
    read(index).asInstanceOf[ArraySeq[Int]]

  def unsafeReadLongArray(index: Int): ArraySeq[Long] =
    read(index).asInstanceOf[ArraySeq[Long]]

  def unsafeReadShortArray(index: Int): ArraySeq[Short] =
    read(index).asInstanceOf[ArraySeq[Short]]
}
