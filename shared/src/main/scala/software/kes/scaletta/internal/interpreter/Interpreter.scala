package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.{ArgumentReader, EvalResult, RuntimeContextReader}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
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
  def run(runtimeContexts: RuntimeContextReader): EvalResult = {
    reset()
    variableStack.expandFrame(program.mainFunction.frameSignature)
    var currentFunction = program.mainFunction
    var done = false

    while (!done) {
      val rawOpcode = currentFunction.fetch(instructionPointer)
      val opcode = (rawOpcode >> 24) & 0xFF
      instructionPointer += 1

      (opcode: @annotation.switch) match {
        case Opcodes.Nop => ()

        case Opcodes.PushConst =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val poolIndex = rawOpcode & 0xFFFF
          (typeTag: @annotation.switch) match {
            case BasicTypes.Long => operandStack.pushLong(program.constantPool.getLong(poolIndex))
            case BasicTypes.Double => operandStack.pushDouble(program.constantPool.getDouble(poolIndex))
            case BasicTypes.Float => operandStack.pushFloat(program.constantPool.getFloat(poolIndex))
            case BasicTypes.Boolean => operandStack.pushBoolean(poolIndex != 0)
            case BasicTypes.Int => operandStack.pushInt(poolIndex)
            case BasicTypes.Short => operandStack.pushShort(poolIndex.toShort)
            case BasicTypes.Byte => operandStack.pushByte(poolIndex.toByte)
            case BasicTypes.Char => operandStack.pushChar(poolIndex.toChar)
            case _ => operandStack.pushObject(program.constantPool.getObject(poolIndex).asInstanceOf[AnyRef])
          }

        case Opcodes.Push =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val value = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          (typeTag: @annotation.switch) match {
            case BasicTypes.Boolean => operandStack.pushBoolean(value != 0)
            case BasicTypes.Int => operandStack.pushInt(value)
            case BasicTypes.Short => operandStack.pushShort(value.toShort)
            case BasicTypes.Byte => operandStack.pushByte(value.toByte)
            case BasicTypes.Char => operandStack.pushChar(value.toChar)
          }

        case Opcodes.Pop =>
          operandStack.pop()

        case Opcodes.PopWide =>
          operandStack.pop()

        case Opcodes.Dup =>
          val value = operandStack.pop()
          operandStack.push(value)
          operandStack.push(value)

        case Opcodes.Swap =>
          val b = operandStack.pop()
          val a = operandStack.pop()
          operandStack.push(b)
          operandStack.push(a)

        case Opcodes.PushFromVar =>
          val varIndex = rawOpcode & 0xFFFF
          val value = varSpace.read(varIndex)
          operandStack.push(value)
          println(s"[DEBUG_LOG] PushFromVar index=$varIndex value=$value stackSize=${operandStack.size()}")

        case Opcodes.PushFromVarWide =>
          val varIndex = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          val value = varSpace.read(varIndex)
          operandStack.push(value)

        case Opcodes.PopIntoVar =>
          val varIndex = rawOpcode & 0xFFFF
          val value = operandStack.pop()
          println(s"[DEBUG_LOG] PopIntoVar index=$varIndex value=$value stackSize=${operandStack.size()}")
          writeToVar(varIndex, value)

        case Opcodes.PopIntoVarWide =>
          val varIndex = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          val value = operandStack.pop()
          writeToVar(varIndex, value)

        case Opcodes.Branch =>
          val offset = rawOpcode & 0xFFFFFF
          // need to handle sign extension if offset can be negative
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          instructionPointer += signedOffset

        case Opcodes.BranchIf =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          if (operandStack.popCondition()) {
            instructionPointer += signedOffset
          }

        case Opcodes.BranchUnless =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          if (!operandStack.popCondition()) {
            instructionPointer += signedOffset
          }

        case Opcodes.CallNative =>
          val nativeId = rawOpcode & 0xFFFFFF
          val nativeFunction = functionTable.get(software.kes.scaletta.api.NativeFunctionId(nativeId))
          val args = new InterpreterArgumentReader(operandStack, nativeFunction.params)
          val result = executeNative(nativeFunction, runtimeContexts, args)
          operandStack.push(result)

        case Opcodes.CallLocal =>
          val functionIndex = rawOpcode & 0xFFFFFF
          callStack.push(userFunctionIndex)
          callStack.push(instructionPointer)
          userFunctionIndex = functionIndex
          instructionPointer = 0
          currentFunction = program.functions(userFunctionIndex)
          variableStack.expandFrame(currentFunction.frameSignature)
          varSpace.setSignature(currentFunction.varSpaceSignature)

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

        case _ =>
          throw new RuntimeException(s"Unknown opcode: $opcode")
      }
    }
    evalResultContainer
  }

  private def writeToVar(index: Int, value: Any): Unit = {
    value match {
      case x: Boolean => varSpace.unsafeWriteBoolean(index, x)
      case x: Int => varSpace.unsafeWriteInt(index, x)
      case x: Long => varSpace.unsafeWriteLong(index, x)
      case x: Short => varSpace.unsafeWriteShort(index, x)
      case x: Byte => varSpace.unsafeWriteByte(index, x)
      case x: Char => varSpace.unsafeWriteChar(index, x)
      case x: Double => varSpace.unsafeWriteDouble(index, x)
      case x: Float => varSpace.unsafeWriteFloat(index, x)
      case x: AnyRef => varSpace.unsafeWriteObject(index, x)
    }
  }

  private def executeNative(nativeFunction: software.kes.scaletta.internal.builtins.NativeFunction,
                            runtimeContexts: RuntimeContextReader,
                            args: ArgumentReader): Any = {
    import software.kes.scaletta.api.FunctionImpl._
    nativeFunction.impl match {
      case ObjectResult(body) => body(args)
      case BooleanResult(body) => body(args)
      case IntResult(body) => body(args)
      case LongResult(body) => body(args)
      case ShortResult(body) => body(args)
      case ByteResult(body) => body(args)
      case CharResult(body) => body(args)
      case DoubleResult(body) => body(args)
      case FloatResult(body) => body(args)
      case ObjectResultWithContext(body) => body(runtimeContexts, args)
      case BooleanResultWithContext(body) => body(runtimeContexts, args)
      case IntResultWithContext(body) => body(runtimeContexts, args)
      case LongResultWithContext(body) => body(runtimeContexts, args)
      case ShortResultWithContext(body) => body(runtimeContexts, args)
      case ByteResultWithContext(body) => body(runtimeContexts, args)
      case CharResultWithContext(body) => body(runtimeContexts, args)
      case DoubleResultWithContext(body) => body(runtimeContexts, args)
      case FloatResultWithContext(body) => body(runtimeContexts, args)
    }
  }

  private def reset(): Unit = {
    userFunctionIndex = 0
    instructionPointer = 0
    callStack.clear()
    operandStack.clear()
    variableStack.clear()
    varSpace.setSignature(program.mainFunction.varSpaceSignature)
  }
}

private[interpreter] final class InterpreterArgumentReader(operandStack: OperandStack,
                                                           params: software.kes.scaletta.internal.runtime.ParamsSignature) extends ArgumentReader {
  override def argCount: Int = params.paramCount

  override def read(index: Int): Any = {
    val basicType = params.basicTypeOf(index)
    val stackOffset = params.stackOffsetOf(index)
    basicType match {
      case BasicTypes.Boolean => unsafeReadBoolean(index)
      case BasicTypes.Int => unsafeReadInt(index)
      case BasicTypes.Long => unsafeReadLong(index)
      case BasicTypes.Short => unsafeReadShort(index)
      case BasicTypes.Byte => unsafeReadByte(index)
      case BasicTypes.Char => unsafeReadChar(index)
      case BasicTypes.Double => unsafeReadDouble(index)
      case BasicTypes.Float => unsafeReadFloat(index)
      case _ => unsafeReadObject(index)
    }
  }

  override def toVector: Vector[Any] =
    (0 until argCount).map(read).toVector

  override def toArray: Array[Any] =
    (0 until argCount).map(read).toArray

  override def unsafeReadBoolean(index: Int): Boolean =
    operandStack.booleans.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadByte(index: Int): Byte =
    operandStack.bytes.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadChar(index: Int): Char =
    operandStack.chars.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadDouble(index: Int): Double =
    operandStack.doubles.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadFloat(index: Int): Float =
    operandStack.floats.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadInt(index: Int): Int =
    operandStack.ints.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadLong(index: Int): Long =
    operandStack.longs.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadShort(index: Int): Short =
    operandStack.shorts.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadObject(index: Int): AnyRef =
    operandStack.objects.unsafeRead(params.stackOffsetOf(index))

  override def unsafeReadBooleanArray(index: Int): ArraySeq[Boolean] =
    read(index).asInstanceOf[ArraySeq[Boolean]]

  override def unsafeReadByteArray(index: Int): ArraySeq[Byte] =
    read(index).asInstanceOf[ArraySeq[Byte]]

  override def unsafeReadCharArray(index: Int): ArraySeq[Char] =
    read(index).asInstanceOf[ArraySeq[Char]]

  override def unsafeReadDoubleArray(index: Int): ArraySeq[Double] =
    read(index).asInstanceOf[ArraySeq[Double]]

  override def unsafeReadFloatArray(index: Int): ArraySeq[Float] =
    read(index).asInstanceOf[ArraySeq[Float]]

  override def unsafeReadIntArray(index: Int): ArraySeq[Int] =
    read(index).asInstanceOf[ArraySeq[Int]]

  override def unsafeReadLongArray(index: Int): ArraySeq[Long] =
    read(index).asInstanceOf[ArraySeq[Long]]

  override def unsafeReadShortArray(index: Int): ArraySeq[Short] =
    read(index).asInstanceOf[ArraySeq[Short]]
}
