package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.{ArgumentReader, EvalResult, FunctionImpl, RuntimeContextReader}
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
          val value = rawOpcode & 0xFFFF
          println(s"[DEBUG_LOG] PushConst typeTag=$typeTag poolIndex=$value")
          (typeTag: @annotation.switch) match {
            case BasicTypes.Long => operandStack.pushLong(program.constantPool.getLong(value))
            case BasicTypes.Double => operandStack.pushDouble(program.constantPool.getDouble(value))
            case BasicTypes.Float => operandStack.pushFloat(program.constantPool.getFloat(value))
            case BasicTypes.Boolean => operandStack.pushBoolean(value != 0)
            case BasicTypes.Int => operandStack.pushInt(value)
            case BasicTypes.Short => operandStack.pushShort(value.toShort)
            case BasicTypes.Byte => operandStack.pushByte(value.toByte)
            case BasicTypes.Char => operandStack.pushChar(value.toChar)
            case _ => operandStack.pushObject(program.constantPool.getObject(value).asInstanceOf[AnyRef])
          }

        case Opcodes.Push =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val value = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          println(s"[DEBUG_LOG] Push typeTag=$typeTag value=$value")
          (typeTag: @annotation.switch) match {
            case BasicTypes.Boolean => operandStack.pushBoolean(value != 0)
            case BasicTypes.Int => operandStack.pushInt(value)
            case BasicTypes.Short => operandStack.pushShort(value.toShort)
            case BasicTypes.Byte => operandStack.pushByte(value.toByte)
            case BasicTypes.Char => operandStack.pushChar(value.toChar)
          }

        case Opcodes.Pop =>
          println("[DEBUG_LOG] Pop")
          operandStack.pop()

        case Opcodes.PopWide =>
          println("[DEBUG_LOG] PopWide")
          operandStack.pop()

        case Opcodes.Dup =>
          val value = operandStack.pop()
          println(s"[DEBUG_LOG] Dup $value")
          operandStack.push(value)
          operandStack.push(value)

        case Opcodes.Swap =>
          val b = operandStack.pop()
          val a = operandStack.pop()
          println(s"[DEBUG_LOG] Swap $a, $b")
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
          println(s"[DEBUG_LOG] PushFromVarWide index=$varIndex value=$value stackSize=${operandStack.size()}")

        case Opcodes.PopIntoVar =>
          val varIndex = rawOpcode & 0xFFFF
          val value = operandStack.pop()
          println(s"[DEBUG_LOG] PopIntoVar index=$varIndex value=$value stackSize=${operandStack.size()}")
          writeToVar(varIndex, value)

        case Opcodes.PopIntoVarWide =>
          val varIndex = currentFunction.fetch(instructionPointer)
          instructionPointer += 1
          val value = operandStack.pop()
          println(s"[DEBUG_LOG] PopIntoVarWide index=$varIndex value=$value stackSize=${operandStack.size()}")
          writeToVar(varIndex, value)

        case Opcodes.Branch =>
          val offset = rawOpcode & 0xFFFFFF
          // need to handle sign extension if offset can be negative
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          println(s"[DEBUG_LOG] Branch offset=$signedOffset")
          instructionPointer += signedOffset

        case Opcodes.BranchIf =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          val cond = operandStack.popCondition()
          println(s"[DEBUG_LOG] BranchIf cond=$cond offset=$signedOffset")
          if (cond) {
            instructionPointer += signedOffset
          }

        case Opcodes.BranchUnless =>
          val offset = rawOpcode & 0xFFFFFF
          val signedOffset = if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
          val cond = operandStack.popCondition()
          println(s"[DEBUG_LOG] BranchUnless cond=$cond offset=$signedOffset")
          if (!cond) {
            instructionPointer += signedOffset
          }

        case Opcodes.CallNative =>
          val nativeId = rawOpcode & 0xFFFFFF
          val nativeFunction = functionTable.get(software.kes.scaletta.api.NativeFunctionId(nativeId))
          println(s"[DEBUG_LOG] CallNative $nativeId paramCount=${nativeFunction.params.paramCount}")
          val args = new InterpreterArgumentReader(operandStack, nativeFunction.params)
          nativeFunction.impl match {
            case FunctionImpl.ObjectResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushObject(result)
            case FunctionImpl.BooleanResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushBoolean(result)
            case FunctionImpl.IntResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushInt(result)
            case FunctionImpl.LongResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushLong(result)
            case FunctionImpl.ShortResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushShort(result)
            case FunctionImpl.ByteResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushByte(result)
            case FunctionImpl.CharResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushChar(result)
            case FunctionImpl.DoubleResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushDouble(result)
            case FunctionImpl.FloatResult(body) =>
              val result = body(args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushFloat(result)
            case FunctionImpl.ObjectResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushObject(result)
            case FunctionImpl.BooleanResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushBoolean(result)
            case FunctionImpl.IntResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushInt(result)
            case FunctionImpl.LongResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushLong(result)
            case FunctionImpl.ShortResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushShort(result)
            case FunctionImpl.ByteResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushByte(result)
            case FunctionImpl.CharResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushChar(result)
            case FunctionImpl.DoubleResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushDouble(result)
            case FunctionImpl.FloatResultWithContext(body) =>
              val result = body(runtimeContexts, args)
              operandStack.contract(nativeFunction.params)
              operandStack.pushFloat(result)
          }

        case Opcodes.CallLocal =>
          val functionIndex = rawOpcode & 0xFFFFFF
          println(s"[DEBUG_LOG] CallLocal $functionIndex")
          callStack.push(userFunctionIndex)
          callStack.push(instructionPointer)
          userFunctionIndex = functionIndex
          instructionPointer = 0
          currentFunction = program.functions(userFunctionIndex)
          variableStack.expandFrame(currentFunction.frameSignature)
          varSpace.setSignature(currentFunction.varSpaceSignature)

        case Opcodes.Return =>
          if (callStack.isEmpty) {
            println(s"[DEBUG_LOG] Return from main. stackSize=${operandStack.size()}")
            evalResultContainer.loadFromOperandStack(operandStack)
            done = true
          } else {
            println(s"[DEBUG_LOG] Return from local. stackSize=${operandStack.size()}")
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
