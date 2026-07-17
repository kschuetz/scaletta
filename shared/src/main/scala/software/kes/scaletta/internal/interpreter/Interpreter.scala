package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.{EvalResult, FunctionImpl, NativeStep, RuntimeContextReader}
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.runtime.{ParamsSignature, VarAddress, VarSpaceSignature}
import software.kes.scaletta.util.stack.{IntStack, ObjectStack}

object Interpreter {
  def create(program: Program,
             functionTable: NativeFunctionTable): Interpreter = {
    val callStack = IntStack.create()
    val operandStack = OperandStack.create()
    val variableStack = VariableStack.create()
    val varSpace = VarSpaceFromVariableStack.create(variableStack, program.mainFunction.varSpaceSignature)
    val pool = new CapturedFramePool(maxRetained = 64)
    val nativeContStack = ObjectStack.create()
    new Interpreter(program, functionTable, callStack, operandStack, variableStack,
      varSpace, 0, 0, pool, nativeContStack)
  }
}

final class Interpreter private(private val program: Program,
                                private val functionTable: NativeFunctionTable,
                                private val callStack: IntStack,
                                private val operandStack: OperandStack,
                                private val variableStack: VariableStack,
                                private val varSpace: VarSpaceFromVariableStack,
                                private var userFunctionIndex: Int,
                                private var instructionPointer: Int,
                                private val capturedFramePool: CapturedFramePool,
                                private val nativeContStack: ObjectStack) {
  private var runtimeContexts: RuntimeContextReader = _
  private var evalResultContainer: EvalResultContainer = _
  private var currentFunction: UserFunction = _
  private var done: Boolean = true
  private val argumentReader = new OperandStackArgumentReader(operandStack, ParamsSignature.empty)

  /**
   * Initializes the interpreter and runs the program to completion.
   */
  def run(runtimeContexts: RuntimeContextReader,
          initializer: Initializer = Initializer.none,
          initialUserFunctionIndex: Int = 0): EvalResult = {
    try {
      initialize(runtimeContexts, initializer, initialUserFunctionIndex)
      runUntilDone()
      getResult
    } finally {
      capturedFramePool.endRun()
    }
  }

  /**
   * Initializes the interpreter for execution.
   * This is only needed if you will be calling [[step]] or [[runUntilDone]] manually.
   * Calling [[run]] will automatically initialize the interpreter.
   */
  def initialize(runtimeContexts: RuntimeContextReader,
                 initializer: Initializer = Initializer.none,
                 initialUserFunctionIndex: Int = 0): Unit = {
    capturedFramePool.endRun()
    val targetFunction = program.functions(initialUserFunctionIndex)
    this.evalResultContainer = EvalResultContainer.create(targetFunction.returnType)
    this.runtimeContexts = runtimeContexts

    reset(initializer, targetFunction)
    this.userFunctionIndex = initialUserFunctionIndex
    this.currentFunction = targetFunction
    this.done = false
  }

  /**
   * Runs the program until completion.
   */
  def runUntilDone(): Unit = {
    while (!done) {
      executeOne()
    }
  }

  /**
   * Executes up to `maxSteps` instructions.
   *
   * @param maxSteps The maximum number of instructions to execute. Must be greater than 0 for any work to be done.
   *                 Defaults to 1.
   * @return true if the program is still running; false if it has completed.
   */
  def step(maxSteps: Int = 1): Boolean = {
    if (done) return false

    var count = 0
    while (count < maxSteps && !done) {
      executeOne()
      count += 1
    }
    !done
  }

  private def executeOne(): Unit = {
    var rawOpcode = 0
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
        argumentReader.signature = nativeFunction.params
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
          case FunctionImpl.HigherOrder(body) =>
            throw new UnsupportedOperationException("Higher order functions are not supported yet")
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
          case FunctionImpl.HigherOrderWithContext(body) =>
            throw new UnsupportedOperationException("Higher order functions are not supported yet")
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
          capturedFramePool.endRun()
        } else {
          val prevFunction = currentFunction
          var nextIP = callStack.pop()
          var nextFuncIdx = callStack.pop()

          while (nextFuncIdx < 0) {
            if (nextFuncIdx == -1) {
              operandStack.swap()
              val cell = operandStack.pop().asInstanceOf[LazyCell]
              cell.update(operandStack)
            } else if (nextFuncIdx == -2) {
              val result = operandStack.pop()
              val (step, tag) = resumeNativeCont(result)
              handleNativeStep(step, tag)
            }

            nextIP = callStack.pop()
            nextFuncIdx = callStack.pop()
          }

          instructionPointer = nextIP
          userFunctionIndex = nextFuncIdx
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

      case Opcodes.StringConcat =>
        val numArgs = rawOpcode & 0xFFFFFF
        val result = if (numArgs > 0) {
          val items = new Array[Any](numArgs)
          var i = numArgs - 1
          while (i >= 0) {
            items(i) = operandStack.pop()
            i -= 1
          }
          val sb = new StringBuilder(numArgs * 16)
          var j = 0
          while (j < numArgs) {
            sb.append(items(j))
            j += 1
          }
          sb.result()
        } else ""
        operandStack.pushObject(result)

      case Opcodes.LazyInit =>
        val typ = (rawOpcode >> 16) & 0xFF
        val varIndex = currentFunction.fetch(instructionPointer)
        instructionPointer += 1
        val cell = LazyCell.create(typ.toByte)
        varSpace.unsafeWriteObject(varIndex, cell)

      case Opcodes.LazyEval =>
        val varIndex = currentFunction.fetch(instructionPointer)
        instructionPointer += 1
        val evalFunctionIndex = currentFunction.fetch(instructionPointer)
        instructionPointer += 1

        val cell = varSpace.unsafeReadObject(varIndex).asInstanceOf[LazyCell]
        if (cell.evaluated) {
          cell.pushValue(operandStack)
        } else if (cell.evaluating) {
          throw new RuntimeException("Circular dependency detected during lazy evaluation")
        } else {
          cell.markEvaluating()
          callStack.push(userFunctionIndex)
          callStack.push(instructionPointer)
          callStack.push(-1)
          callStack.push(0)

          operandStack.pushObject(cell)

          userFunctionIndex = evalFunctionIndex
          instructionPointer = 0
          currentFunction = program.functions(userFunctionIndex)
          variableStack.expandFrame(currentFunction.frameSignature)
          varSpace.setSignature(currentFunction.varSpaceSignature)
        }

      case Opcodes.MakeClosure =>
        val functionIndex = rawOpcode & 0xFFFFFF
        val capturePlanIndex = currentFunction.fetch(instructionPointer)
        instructionPointer += 1
        val capturedFrame = if (capturePlanIndex == 0) CapturedFrame.empty else {
          val capturePlan = program.constantPool.getObject(capturePlanIndex).asInstanceOf[CapturePlan]
          val cf = capturedFramePool.acquire(capturePlan.signature)
          capturePlan.capture(varSpace, cf)
          cf
        }
        operandStack.pushObject(new RuntimeClosure(functionIndex, capturedFrame))

      case Opcodes.CallClosure =>
        val closure = operandStack.unsafePopObject().asInstanceOf[RuntimeClosure]
        callStack.push(userFunctionIndex)
        callStack.push(instructionPointer)
        userFunctionIndex = closure.functionIndex
        instructionPointer = 0
        currentFunction = program.functions(userFunctionIndex)
        variableStack.expandFrame(currentFunction.frameSignature)
        varSpace.setSignature(currentFunction.varSpaceSignature)
        transferParameters(currentFunction.frameSignature, currentFunction.parameterCount)
        transferCaptures(closure.capturedFrame, currentFunction.varSpaceSignature, currentFunction.parameterCount)

      case Opcodes.TailCallClosure =>
        val closure = operandStack.unsafePopObject().asInstanceOf[RuntimeClosure]
        instructionPointer = 0

        if (closure.functionIndex != userFunctionIndex) {
          val prevFunction = currentFunction
          userFunctionIndex = closure.functionIndex
          currentFunction = program.functions(userFunctionIndex)
          variableStack.contractFrame(prevFunction.frameSignature)
          variableStack.expandFrame(currentFunction.frameSignature)
          varSpace.setSignature(currentFunction.varSpaceSignature)
        }
        transferParameters(currentFunction.frameSignature, currentFunction.parameterCount)
        transferCaptures(closure.capturedFrame, currentFunction.varSpaceSignature, currentFunction.parameterCount)

      case _ =>
        throw new RuntimeException(s"Unknown opcode: $opcode")
    }
  }

  private[interpreter] def pushNativeCont(k: Any => NativeStep, resultTypeTag: Byte): Unit = {
    nativeContStack.push(NativeContFrame.HigherOrderCont(k, resultTypeTag))
    callStack.push(userFunctionIndex)
    callStack.push(instructionPointer)
    callStack.push(-2)
    callStack.push(0)
  }

  private[interpreter] def hasPendingNativeCont: Boolean = !nativeContStack.isEmpty

  private[interpreter] def resumeNativeCont(result: Any): (NativeStep, Byte) = {
    if (nativeContStack.isEmpty) {
      throw new IllegalStateException("No pending native continuation")
    }
    val cont = nativeContStack.pop().asInstanceOf[NativeContFrame.HigherOrderCont]
    (cont.k(result), cont.resultTypeTag)
  }

  private[interpreter] def clearTopNativeCont(): Unit = {
    if (!nativeContStack.isEmpty) {
      nativeContStack.pop()
    }
  }

  private def handleNativeStep(step: NativeStep, resultTypeTag: Byte): Unit = {
    step match {
      case NativeStep.Done(value) =>
        NativeResultPusher.pushReturn(resultTypeTag, value, operandStack)
      case _ =>
        throw new UnsupportedOperationException(s"NativeStep $step is not supported yet")
    }
  }

  /**
   * Returns true if the program has finished execution.
   */
  def isDone: Boolean = done

  /**
   * Returns the result of the evaluation. Should only be called when isDone is true.
   */
  def getResult: EvalResult = {
    if (!done) {
      throw new IllegalStateException("Cannot get result: program is still running")
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

  private def transferCaptures(capturedFrame: CapturedFrame,
                               varSpaceSignature: VarSpaceSignature,
                               parameterCount: Int): Unit = {
    val paramCounts = new Array[Int](BasicTypes.MaxValue + 1)
    java.util.Arrays.fill(paramCounts, 0)
    var i = 0
    while (i < parameterCount) {
      val encoded = varSpaceSignature.slot(i)
      val typeTag = VarAddress.decodeBasicType(encoded)
      paramCounts(typeTag) += 1
      i += 1
    }

    val sig = capturedFrame.signature

    if (sig.objectCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Object)
      while (c < sig.objectCount) {
        variableStack.objects.unsafeWrite(base + c, capturedFrame.objects(c))
        c += 1
      }
    }
    if (sig.booleanCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Boolean)
      while (c < sig.booleanCount) {
        variableStack.booleans.unsafeWrite(base + c, capturedFrame.booleans(c))
        c += 1
      }
    }
    if (sig.intCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Int)
      while (c < sig.intCount) {
        variableStack.ints.unsafeWrite(base + c, capturedFrame.ints(c))
        c += 1
      }
    }
    if (sig.longCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Long)
      while (c < sig.longCount) {
        variableStack.longs.unsafeWrite(base + c, capturedFrame.longs(c))
        c += 1
      }
    }
    if (sig.shortCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Short)
      while (c < sig.shortCount) {
        variableStack.shorts.unsafeWrite(base + c, capturedFrame.shorts(c))
        c += 1
      }
    }
    if (sig.byteCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Byte)
      while (c < sig.byteCount) {
        variableStack.bytes.unsafeWrite(base + c, capturedFrame.bytes(c))
        c += 1
      }
    }
    if (sig.charCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Char)
      while (c < sig.charCount) {
        variableStack.chars.unsafeWrite(base + c, capturedFrame.chars(c))
        c += 1
      }
    }
    if (sig.doubleCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Double)
      while (c < sig.doubleCount) {
        variableStack.doubles.unsafeWrite(base + c, capturedFrame.doubles(c))
        c += 1
      }
    }
    if (sig.floatCount > 0) {
      var c = 0
      val base = paramCounts(BasicTypes.Float)
      while (c < sig.floatCount) {
        variableStack.floats.unsafeWrite(base + c, capturedFrame.floats(c))
        c += 1
      }
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

  private def reset(initializer: Initializer,
                    targetFunction: UserFunction): Unit = {
    instructionPointer = 0
    callStack.clear()
    operandStack.clear()
    variableStack.clear()
    nativeContStack.clear()
    variableStack.expandFrame(targetFunction.frameSignature)
    varSpace.setSignature(targetFunction.varSpaceSignature)
    initializer(varSpace)
  }

  private[interpreter] def readAllVariables(): Array[Any] =
    varSpace.readAll()
}

