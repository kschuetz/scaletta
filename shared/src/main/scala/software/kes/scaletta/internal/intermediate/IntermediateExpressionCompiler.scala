package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.intermediate.IntermediateExpression.Value
import software.kes.scaletta.internal.interpreter._
import software.kes.scaletta.internal.runtime.{FrameSignature, UserFunctionSignature, VarAddress}

import scala.collection.mutable

private final case class PreparedCaptures(capturePlan: CapturePlan, captureBindings: Vector[BindingInfo])

final class IntermediateExpressionCompiler(nativeFunctionTable: NativeFunctionTable) {

  def compile(mainSignature: UserFunctionSignature,
              expression: IntermediateExpression): Program = {
    val programBuilder = ProgramBuilder.create(mainSignature)
    val emitter = new Emitter(programBuilder)
    emitter.emitMain(expression, mainSignature)
    emitter.emitDiscoveredFunctions()
    programBuilder.build()
  }

  private final class Emitter(programBuilder: ProgramBuilder) {
    private val workQueue = mutable.Queue[(IntermediateExpression, UserFunctionSignature, CompileEnv, Assembler)]()

    def emitMain(expression: IntermediateExpression, signature: UserFunctionSignature): Unit = {
      val assembler = programBuilder.mainAssembler()
      val initialEnv = createInitialEnv(signature)
      emitTail(expression, initialEnv, signature, assembler)
    }

    def emitDiscoveredFunctions(): Unit = {
      while (workQueue.nonEmpty) {
        val (expression, signature, env, assembler) = workQueue.dequeue()
        emitTail(expression, env, signature, assembler)
      }
    }

    private def createInitialEnv(signature: UserFunctionSignature,
                                 captureBindings: Vector[BindingInfo] = Vector.empty): CompileEnv = {
      val paramCount = signature.parameterCount
      val paramBindings = (0 until paramCount).toVector.map(BindingInfo.Val)
      val layer = paramBindings ++ captureBindings
      CompileEnv(List(layer), layer.length)
    }

    private def emit(expression: IntermediateExpression,
                     env: CompileEnv,
                     signature: UserFunctionSignature,
                     assembler: Assembler): Unit = {
      expression match {
        case v: IntermediateExpression.Value =>
          v match {
            case Value.IntValue(value) => assembler.pushImmediateInt(value)
            case Value.LongValue(value) => assembler.pushImmediateLong(value)
            case Value.FloatValue(value) => assembler.pushImmediateFloat(value)
            case Value.DoubleValue(value) => assembler.pushImmediateDouble(value)
            case Value.ShortValue(value) => assembler.pushImmediateShort(value)
            case Value.ByteValue(value) => assembler.pushImmediateByte(value)
            case value: Value.BooleanValue => assembler.pushImmediateBoolean(value.value)
            case Value.CharValue(value) => assembler.pushImmediateChar(value)
            case value: Value.AnyRefValue => assembler.pushImmediateObject(value.value)
          }

        case IntermediateExpression.Reference(scope, slot) =>
          env.resolve(scope, slot) match {
            case BindingInfo.Val(absoluteIndex) =>
              val typ = signature.varSpace.basicTypeOf(absoluteIndex)
              assembler.pushFromVar(typ, absoluteIndex)
            case BindingInfo.LazyVal(absoluteIndex, functionIndex, _) =>
              assembler.lazyEval(absoluteIndex, functionIndex)
            case BindingInfo.Def(_, _) =>
              throw new RuntimeException("Cannot reference a local function as a value")
          }

        case IntermediateExpression.NativeCall(target, arguments) =>
          val nativeFunction = nativeFunctionTable.get(target)
          arguments.zipWithIndex.foreach { case (arg, index) =>
            emit(arg, env, signature, assembler)
            val targetType = nativeFunction.params.basicTypeOf(index)
            if (TypeResolver.resolveType(arg, env, signature, nativeFunctionTable) != targetType) {
              assembler.convert(targetType)
            }
          }
          assembler.callNative(target)

        case IntermediateExpression.LocalCall(scope, slot, arguments) =>
          arguments.foreach(arg => emit(arg, env, signature, assembler))
          env.resolve(scope, slot) match {
            case BindingInfo.Def(functionIndex, _) =>
              assembler.callLocal(functionIndex)
            case _ =>
              throw new RuntimeException("Cannot call a value as a function")
          }

        case IntermediateExpression.ClosureCall(target, arguments, _) =>
          arguments.foreach(arg => emit(arg, env, signature, assembler))
          emit(target, env, signature, assembler)
          assembler.callClosure()

        case IntermediateExpression.Lambda(lambdaSignature, captures, lambdaBody) =>
          val captureBindings = captures.map(ref => env.resolve(ref.scope, ref.slot))
          val prepared = prepareCaptures(captureBindings, signature, lambdaSignature.parameterCount)
          val added = programBuilder.addFunction(lambdaSignature)
          assembler.makeClosure(added.index, prepared.capturePlan)

          val lambdaInitialEnv = createInitialEnv(lambdaSignature, prepared.captureBindings)
          workQueue.enqueue((lambdaBody, lambdaSignature, lambdaInitialEnv, added))

        case IntermediateExpression.FunctionValue(scope, slot, _, captures) =>
          val binding = env.resolve(scope, slot)
          val functionIndex = binding match {
            case BindingInfo.Def(idx, _) => idx
            case _ => throw new RuntimeException("FunctionValue must refer to a def")
          }

          val captureBindings = captures.map(ref => env.resolve(ref.scope, ref.slot))
          val prepared = prepareCaptures(captureBindings, signature, 0)
          assembler.makeClosure(functionIndex, prepared.capturePlan)

        case IntermediateExpression.PartialNativeFunctionApplication(functionId, partialArgs) =>
          val nativeFunction = nativeFunctionTable.get(functionId)
          val nativeParams = nativeFunction.params

          val holes = partialArgs.zipWithIndex.collect { case (None, i) => i }
          val prefilled = partialArgs.zipWithIndex.collect { case (Some(expr), i) => (expr, i) }

          var currentNextVarIndex = env.nextVarIndex
          val captureBindings = prefilled.map { case (expr, origIdx) =>
            expr match {
              case ref: IntermediateExpression.Reference =>
                env.resolve(ref.scope, ref.slot)
              case other =>
                val typ = nativeParams.basicTypeOf(origIdx)
                val tempIndex = currentNextVarIndex
                currentNextVarIndex += 1
                emit(other, env.copy(nextVarIndex = currentNextVarIndex), signature, assembler)
                assembler.popIntoVar(typ, tempIndex)
                BindingInfo.Val(tempIndex)
            }
          }

          val holeTypes = holes.map(i => nativeParams.basicTypeOf(i))
          val captureTypes = prefilled.map { case (_, i) => nativeParams.basicTypeOf(i) }

          val syntheticFrame = FrameSignature.fromBasicTypes(holeTypes ++ captureTypes)
          val syntheticSignature = UserFunctionSignature(
            software.kes.scaletta.internal.runtime.VarSpaceSignature.of(syntheticFrame),
            nativeFunction.returnType.toByte,
            holes.size
          )

          val prepared = prepareCaptures(captureBindings, signature, holes.size)
          val added = programBuilder.addFunction(syntheticSignature)
          assembler.makeClosure(added.index, prepared.capturePlan)

          val bodyHoleArgs = holes.zipWithIndex.map { case (_, k) => IntermediateExpression.Reference(0, k) }
          val bodyCapturedArgs = prefilled.zipWithIndex.map { case (_, m) => IntermediateExpression.Reference(0, holes.size + m) }

          val fullArgs = new Array[IntermediateExpression](partialArgs.size)
          holes.zipWithIndex.foreach { case (origIdx, k) => fullArgs(origIdx) = bodyHoleArgs(k) }
          prefilled.zipWithIndex.foreach { case ((_, origIdx), m) => fullArgs(origIdx) = bodyCapturedArgs(m) }

          val bodyNativeCall = IntermediateExpression.NativeCall(functionId, fullArgs.toVector)

          val lambdaInitialEnv = createInitialEnv(syntheticSignature, prepared.captureBindings)
          workQueue.enqueue((bodyNativeCall, syntheticSignature, lambdaInitialEnv, added))

        case IntermediateExpression.Conditional(condition, thenBranch, elseBranch) =>
          emit(condition, env, signature, assembler)
          assembler.ifElse(
            emit(thenBranch, env, signature, assembler),
            emit(elseBranch, env, signature, assembler)
          )

        case IntermediateExpression.And(lhs, rhs) =>
          val endLabel = assembler.label()
          emit(lhs, env, signature, assembler)
          assembler.logicalAnd(endLabel)
          emit(rhs, env, signature, assembler)
          endLabel.bind()

        case IntermediateExpression.Or(lhs, rhs) =>
          val endLabel = assembler.label()
          emit(lhs, env, signature, assembler)
          assembler.logicalOr(endLabel)
          emit(rhs, env, signature, assembler)
          endLabel.bind()

        case IntermediateExpression.StringConcat(segments) =>
          segments.foreach(seg => emit(seg, env, signature, assembler))
          assembler.stringConcat(segments.length)

        case IntermediateExpression.Convert(value, targetType) =>
          emit(value, env, signature, assembler)
          if (TypeResolver.resolveType(value, env, signature, nativeFunctionTable) != targetType) {
            assembler.convert(targetType)
          }

        case IntermediateExpression.WithBindings(bindings, body) =>
          var currentLayer = Vector.empty[BindingInfo]
          var newVarCountInBlock = 0
          val discoveredInBlock = mutable.ArrayBuffer.empty[(IntermediateExpression, UserFunctionSignature, Assembler)]

          bindings.foreach { b =>
            val envForBinding = env.pushLayer(currentLayer, newVarCountInBlock)

            b match {
              case Binding.Val(value) =>
                val absoluteIndex = env.nextVarIndex + newVarCountInBlock
                emit(value, envForBinding, signature, assembler)
                val typ = signature.varSpace.basicTypeOf(absoluteIndex)
                assembler.popIntoVar(typ, absoluteIndex)
                currentLayer = currentLayer :+ BindingInfo.Val(absoluteIndex)
                newVarCountInBlock += 1

              case Binding.LazyVal(value) =>
                val absoluteIndex = env.nextVarIndex + newVarCountInBlock

                val placeholder = BindingInfo.LazyVal(absoluteIndex, -1, BasicTypes.Object)
                val envForRhs = env.pushLayer(currentLayer :+ placeholder, newVarCountInBlock + 1)
                  .pushLayer(Vector.empty, 0)
                val underlyingType = TypeResolver.resolveType(value, envForRhs, signature, nativeFunctionTable)

                assembler.lazyInit(underlyingType, absoluteIndex)

                val evalSignature = UserFunctionSignature(
                  signature.varSpace.pushFrame(software.kes.scaletta.internal.runtime.FrameSignature.empty),
                  underlyingType,
                  0
                )
                val added = programBuilder.addFunction(evalSignature)

                currentLayer = currentLayer :+ BindingInfo.LazyVal(absoluteIndex, added.index, underlyingType)
                discoveredInBlock += ((value, evalSignature, added))
                newVarCountInBlock += 1

              case Binding.Def(fSignature, fBody) =>
                val added = programBuilder.addFunction(fSignature)
                currentLayer = currentLayer :+ BindingInfo.Def(added.index, fSignature.returnType)
                discoveredInBlock += ((fBody, fSignature, added))
            }
          }

          val finalEnvForBlock = env.pushLayer(currentLayer, newVarCountInBlock)

          discoveredInBlock.foreach { case (fb, fs, fa) =>
            val captureCount = fs.varSpace.slotCount - fs.parameterCount
            val captureBindings = (fs.parameterCount until fs.varSpace.slotCount).toVector.map(BindingInfo.Val)
            val fInitialEnv = createInitialEnv(fs, captureBindings)
            val fEnv = CompileEnv(fInitialEnv.layers ++ finalEnvForBlock.layers, fInitialEnv.nextVarIndex)
            workQueue.enqueue((fb, fs, fEnv, fa))
          }

          emit(body, finalEnvForBlock, signature, assembler)
      }
    }

    private def emitTail(expression: IntermediateExpression,
                         env: CompileEnv,
                         signature: UserFunctionSignature,
                         assembler: Assembler): Unit = {
      expression match {
        case IntermediateExpression.LocalCall(scope, slot, arguments) =>
          env.resolve(scope, slot) match {
            case BindingInfo.Def(functionIndex, _) if functionIndex == assembler.index =>
              arguments.foreach(arg => emit(arg, env, signature, assembler))
              assembler.tailCallLocal(functionIndex)
            case BindingInfo.Def(functionIndex, _) =>
              arguments.foreach(arg => emit(arg, env, signature, assembler))
              assembler.callLocal(functionIndex)
            case _ =>
              throw new RuntimeException("Cannot call a value as a function")
          }

        case IntermediateExpression.Conditional(condition, thenBranch, elseBranch) =>
          emit(condition, env, signature, assembler)
          assembler.ifElse(
            emitTail(thenBranch, env, signature, assembler),
            emitTail(elseBranch, env, signature, assembler)
          )

        case IntermediateExpression.WithBindings(bindings, body) =>
          var currentLayer = Vector.empty[BindingInfo]
          var newVarCountInBlock = 0
          val discoveredInBlock = mutable.ArrayBuffer.empty[(IntermediateExpression, UserFunctionSignature, Assembler)]

          bindings.foreach { b =>
            val envForBinding = env.pushLayer(currentLayer, newVarCountInBlock)

            b match {
              case Binding.Val(value) =>
                val absoluteIndex = env.nextVarIndex + newVarCountInBlock
                emit(value, envForBinding, signature, assembler)
                val typ = signature.varSpace.basicTypeOf(absoluteIndex)
                assembler.popIntoVar(typ, absoluteIndex)
                currentLayer = currentLayer :+ BindingInfo.Val(absoluteIndex)
                newVarCountInBlock += 1

              case Binding.LazyVal(value) =>
                val absoluteIndex = env.nextVarIndex + newVarCountInBlock

                val placeholder = BindingInfo.LazyVal(absoluteIndex, -1, BasicTypes.Object)
                val envForRhs = env.pushLayer(currentLayer :+ placeholder, newVarCountInBlock + 1)
                  .pushLayer(Vector.empty, 0)
                val underlyingType = TypeResolver.resolveType(value, envForRhs, signature, nativeFunctionTable)

                assembler.lazyInit(underlyingType, absoluteIndex)

                val evalSignature = UserFunctionSignature(
                  signature.varSpace.pushFrame(software.kes.scaletta.internal.runtime.FrameSignature.empty),
                  underlyingType,
                  0
                )
                val added = programBuilder.addFunction(evalSignature)

                currentLayer = currentLayer :+ BindingInfo.LazyVal(absoluteIndex, added.index, underlyingType)
                discoveredInBlock += ((value, evalSignature, added))
                newVarCountInBlock += 1

              case Binding.Def(fSignature, fBody) =>
                val added = programBuilder.addFunction(fSignature)
                currentLayer = currentLayer :+ BindingInfo.Def(added.index, fSignature.returnType)
                discoveredInBlock += ((fBody, fSignature, added))
            }
          }

          val finalEnvForBlock = env.pushLayer(currentLayer, newVarCountInBlock)

          discoveredInBlock.foreach { case (fb, fs, fa) =>
            val captureBindings = (fs.parameterCount until fs.varSpace.slotCount).toVector.map(BindingInfo.Val)
            val fInitialEnv = createInitialEnv(fs, captureBindings)
            val fEnv = CompileEnv(fInitialEnv.layers ++ finalEnvForBlock.layers, fInitialEnv.nextVarIndex)
            workQueue.enqueue((fb, fs, fEnv, fa))
          }

          emitTail(body, finalEnvForBlock, signature, assembler)

        case IntermediateExpression.Convert(value, targetType) =>
          if (TypeResolver.resolveType(value, env, signature, nativeFunctionTable) == targetType) {
            emitTail(value, env, signature, assembler)
          } else {
            emit(expression, env, signature, assembler)
          }

        case other =>
          emit(other, env, signature, assembler)
      }
    }

    private def prepareCaptures(inputCaptures: Vector[BindingInfo],
                                signature: UserFunctionSignature,
                                targetBaseIndex: Int): PreparedCaptures = {
      val sourceIndices = new Array[Int](inputCaptures.length)
      val targetEncoded = new Array[Int](inputCaptures.length)
      val counts = new Array[Int](BasicTypes.MaxValue + 1)

      val captureBindings = inputCaptures.zipWithIndex.map { case (binding, i) =>
        val absIndex = binding match {
          case BindingInfo.Val(idx) => idx
          case BindingInfo.LazyVal(idx, _, _) => idx
          case _ => throw new RuntimeException("Capture must be a val or lazy val")
        }
        sourceIndices(i) = absIndex
        val typ = signature.varSpace.basicTypeOf(absIndex)
        val offset = counts(typ)
        counts(typ) += 1
        targetEncoded(i) = VarAddress.encode(typ, offset)

        val newAbsIndex = targetBaseIndex + i
        binding match {
          case BindingInfo.Val(_) => BindingInfo.Val(newAbsIndex)
          case BindingInfo.LazyVal(_, functionIndex, basicType) =>
            BindingInfo.LazyVal(newAbsIndex, functionIndex, basicType)
          case _ => throw new RuntimeException("Unreachable")
        }
      }

      val captureSignature = new CaptureSignature(
        objectCount = counts(BasicTypes.Object),
        booleanCount = counts(BasicTypes.Boolean),
        intCount = counts(BasicTypes.Int),
        longCount = counts(BasicTypes.Long),
        shortCount = counts(BasicTypes.Short),
        byteCount = counts(BasicTypes.Byte),
        charCount = counts(BasicTypes.Char),
        doubleCount = counts(BasicTypes.Double),
        floatCount = counts(BasicTypes.Float)
      )

      val capturePlan = new CapturePlan(captureSignature, sourceIndices, targetEncoded)
      PreparedCaptures(capturePlan, captureBindings)
    }

  }

}
