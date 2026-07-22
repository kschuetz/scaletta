package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.intermediate.IntermediateExpression.Value
import software.kes.scaletta.internal.interpreter._
import software.kes.scaletta.internal.runtime.{FrameSignature, UserFunctionSignature, VarAddress}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
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

  private def findMaxCaptureIndex(expr: IntermediateExpression, parameterCount: Int): Int = {
    var maxIndex = parameterCount - 1

    def scan(e: IntermediateExpression, depth: Int): Unit = {
      e match {
        case IntermediateExpression.Reference(d, slot) =>
          if (d == depth && slot > maxIndex) maxIndex = slot
        case IntermediateExpression.NativeCall(_, arguments) =>
          arguments.foreach(scan(_, depth))
        case IntermediateExpression.LocalCall(d, _, arguments) =>
          if (d == depth) {
            // function slot itself doesn't count as a variable capture
          }
          arguments.foreach(scan(_, depth))
        case IntermediateExpression.ClosureCall(target, arguments, _) =>
          scan(target, depth)
          arguments.foreach(scan(_, depth))
        case IntermediateExpression.Conditional(condition, thenBranch, elseBranch) =>
          scan(condition, depth)
          scan(thenBranch, depth)
          scan(elseBranch, depth)
        case IntermediateExpression.WithBindings(bindings, body) =>
          bindings.foreach {
            case Binding.Val(value) => scan(value, depth)
            case Binding.LazyVal(value) => scan(value, depth + 1)
            case Binding.Def(_, fBody) => scan(fBody, depth + 1)
          }
          scan(body, depth)
        case IntermediateExpression.Lambda(_, captures, body) =>
          captures.foreach(scan(_, depth))
          scan(body, depth + 1)
        case IntermediateExpression.FunctionValue(_, _, _, captures) =>
          captures.foreach(scan(_, depth))
        case IntermediateExpression.PartialNativeFunctionApplication(_, arguments) =>
          arguments.flatten.foreach(scan(_, depth))
        case IntermediateExpression.And(lhs, rhs) =>
          scan(lhs, depth)
          scan(rhs, depth)
        case IntermediateExpression.Or(lhs, rhs) =>
          scan(lhs, depth)
          scan(rhs, depth)
        case IntermediateExpression.StringConcat(segments) =>
          segments.foreach(scan(_, depth))
        case IntermediateExpression.Convert(value, _) =>
          scan(value, depth)
        case IntermediateExpression.Tuple(elements) =>
          elements.foreach(scan(_, depth))
        case IntermediateExpression.Match(scrutinee, cases) =>
          scan(scrutinee, depth)
          cases.foreach { c =>
            c.guard.foreach(scan(_, depth))
            scan(c.body, depth)
          }
        case _: IntermediateExpression.Value => ()
      }
    }

    scan(expr, 0)
    maxIndex
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
                     assembler: Assembler,
                     onStack: Option[BindingInfo] = None): Unit = {
      expression match {
        case v: IntermediateExpression.Value =>
          onStack.foreach(_ => assembler.pop())
          v match {
            case Value.IntValue(value) => assembler.pushImmediateInt(value)
            case Value.LongValue(value) => assembler.pushImmediateLong(value)
            case Value.FloatValue(value) => assembler.pushImmediateFloat(value)
            case Value.DoubleValue(value) => assembler.pushImmediateDouble(value)
            case Value.ShortValue(value) => assembler.pushImmediateShort(value)
            case Value.ByteValue(value) => assembler.pushImmediateByte(value)
            case value: Value.BooleanValue => assembler.pushImmediateBoolean(value.value)
            case Value.CharValue(value) => assembler.pushImmediateChar(value)
            case Value.UnitValue => assembler.pushImmediateObject(scala.runtime.BoxedUnit.UNIT)
            case value: Value.AnyRefValue => assembler.pushImmediateObject(value.value)
          }

        case IntermediateExpression.Reference(scope, slot) =>
          val info = env.resolve(scope, slot)
          if (onStack.contains(info)) {
            // Already on stack
          } else {
            onStack.foreach(_ => assembler.pop())
            info match {
              case BindingInfo.Val(absoluteIndex) =>
                val typ = signature.varSpace.basicTypeOf(absoluteIndex)
                assembler.pushFromVar(typ, absoluteIndex)
              case BindingInfo.LazyVal(absoluteIndex, functionIndex, _) =>
                assembler.lazyEval(absoluteIndex, functionIndex)
              case BindingInfo.Def(_, _) =>
                throw new RuntimeException("Cannot reference a local function as a value")
            }
          }

        case IntermediateExpression.NativeCall(target, arguments) =>
          val nativeFunction = nativeFunctionTable.get(target)
          if (arguments.isEmpty) onStack.foreach(_ => assembler.pop())
          arguments.zipWithIndex.foreach { case (arg, index) =>
            val useOnStack = if (index == 0) onStack else None
            emit(arg, env, signature, assembler, useOnStack)
            val targetType = nativeFunction.params.basicTypeOf(index)
            if (TypeResolver.resolveType(arg, env, signature, nativeFunctionTable) != targetType) {
              assembler.convert(targetType)
            }
          }
          assembler.callNative(target)

        case IntermediateExpression.LocalCall(scope, slot, arguments) =>
          if (arguments.isEmpty) onStack.foreach(_ => assembler.pop())
          arguments.zipWithIndex.foreach { case (arg, index) =>
            val useOnStack = if (index == 0) onStack else None
            emit(arg, env, signature, assembler, useOnStack)
          }
          env.resolve(scope, slot) match {
            case BindingInfo.Def(functionIndex, _) =>
              assembler.callLocal(functionIndex)
            case _ =>
              throw new RuntimeException("Cannot call a value as a function")
          }

        case IntermediateExpression.ClosureCall(target, arguments, _) =>
          if (arguments.nonEmpty) {
            arguments.zipWithIndex.foreach { case (arg, index) =>
              val useOnStack = if (index == 0) onStack else None
              emit(arg, env, signature, assembler, useOnStack)
            }
            if (target == arguments.last) {
              assembler.dup()
            } else {
              emit(target, env, signature, assembler)
            }
          } else {
            emit(target, env, signature, assembler, onStack)
          }
          assembler.callClosure()

        case IntermediateExpression.Lambda(lambdaSignature, captures, lambdaBody) =>
          onStack.foreach(_ => assembler.pop())
          val captureBindings = captures.map(ref => env.resolve(ref.scope, ref.slot))
          val prepared = prepareCaptures(captureBindings, signature, lambdaSignature.parameterCount)
          val added = programBuilder.addFunction(lambdaSignature)
          assembler.makeClosure(added.index, prepared.capturePlan)

          val lambdaInitialEnv = createInitialEnv(lambdaSignature, prepared.captureBindings)
          workQueue.enqueue((lambdaBody, lambdaSignature, lambdaInitialEnv, added))

        case IntermediateExpression.FunctionValue(scope, slot, _, captures) =>
          onStack.foreach(_ => assembler.pop())
          val binding = env.resolve(scope, slot)
          val functionIndex = binding match {
            case BindingInfo.Def(idx, _) => idx
            case _ => throw new RuntimeException("FunctionValue must refer to a def")
          }

          val captureBindings = captures.map(ref => env.resolve(ref.scope, ref.slot))
          val prepared = prepareCaptures(captureBindings, signature, 0)
          assembler.makeClosure(functionIndex, prepared.capturePlan)

        case IntermediateExpression.PartialNativeFunctionApplication(functionId, partialArgs) =>
          onStack.foreach(_ => assembler.pop())
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
          emit(condition, env, signature, assembler, onStack)
          assembler.ifElse(
            emit(thenBranch, env, signature, assembler),
            emit(elseBranch, env, signature, assembler)
          )

        case IntermediateExpression.And(lhs, rhs) =>
          val endLabel = assembler.label()
          emit(lhs, env, signature, assembler, onStack)
          assembler.logicalAnd(endLabel)
          emit(rhs, env, signature, assembler)
          endLabel.bind()

        case IntermediateExpression.Or(lhs, rhs) =>
          val endLabel = assembler.label()
          emit(lhs, env, signature, assembler, onStack)
          assembler.logicalOr(endLabel)
          emit(rhs, env, signature, assembler)
          endLabel.bind()

        case IntermediateExpression.StringConcat(segments) =>
          segments.zipWithIndex.foreach { case (seg, index) =>
            val useOnStack = if (index == 0) onStack else None
            emit(seg, env, signature, assembler, useOnStack)
          }
          assembler.stringConcat(segments.length)

        case IntermediateExpression.Convert(value, targetType) =>
          emit(value, env, signature, assembler, onStack)
          if (TypeResolver.resolveType(value, env, signature, nativeFunctionTable) != targetType) {
            assembler.convert(targetType)
          }

        case IntermediateExpression.Tuple(elements) =>
          elements.zipWithIndex.foreach { case (elem, index) =>
            val useOnStack = if (index == 0) onStack else None
            emit(elem, env, signature, assembler, useOnStack)
          }
          assembler.makeTuple(elements.length)

        case IntermediateExpression.Match(scrutinee, cases) =>
          emitMatch(scrutinee, cases, env, signature, assembler, onStack, tail = false)

        case IntermediateExpression.WithBindings(bindings, body) =>
          var currentLayer = Vector.empty[BindingInfo]
          var newVarCountInBlock = 0
          val discoveredInBlock = mutable.ArrayBuffer.empty[(IntermediateExpression, UserFunctionSignature, Assembler)]
          var currentOnStack = onStack

          bindings.zipWithIndex.foreach { case (b, idx) =>
            val envForBinding = env.pushLayer(currentLayer, newVarCountInBlock)

            b match {
              case Binding.Val(value) =>
                val absoluteIndex = env.nextVarIndex + newVarCountInBlock
                emit(value, envForBinding, signature, assembler, currentOnStack)
                val typ = signature.varSpace.basicTypeOf(absoluteIndex)

                val bindingInfo = BindingInfo.Val(absoluteIndex)
                val isLast = idx == bindings.length - 1
                val envForNext = env.pushLayer(currentLayer :+ bindingInfo, newVarCountInBlock + 1)
                val useDup = if (isLast) startsByReferenceTo(body, envForNext, bindingInfo)
                else (if (idx + 1 < bindings.length) bindings(idx + 1) match {
                  case Binding.Val(nextVal) => startsByReferenceTo(nextVal, envForNext, bindingInfo)
                  case _ => false
                } else false)

                if (useDup) {
                  assembler.dup()
                  currentOnStack = Some(bindingInfo)
                } else {
                  currentOnStack = None
                }
                assembler.popIntoVar(typ, absoluteIndex)
                currentLayer = currentLayer :+ bindingInfo
                newVarCountInBlock += 1

              case Binding.LazyVal(value) =>
                currentOnStack.foreach(_ => assembler.pop())
                currentOnStack = None
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
                currentOnStack.foreach(_ => assembler.pop())
                currentOnStack = None
                val added = programBuilder.addFunction(fSignature)
                currentLayer = currentLayer :+ BindingInfo.Def(added.index, fSignature.returnType)
                discoveredInBlock += ((fBody, fSignature, added))
            }
          }

          val finalEnvForBlock = env.pushLayer(currentLayer, newVarCountInBlock)

          discoveredInBlock.foreach { case (fb, fs, fa) =>
            val maxCaptureIndex = findMaxCaptureIndex(fb, fs.parameterCount)
            val captureCount = math.max(0, maxCaptureIndex - fs.parameterCount + 1)
            val captureBindings = (fs.parameterCount until (fs.parameterCount + captureCount)).toVector.map(BindingInfo.Val)
            val fInitialEnv = createInitialEnv(fs, captureBindings)
            val fEnv = CompileEnv(fInitialEnv.layers ++ finalEnvForBlock.layers, fInitialEnv.nextVarIndex)
            workQueue.enqueue((fb, fs, fEnv, fa))
          }

          emit(body, finalEnvForBlock, signature, assembler, currentOnStack)
      }
    }

    private def emitTail(expression: IntermediateExpression,
                         env: CompileEnv,
                         signature: UserFunctionSignature,
                         assembler: Assembler,
                         onStack: Option[BindingInfo] = None): Unit = {
      expression match {
        case IntermediateExpression.LocalCall(scope, slot, arguments) =>
          env.resolve(scope, slot) match {
            case BindingInfo.Def(functionIndex, _) if functionIndex == assembler.index =>
              if (arguments.isEmpty) onStack.foreach(_ => assembler.pop())
              arguments.zipWithIndex.foreach { case (arg, index) =>
                val useOnStack = if (index == 0) onStack else None
                emit(arg, env, signature, assembler, useOnStack)
              }
              assembler.tailCallLocal(functionIndex)
            case BindingInfo.Def(functionIndex, _) =>
              if (arguments.isEmpty) onStack.foreach(_ => assembler.pop())
              arguments.zipWithIndex.foreach { case (arg, index) =>
                val useOnStack = if (index == 0) onStack else None
                emit(arg, env, signature, assembler, useOnStack)
              }
              assembler.callLocal(functionIndex)
            case _ =>
              throw new RuntimeException("Cannot call a value as a function")
          }

        case IntermediateExpression.Conditional(condition, thenBranch, elseBranch) =>
          emit(condition, env, signature, assembler, onStack)
          assembler.ifElse(
            emitTail(thenBranch, env, signature, assembler),
            emitTail(elseBranch, env, signature, assembler)
          )

        case IntermediateExpression.WithBindings(bindings, body) =>
          var currentLayer = Vector.empty[BindingInfo]
          var newVarCountInBlock = 0
          val discoveredInBlock = mutable.ArrayBuffer.empty[(IntermediateExpression, UserFunctionSignature, Assembler)]
          var currentOnStack = onStack

          bindings.zipWithIndex.foreach { case (b, idx) =>
            val envForBinding = env.pushLayer(currentLayer, newVarCountInBlock)

            b match {
              case Binding.Val(value) =>
                val absoluteIndex = env.nextVarIndex + newVarCountInBlock
                emit(value, envForBinding, signature, assembler, currentOnStack)
                val typ = signature.varSpace.basicTypeOf(absoluteIndex)

                val bindingInfo = BindingInfo.Val(absoluteIndex)
                val isLast = idx == bindings.length - 1
                val envForNext = env.pushLayer(currentLayer :+ bindingInfo, newVarCountInBlock + 1)
                val useDup = if (isLast) startsByReferenceTo(body, envForNext, bindingInfo)
                else (if (idx + 1 < bindings.length) bindings(idx + 1) match {
                  case Binding.Val(nextVal) => startsByReferenceTo(nextVal, envForNext, bindingInfo)
                  case _ => false
                } else false)

                if (useDup) {
                  assembler.dup()
                  currentOnStack = Some(bindingInfo)
                } else {
                  currentOnStack = None
                }
                assembler.popIntoVar(typ, absoluteIndex)
                currentLayer = currentLayer :+ bindingInfo
                newVarCountInBlock += 1

              case Binding.LazyVal(value) =>
                currentOnStack.foreach(_ => assembler.pop())
                currentOnStack = None
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
                currentOnStack.foreach(_ => assembler.pop())
                currentOnStack = None
                val added = programBuilder.addFunction(fSignature)
                currentLayer = currentLayer :+ BindingInfo.Def(added.index, fSignature.returnType)
                discoveredInBlock += ((fBody, fSignature, added))
            }
          }

          val finalEnvForBlock = env.pushLayer(currentLayer, newVarCountInBlock)

          discoveredInBlock.foreach { case (fb, fs, fa) =>
            val maxCaptureIndex = findMaxCaptureIndex(fb, fs.parameterCount)
            val captureCount = math.max(0, maxCaptureIndex - fs.parameterCount + 1)
            val captureBindings = (fs.parameterCount until (fs.parameterCount + captureCount)).toVector.map(BindingInfo.Val)
            val fInitialEnv = createInitialEnv(fs, captureBindings)
            val fEnv = CompileEnv(fInitialEnv.layers ++ finalEnvForBlock.layers, fInitialEnv.nextVarIndex)
            workQueue.enqueue((fb, fs, fEnv, fa))
          }

          emitTail(body, finalEnvForBlock, signature, assembler, currentOnStack)

        case IntermediateExpression.Convert(value, targetType) =>
          if (TypeResolver.resolveType(value, env, signature, nativeFunctionTable) == targetType) {
            emitTail(value, env, signature, assembler, onStack)
          } else {
            emit(expression, env, signature, assembler, onStack)
          }

        case IntermediateExpression.Match(scrutinee, cases) =>
          emitMatch(scrutinee, cases, env, signature, assembler, onStack, tail = true)

        case other =>
          emit(other, env, signature, assembler, onStack)
      }
    }

    private val tupleUnapplyStrategy = software.kes.scaletta.api.UnapplyStrategy.unapplyDynamic { (arity, value) =>
      value match {
        case t: Product if t.productArity == arity => software.kes.scaletta.api.UnapplyResult.success(t.productIterator.toSeq)
        case _ => software.kes.scaletta.api.UnapplyResult.failure
      }
    }

    private def emitMatch(scrutinee: IntermediateExpression,
                          cases: software.kes.scaletta.util.NonEmptyVector[Case],
                          env: CompileEnv,
                          signature: UserFunctionSignature,
                          assembler: Assembler,
                          onStack: Option[BindingInfo],
                          tail: Boolean): Unit = {
      val endLabel = assembler.label()
      val scrutineeType = TypeResolver.resolveType(scrutinee, env, signature, nativeFunctionTable)
      val scrutineeSlot = env.nextVarIndex
      val envWithTemp = env.copy(
        layers = (env.layers.head :+ BindingInfo.Val(scrutineeSlot)) :: env.layers.tail,
        nextVarIndex = env.nextVarIndex + 1
      )

      emit(scrutinee, env, signature, assembler, None)
      assembler.popIntoVar(scrutineeType, scrutineeSlot)

      cases.zipWithIndex.foreach { case (c, idx) =>
        val envForCase = growEnvForPattern(c.pattern, envWithTemp, signature)
        val nextCase = assembler.label()

        assembler.pushFromVar(scrutineeType, scrutineeSlot)
        emitPattern(c.pattern, envForCase, signature, assembler, nextCase)

        c.guard.foreach { g =>
          emit(g, envForCase, signature, assembler)
          assembler.branchUnless(nextCase)
        }

        if (tail) emitTail(c.body, envForCase, signature, assembler)
        else emit(c.body, envForCase, signature, assembler)

        if (idx < cases.length - 1) {
          assembler.branch(endLabel)
        }

        nextCase.bind()
      }
      endLabel.bind()
    }

    private def emitPattern(pattern: Pattern,
                            env: CompileEnv,
                            signature: UserFunctionSignature,
                            assembler: Assembler,
                            onFailure: Assembler.Label): Unit = {
      pattern match {
        case Pattern.Wildcard =>
          assembler.pop()

        case Pattern.Slot(scope, slot) =>
          val info = env.resolve(scope, slot)
          info match {
            case BindingInfo.Val(absIndex) =>
              val typ = signature.varSpace.basicTypeOf(absIndex)
              assembler.popIntoVar(typ, absIndex)
            case _ => throw new RuntimeException("Slot pattern must refer to a val")
          }

        case Pattern.Literal(v) =>
          val rawLit = getRawValue(v)
          val predicate: Any => Boolean = _ == rawLit
          assembler.dup()
          assembler.pushImmediateObject(predicate)
          assembler.applyPredicate()
          val success = assembler.label()
          assembler.branchIf(success)
          assembler.pop()
          assembler.branch(onFailure)
          success.bind()
          assembler.pop()

        case Pattern.Typed(inner, typeInfo) =>
          assembler.dup()
          assembler.pushImmediateObject(typeInfo.isInstance)
          assembler.applyPredicate()
          val success = assembler.label()
          assembler.branchIf(success)
          assembler.pop()
          assembler.branch(onFailure)
          success.bind()
          emitPattern(inner, env, signature, assembler, onFailure)

        case Pattern.Tuple(elements) =>
          assembler.dup()
          assembler.pushImmediateObject(tupleUnapplyStrategy)
          assembler.pushImmediateInt(elements.length)
          assembler.unapply()
          val success = assembler.label()
          assembler.branchIf(success)
          assembler.pop()
          assembler.branch(onFailure)

          success.bind()
          elements.zipWithIndex.reverse.foreach { case (elem, i) =>
            val elementSuccess = assembler.label()
            val elementFailure = assembler.label()
            emitPattern(elem, env, signature, assembler, elementFailure)
            assembler.branch(elementSuccess)

            elementFailure.bind()
            (0 until i).foreach(_ => assembler.pop())
            assembler.pop()
            assembler.branch(onFailure)

            elementSuccess.bind()
          }
          assembler.pop()

        case Pattern.Product(typeInfo, args) =>
          assembler.dup()
          assembler.pushImmediateObject(typeInfo.unapplyStrategy)
          assembler.pushImmediateInt(args.length)
          assembler.unapply()
          val success = assembler.label()
          assembler.branchIf(success)
          assembler.pop()
          assembler.branch(onFailure)

          success.bind()
          args.zipWithIndex.reverse.foreach { case (arg, i) =>
            val argSuccess = assembler.label()
            val argFailure = assembler.label()
            emitPattern(arg, env, signature, assembler, argFailure)
            assembler.branch(argSuccess)

            argFailure.bind()
            (0 until i).foreach(_ => assembler.pop())
            assembler.pop()
            assembler.branch(onFailure)

            argSuccess.bind()
          }
          assembler.pop()

        case Pattern.As(scope, slot, inner) =>
          val info = env.resolve(scope, slot)
          info match {
            case BindingInfo.Val(absIndex) =>
              val typ = signature.varSpace.basicTypeOf(absIndex)
              assembler.dup()
              assembler.popIntoVar(typ, absIndex)
              emitPattern(inner, env, signature, assembler, onFailure)
            case _ => throw new RuntimeException("As pattern must refer to a val")
          }
      }
    }

    private def growEnvForPattern(pattern: Pattern,
                                  env: CompileEnv,
                                  signature: UserFunctionSignature): CompileEnv = {
      val slots = mutable.Set.empty[Int]

      def collect(p: Pattern): Unit = p match {
        case Pattern.Slot(0, slot) => slots += slot
        case Pattern.As(0, slot, inner) =>
          slots += slot
          collect(inner)
        case Pattern.Typed(inner, _) => collect(inner)
        case Pattern.Tuple(elements) => elements.foreach(collect)
        case Pattern.Product(_, args) => args.foreach(collect)
        case _ => ()
      }

      collect(pattern)

      if (slots.isEmpty) env
      else {
        val maxSlot = slots.max
        val currentLayer = env.layers.head
        if (maxSlot < currentLayer.size) env
        else {
          val baseIndex = env.nextVarIndex - currentLayer.size
          val newBindings = (currentLayer.size to maxSlot).toVector.map { i =>
            BindingInfo.Val(baseIndex + i)
          }
          val newLayer = currentLayer ++ newBindings
          env.copy(layers = newLayer :: env.layers.tail, nextVarIndex = baseIndex + newLayer.size)
        }
      }
    }

    private def getRawValue(v: Value): Any = v match {
      case Value.IntValue(x) => x
      case Value.LongValue(x) => x
      case Value.FloatValue(x) => x
      case Value.DoubleValue(x) => x
      case Value.ShortValue(x) => x
      case Value.ByteValue(x) => x
      case v: Value.BooleanValue => v.value
      case Value.CharValue(x) => x
      case Value.UnitValue => scala.runtime.BoxedUnit.UNIT
      case Value.StringValue(x) => x
      case Value.ObjectValue(x) => x
      case Value.Null => null
    }

    @tailrec
    private def startsByReferenceTo(expr: IntermediateExpression,
                                    env: CompileEnv,
                                    info: BindingInfo): Boolean = {
      expr match {
        case IntermediateExpression.Reference(s, l) =>
          try {
            env.resolve(s, l) == info
          } catch {
            case _: Throwable => false
          }
        case IntermediateExpression.Convert(v, _) => startsByReferenceTo(v, env, info)
        case IntermediateExpression.NativeCall(_, args) => args.nonEmpty && startsByReferenceTo(args.head, env, info)
        case IntermediateExpression.LocalCall(_, _, args) => args.nonEmpty && startsByReferenceTo(args.head, env, info)
        case IntermediateExpression.ClosureCall(target, args, _) =>
          if (args.nonEmpty) startsByReferenceTo(args.head, env, info)
          else startsByReferenceTo(target, env, info)
        case IntermediateExpression.Tuple(elements) => startsByReferenceTo(elements.head, env, info)
        case _ => false
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

      val captureSignature = CaptureSignature.create(
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

      val capturePlan = CapturePlan.create(captureSignature, ArraySeq.unsafeWrapArray(sourceIndices), ArraySeq.unsafeWrapArray(targetEncoded))
      PreparedCaptures(capturePlan, captureBindings)
    }

  }

}
