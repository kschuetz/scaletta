package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.common.{BasicType, BasicTypes}
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.interpreter.{Assembler, Program, ProgramBuilder}
import software.kes.scaletta.internal.runtime.UserFunctionSignature

import scala.collection.mutable

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
      emit(expression, initialEnv, signature, assembler)
    }

    def emitDiscoveredFunctions(): Unit = {
      while (workQueue.nonEmpty) {
        val (expression, signature, env, assembler) = workQueue.dequeue()
        emit(expression, env, signature, assembler)
      }
    }

    private def createInitialEnv(signature: UserFunctionSignature): CompileEnv = {
      val paramCount = signature.parameterCount
      val layer = (0 until paramCount).toVector.map(BindingInfo.Val)
      CompileEnv(List(layer), paramCount)
    }

    private def emit(expression: IntermediateExpression,
                     env: CompileEnv,
                     signature: UserFunctionSignature,
                     assembler: Assembler): Unit = {
      expression match {
        case v: IntermediateExpression.Value =>
          assembler.pushImmediate(v.asAny)

        case IntermediateExpression.Reference(scope, slot) =>
          env.resolve(scope, slot) match {
            case BindingInfo.Val(absoluteIndex) =>
              val typ = signature.varSpace.basicTypeOf(absoluteIndex)
              pushFromVar(assembler, typ, absoluteIndex)
            case BindingInfo.LazyVal(absoluteIndex, functionIndex, _) =>
              assembler.lazyEval(absoluteIndex, functionIndex)
            case BindingInfo.Def(_, _) =>
              throw new RuntimeException("Cannot reference a local function as a value")
          }

        case IntermediateExpression.NativeCall(target, arguments) =>
          val nativeFunction = nativeFunctionTable.get(target)
          arguments.zipWithIndex.foreach { case (arg, index) =>
            emit(arg, env, signature, assembler)
            assembler.convert(nativeFunction.params.basicTypeOf(index))
          }
          assembler.callNative(target)

        case IntermediateExpression.LocalCall(scope, slot, arguments) =>
          arguments.foreach(arg => emit(arg, env, signature, assembler))
          env.resolve(scope, slot) match {
            case BindingInfo.Def(functionIndex, _) =>
              assembler.callLocal(functionIndex)
            case BindingInfo.Val(_) =>
              throw new RuntimeException("Cannot call a value as a function")
          }

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
                popIntoVar(assembler, typ, absoluteIndex)
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
            val fInitialEnv = createInitialEnv(fs)
            val fEnv = CompileEnv(fInitialEnv.layers ++ finalEnvForBlock.layers, fInitialEnv.nextVarIndex)
            workQueue.enqueue((fb, fs, fEnv, fa))
          }

          emit(body, finalEnvForBlock, signature, assembler)
      }
    }

    private def pushFromVar(assembler: Assembler, typ: BasicType, absoluteIndex: Int): Unit = {
      import software.kes.scaletta.common.BasicTypes._
      typ match {
        case Int => assembler.pushIntFromVar(absoluteIndex)
        case Long => assembler.pushLongFromVar(absoluteIndex)
        case Double => assembler.pushDoubleFromVar(absoluteIndex)
        case Float => assembler.pushFloatFromVar(absoluteIndex)
        case Byte => assembler.pushByteFromVar(absoluteIndex)
        case Short => assembler.pushShortFromVar(absoluteIndex)
        case Char => assembler.pushCharFromVar(absoluteIndex)
        case Boolean => assembler.pushBooleanFromVar(absoluteIndex)
        case Object => assembler.pushObjectFromVar(absoluteIndex)
        case _ => throw new RuntimeException(s"Unsupported type for pushFromVar: $typ")
      }
    }

    private def popIntoVar(assembler: Assembler, typ: BasicType, absoluteIndex: Int): Unit = {
      import software.kes.scaletta.common.BasicTypes._
      typ match {
        case Int => assembler.popIntIntoVar(absoluteIndex)
        case Long => assembler.popLongIntoVar(absoluteIndex)
        case Double => assembler.popDoubleIntoVar(absoluteIndex)
        case Float => assembler.popFloatIntoVar(absoluteIndex)
        case Byte => assembler.popByteIntoVar(absoluteIndex)
        case Short => assembler.popShortIntoVar(absoluteIndex)
        case Char => assembler.popCharIntoVar(absoluteIndex)
        case Boolean => assembler.popBooleanIntoVar(absoluteIndex)
        case Object => assembler.popObjectIntoVar(absoluteIndex)
        case _ => throw new RuntimeException(s"Unsupported type for popIntoVar: $typ")
      }
    }
  }

}
