package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.common.{BasicType, BasicTypes}
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.runtime.UserFunctionSignature

import scala.annotation.tailrec

object TypeResolver {
  @tailrec
  def resolveType(expression: IntermediateExpression,
                  env: CompileEnv,
                  signature: UserFunctionSignature,
                  nativeTable: NativeFunctionTable): BasicType = {
    expression match {
      case v: IntermediateExpression.Value =>
        v match {
          case _: IntermediateExpression.Value.IntValue => BasicTypes.Int
          case _: IntermediateExpression.Value.LongValue => BasicTypes.Long
          case _: IntermediateExpression.Value.FloatValue => BasicTypes.Float
          case _: IntermediateExpression.Value.DoubleValue => BasicTypes.Double
          case _: IntermediateExpression.Value.ShortValue => BasicTypes.Short
          case _: IntermediateExpression.Value.ByteValue => BasicTypes.Byte
          case _: IntermediateExpression.Value.BooleanValue => BasicTypes.Boolean
          case _: IntermediateExpression.Value.CharValue => BasicTypes.Char
          case _: IntermediateExpression.Value.AnyRefValue => BasicTypes.Object
        }

      case IntermediateExpression.Reference(scope, slot) =>
        env.resolve(scope, slot) match {
          case BindingInfo.Val(absoluteIndex) =>
            signature.varSpace.basicTypeOf(absoluteIndex)
          case BindingInfo.LazyVal(_, _, basicType) =>
            basicType
          case BindingInfo.Def(_, _) =>
            throw new RuntimeException("Cannot reference a local function as a value")
        }

      case IntermediateExpression.NativeCall(target, _) =>
        nativeTable.get(target).returnType.toByte

      case IntermediateExpression.LocalCall(scope, slot, _) =>
        env.resolve(scope, slot) match {
          case BindingInfo.Def(_, returnType) =>
            returnType
          case _ =>
            throw new RuntimeException("Cannot call a value as a function")
        }

      case IntermediateExpression.Conditional(_, thenBranch, _) =>
        // Assume both branches have the same type, as validated by TypeChecker earlier in the pipeline
        resolveType(thenBranch, env, signature, nativeTable)

      case IntermediateExpression.And(_, _) =>
        BasicTypes.Boolean

      case IntermediateExpression.Or(_, _) =>
        BasicTypes.Boolean

      case IntermediateExpression.StringConcat(_) =>
        BasicTypes.Object

      case IntermediateExpression.WithBindings(bindings, body) =>
        var currentLayer = Vector.empty[BindingInfo]
        var newVarCountInBlock = 0
        bindings.foreach {
          case Binding.Val(_) =>
            val absoluteIndex = env.nextVarIndex + newVarCountInBlock
            currentLayer = currentLayer :+ BindingInfo.Val(absoluteIndex)
            newVarCountInBlock += 1
          case Binding.LazyVal(_, underlyingType) =>
            val absoluteIndex = env.nextVarIndex + newVarCountInBlock
            currentLayer = currentLayer :+ BindingInfo.LazyVal(absoluteIndex, -1, underlyingType)
            newVarCountInBlock += 1
          case Binding.Def(fSignature, _) =>
            currentLayer = currentLayer :+ BindingInfo.Def(-1, fSignature.returnType)
        }
        val finalEnvForBlock = env.pushLayer(currentLayer, newVarCountInBlock)
        resolveType(body, finalEnvForBlock, signature, nativeTable)
    }
  }
}
