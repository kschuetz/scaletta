package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.{Type, TypeId}
import software.kes.scaletta.common.{BasicType, BasicTypes}
import software.kes.scaletta.internal.builtins.{NativeFunction, NativeFunctionDefinition}

object NativeResultPusher {
  def pushReturn(basicType: BasicType, value: Any, operandStack: OperandStack): Unit = {
    (basicType: @annotation.switch) match {
      case BasicTypes.Boolean =>
        value match {
          case b: java.lang.Boolean => operandStack.pushBoolean(b.booleanValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Int =>
        value match {
          case i: java.lang.Integer => operandStack.pushInt(i.intValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Long =>
        value match {
          case l: java.lang.Long => operandStack.pushLong(l.longValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Short =>
        value match {
          case s: java.lang.Short => operandStack.pushShort(s.shortValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Byte =>
        value match {
          case b: java.lang.Byte => operandStack.pushByte(b.byteValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Char =>
        value match {
          case c: java.lang.Character => operandStack.pushChar(c.charValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Double =>
        value match {
          case d: java.lang.Double => operandStack.pushDouble(d.doubleValue())
          case _ => throw mismatch(basicType, value)
        }
      case BasicTypes.Float =>
        value match {
          case f: java.lang.Float => operandStack.pushFloat(f.floatValue())
          case _ => throw mismatch(basicType, value)
        }
      case _ =>
        value match {
          case null => operandStack.pushObject(null)
          case r: AnyRef => operandStack.pushObject(r)
          case _ => throw mismatch(basicType, value)
        }
    }
  }

  def pushReturnFromType(t: Type[TypeId], value: Any, operandStack: OperandStack): Unit = {
    pushReturn(BasicTypes.fromType(t), value, operandStack)
  }

  def pushReturnFromDefinition(sig: NativeFunctionDefinition, value: Any, operandStack: OperandStack): Unit = {
    pushReturnFromType(sig.returnType, value, operandStack)
  }

  def pushReturnFromNativeFunction(nf: NativeFunction, value: Any, operandStack: OperandStack): Unit = {
    pushReturn(nf.returnType.toByte, value, operandStack)
  }

  private def mismatch(expected: BasicType, actual: Any): IllegalArgumentException = {
    val actualType = if (actual == null) "null" else actual.getClass.getName
    new IllegalArgumentException(s"Type mismatch: expected ${BasicTypes.friendlyName(expected)}, but got $actualType")
  }
}
