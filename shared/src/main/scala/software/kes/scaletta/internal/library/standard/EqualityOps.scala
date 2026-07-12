package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.FunctionImpl.booleanResult
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.runtime.CoreTypes._

object EqualityOps {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.withPureHint(value = true) {
      ScalettaModule.composite(
        ScalettaModule.methodsOnly(eq.register),
        ScalettaModule.methodsOnly(neq.register),
        ScalettaModule.methodsOnly(refEq.register),
      )
    }

  object refEq {
    val name: Name = Name("eq")

    def register(registry: MethodRegistry): Unit = {
      registry.addMethod(MethodName(ReceiverType.instance(CoreTypes.AnyRefT), name),
        Vector(FormalParameter(Name("x"), CoreTypes.AnyRefT)), CoreTypes.BooleanT,
        booleanResult(impl))
    }

    private def impl(args: ArgumentReader): Boolean =
      args.readObject(0) eq args.readObject(1)
  }

  trait EqualityOp {
    def name: Name

    def anyAny(args: ArgumentReader): Boolean

    def anyBoolean(args: ArgumentReader): Boolean

    def anyInt(args: ArgumentReader): Boolean

    def anyLong(args: ArgumentReader): Boolean

    def anyShort(args: ArgumentReader): Boolean

    def anyByte(args: ArgumentReader): Boolean

    def anyChar(args: ArgumentReader): Boolean

    def anyDouble(args: ArgumentReader): Boolean

    def anyFloat(args: ArgumentReader): Boolean

    def booleanAny(args: ArgumentReader): Boolean

    def booleanBoolean(args: ArgumentReader): Boolean

    def intAny(args: ArgumentReader): Boolean

    def intInt(args: ArgumentReader): Boolean

    def intLong(args: ArgumentReader): Boolean

    def intShort(args: ArgumentReader): Boolean

    def intByte(args: ArgumentReader): Boolean

    def intChar(args: ArgumentReader): Boolean

    def intDouble(args: ArgumentReader): Boolean

    def intFloat(args: ArgumentReader): Boolean

    def longAny(args: ArgumentReader): Boolean

    def longInt(args: ArgumentReader): Boolean

    def longLong(args: ArgumentReader): Boolean

    def longShort(args: ArgumentReader): Boolean

    def longByte(args: ArgumentReader): Boolean

    def longChar(args: ArgumentReader): Boolean

    def longDouble(args: ArgumentReader): Boolean

    def longFloat(args: ArgumentReader): Boolean

    def shortAny(args: ArgumentReader): Boolean

    def shortInt(args: ArgumentReader): Boolean

    def shortLong(args: ArgumentReader): Boolean

    def shortShort(args: ArgumentReader): Boolean

    def shortByte(args: ArgumentReader): Boolean

    def shortChar(args: ArgumentReader): Boolean

    def shortDouble(args: ArgumentReader): Boolean

    def shortFloat(args: ArgumentReader): Boolean

    def byteAny(args: ArgumentReader): Boolean

    def byteInt(args: ArgumentReader): Boolean

    def byteLong(args: ArgumentReader): Boolean

    def byteShort(args: ArgumentReader): Boolean

    def byteByte(args: ArgumentReader): Boolean

    def byteChar(args: ArgumentReader): Boolean

    def byteDouble(args: ArgumentReader): Boolean

    def byteFloat(args: ArgumentReader): Boolean

    def charAny(args: ArgumentReader): Boolean

    def charInt(args: ArgumentReader): Boolean

    def charLong(args: ArgumentReader): Boolean

    def charShort(args: ArgumentReader): Boolean

    def charByte(args: ArgumentReader): Boolean

    def charChar(args: ArgumentReader): Boolean

    def charDouble(args: ArgumentReader): Boolean

    def charFloat(args: ArgumentReader): Boolean

    def doubleAny(args: ArgumentReader): Boolean

    def doubleInt(args: ArgumentReader): Boolean

    def doubleLong(args: ArgumentReader): Boolean

    def doubleShort(args: ArgumentReader): Boolean

    def doubleByte(args: ArgumentReader): Boolean

    def doubleChar(args: ArgumentReader): Boolean

    def doubleDouble(args: ArgumentReader): Boolean

    def doubleFloat(args: ArgumentReader): Boolean

    def floatAny(args: ArgumentReader): Boolean

    def floatInt(args: ArgumentReader): Boolean

    def floatLong(args: ArgumentReader): Boolean

    def floatShort(args: ArgumentReader): Boolean

    def floatByte(args: ArgumentReader): Boolean

    def floatChar(args: ArgumentReader): Boolean

    def floatDouble(args: ArgumentReader): Boolean

    def floatFloat(args: ArgumentReader): Boolean

    def register(registry: MethodRegistry): Unit = {
      var overloads = registry.overloadRegistryFor(MethodName(AnyT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(anyAny))
      overloads.addOverload(rhsBoolean, BooleanT, booleanResult(anyBoolean))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(anyInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(anyLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(anyShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(anyByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(anyChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(anyDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(anyFloat))

      overloads = registry.overloadRegistryFor(MethodName(BooleanT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(booleanAny))
      overloads.addOverload(rhsBoolean, BooleanT, booleanResult(booleanBoolean))

      overloads = registry.overloadRegistryFor(MethodName(IntT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(intAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(intInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(intLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(intShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(intByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(intChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(intDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(intFloat))

      overloads = registry.overloadRegistryFor(MethodName(LongT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(longAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(longInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(longLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(longShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(longByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(longChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(longDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(longFloat))

      overloads = registry.overloadRegistryFor(MethodName(ShortT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(shortAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(shortInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(shortLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(shortShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(shortByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(shortChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(shortDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(shortFloat))

      overloads = registry.overloadRegistryFor(MethodName(ByteT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(byteAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(byteInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(byteLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(byteShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(byteByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(byteChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(byteDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(byteFloat))

      overloads = registry.overloadRegistryFor(MethodName(CharT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(charAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(charInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(charLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(charShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(charByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(charChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(charDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(charFloat))

      overloads = registry.overloadRegistryFor(MethodName(DoubleT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(doubleAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(doubleInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(doubleLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(doubleShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(doubleByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(doubleChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(doubleDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(doubleFloat))

      overloads = registry.overloadRegistryFor(MethodName(FloatT, name))
      overloads.addOverload(rhsAny, BooleanT, booleanResult(floatAny))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(floatInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(floatLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(floatShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(floatByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(floatChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(floatDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(floatFloat))
    }
  }

  object eq extends EqualityOp {
    val name: Name = Name("==")

    def anyAny(args: ArgumentReader): Boolean =
      args.read(0) == args.read(1)

    def anyBoolean(args: ArgumentReader): Boolean =
      args.read(0) == args.readBoolean(1)

    def anyInt(args: ArgumentReader): Boolean =
      args.read(0) == args.readInt(1)

    def anyLong(args: ArgumentReader): Boolean =
      args.read(0) == args.readLong(1)

    def anyShort(args: ArgumentReader): Boolean =
      args.read(0) == args.readShort(1)

    def anyByte(args: ArgumentReader): Boolean =
      args.read(0) == args.readByte(1)

    def anyChar(args: ArgumentReader): Boolean =
      args.read(0) == args.readChar(1)

    def anyDouble(args: ArgumentReader): Boolean =
      args.read(0) == args.readDouble(1)

    def anyFloat(args: ArgumentReader): Boolean =
      args.read(0) == args.readFloat(1)

    def booleanAny(args: ArgumentReader): Boolean =
      args.readBoolean(0) == args.read(1)

    def booleanBoolean(args: ArgumentReader): Boolean =
      args.readBoolean(0) == args.readBoolean(1)

    def intAny(args: ArgumentReader): Boolean =
      args.readInt(0) == args.read(1)

    def intInt(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readInt(1)

    def intLong(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readLong(1)

    def intShort(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readShort(1)

    def intByte(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readByte(1)

    def intChar(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readChar(1)

    def intDouble(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readDouble(1)

    def intFloat(args: ArgumentReader): Boolean =
      args.readInt(0) == args.readFloat(1)

    def longAny(args: ArgumentReader): Boolean =
      args.readLong(0) == args.read(1)

    def longInt(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readInt(1)

    def longLong(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readLong(1)

    def longShort(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readShort(1)

    def longByte(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readByte(1)

    def longChar(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readChar(1)

    def longDouble(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readDouble(1)

    def longFloat(args: ArgumentReader): Boolean =
      args.readLong(0) == args.readFloat(1)

    def shortAny(args: ArgumentReader): Boolean =
      args.readShort(0) == args.read(1)

    def shortInt(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readInt(1)

    def shortLong(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readLong(1)

    def shortShort(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readShort(1)

    def shortByte(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readByte(1)

    def shortChar(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readChar(1)

    def shortDouble(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readDouble(1)

    def shortFloat(args: ArgumentReader): Boolean =
      args.readShort(0) == args.readFloat(1)

    def byteAny(args: ArgumentReader): Boolean =
      args.readByte(0) == args.read(1)

    def byteInt(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readInt(1)

    def byteLong(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readLong(1)

    def byteShort(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readShort(1)

    def byteByte(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readByte(1)

    def byteChar(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readChar(1)

    def byteDouble(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readDouble(1)

    def byteFloat(args: ArgumentReader): Boolean =
      args.readByte(0) == args.readFloat(1)

    def charAny(args: ArgumentReader): Boolean =
      args.readChar(0) == args.read(1)

    def charInt(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readInt(1)

    def charLong(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readLong(1)

    def charShort(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readShort(1)

    def charByte(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readByte(1)

    def charChar(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readChar(1)

    def charDouble(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readDouble(1)

    def charFloat(args: ArgumentReader): Boolean =
      args.readChar(0) == args.readFloat(1)

    def doubleAny(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.read(1)

    def doubleInt(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readInt(1)

    def doubleLong(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readLong(1)

    def doubleShort(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readShort(1)

    def doubleByte(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readByte(1)

    def doubleChar(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readChar(1)

    def doubleDouble(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readDouble(1)

    def doubleFloat(args: ArgumentReader): Boolean =
      args.readDouble(0) == args.readFloat(1)

    def floatAny(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.read(1)

    def floatInt(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readInt(1)

    def floatLong(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readLong(1)

    def floatShort(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readShort(1)

    def floatByte(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readByte(1)

    def floatChar(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readChar(1)

    def floatDouble(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readDouble(1)

    def floatFloat(args: ArgumentReader): Boolean =
      args.readFloat(0) == args.readFloat(1)
  }

  object neq extends EqualityOp {
    val name: Name = Name("!=")

    def anyAny(args: ArgumentReader): Boolean =
      args.read(0) != args.read(1)

    def anyBoolean(args: ArgumentReader): Boolean =
      args.read(0) != args.readBoolean(1)

    def anyInt(args: ArgumentReader): Boolean =
      args.read(0) != args.readInt(1)

    def anyLong(args: ArgumentReader): Boolean =
      args.read(0) != args.readLong(1)

    def anyShort(args: ArgumentReader): Boolean =
      args.read(0) != args.readShort(1)

    def anyByte(args: ArgumentReader): Boolean =
      args.read(0) != args.readByte(1)

    def anyChar(args: ArgumentReader): Boolean =
      args.read(0) != args.readChar(1)

    def anyDouble(args: ArgumentReader): Boolean =
      args.read(0) != args.readDouble(1)

    def anyFloat(args: ArgumentReader): Boolean =
      args.read(0) != args.readFloat(1)

    def booleanAny(args: ArgumentReader): Boolean =
      args.readBoolean(0) != args.read(1)

    def booleanBoolean(args: ArgumentReader): Boolean =
      args.readBoolean(0) != args.readBoolean(1)

    def intAny(args: ArgumentReader): Boolean =
      args.readInt(0) != args.read(1)

    def intInt(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readInt(1)

    def intLong(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readLong(1)

    def intShort(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readShort(1)

    def intByte(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readByte(1)

    def intChar(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readChar(1)

    def intDouble(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readDouble(1)

    def intFloat(args: ArgumentReader): Boolean =
      args.readInt(0) != args.readFloat(1)

    def longAny(args: ArgumentReader): Boolean =
      args.readLong(0) != args.read(1)

    def longInt(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readInt(1)

    def longLong(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readLong(1)

    def longShort(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readShort(1)

    def longByte(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readByte(1)

    def longChar(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readChar(1)

    def longDouble(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readDouble(1)

    def longFloat(args: ArgumentReader): Boolean =
      args.readLong(0) != args.readFloat(1)

    def shortAny(args: ArgumentReader): Boolean =
      args.readShort(0) != args.read(1)

    def shortInt(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readInt(1)

    def shortLong(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readLong(1)

    def shortShort(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readShort(1)

    def shortByte(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readByte(1)

    def shortChar(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readChar(1)

    def shortDouble(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readDouble(1)

    def shortFloat(args: ArgumentReader): Boolean =
      args.readShort(0) != args.readFloat(1)

    def byteAny(args: ArgumentReader): Boolean =
      args.readByte(0) != args.read(1)

    def byteInt(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readInt(1)

    def byteLong(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readLong(1)

    def byteShort(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readShort(1)

    def byteByte(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readByte(1)

    def byteChar(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readChar(1)

    def byteDouble(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readDouble(1)

    def byteFloat(args: ArgumentReader): Boolean =
      args.readByte(0) != args.readFloat(1)

    def charAny(args: ArgumentReader): Boolean =
      args.readChar(0) != args.read(1)

    def charInt(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readInt(1)

    def charLong(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readLong(1)

    def charShort(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readShort(1)

    def charByte(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readByte(1)

    def charChar(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readChar(1)

    def charDouble(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readDouble(1)

    def charFloat(args: ArgumentReader): Boolean =
      args.readChar(0) != args.readFloat(1)

    def doubleAny(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.read(1)

    def doubleInt(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readInt(1)

    def doubleLong(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readLong(1)

    def doubleShort(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readShort(1)

    def doubleByte(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readByte(1)

    def doubleChar(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readChar(1)

    def doubleDouble(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readDouble(1)

    def doubleFloat(args: ArgumentReader): Boolean =
      args.readDouble(0) != args.readFloat(1)

    def floatAny(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.read(1)

    def floatInt(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readInt(1)

    def floatLong(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readLong(1)

    def floatShort(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readShort(1)

    def floatByte(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readByte(1)

    def floatChar(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readChar(1)

    def floatDouble(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readDouble(1)

    def floatFloat(args: ArgumentReader): Boolean =
      args.readFloat(0) != args.readFloat(1)
  }

  private val rhsName = Name("x")
  private val rhsAny = Vector(FormalParameter.any(rhsName))
  private val rhsBoolean =
    Vector(FormalParameter.boolean(rhsName))
  private val rhsInt = Vector(FormalParameter.int(rhsName))
  private val rhsLong = Vector(FormalParameter.long(rhsName))
  private val rhsShort = Vector(FormalParameter.short(rhsName))
  private val rhsByte = Vector(FormalParameter.byte(rhsName))
  private val rhsChar = Vector(FormalParameter.char(rhsName))
  private val rhsDouble = Vector(FormalParameter.double(rhsName))
  private val rhsFloat = Vector(FormalParameter.float(rhsName))
}
