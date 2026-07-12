package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.FunctionImpl.booleanResult
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes._

object ComparisonOps {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.withPureHint(value = true) {
      ScalettaModule.composite(
        ScalettaModule.methodsOnly(lt.register),
        ScalettaModule.methodsOnly(gt.register),
        ScalettaModule.methodsOnly(le.register),
        ScalettaModule.methodsOnly(ge.register),
      )
    }

  trait ComparisonOp {
    def name: Name

    def byteByte(args: ArgumentReader): Boolean

    def byteChar(args: ArgumentReader): Boolean

    def byteDouble(args: ArgumentReader): Boolean

    def byteFloat(args: ArgumentReader): Boolean

    def byteInt(args: ArgumentReader): Boolean

    def byteLong(args: ArgumentReader): Boolean

    def byteShort(args: ArgumentReader): Boolean

    def charChar(args: ArgumentReader): Boolean

    def charDouble(args: ArgumentReader): Boolean

    def charFloat(args: ArgumentReader): Boolean

    def charInt(args: ArgumentReader): Boolean

    def charLong(args: ArgumentReader): Boolean

    def doubleDouble(args: ArgumentReader): Boolean

    def floatDouble(args: ArgumentReader): Boolean

    def floatFloat(args: ArgumentReader): Boolean

    def floatLong(args: ArgumentReader): Boolean

    def intDouble(args: ArgumentReader): Boolean

    def intFloat(args: ArgumentReader): Boolean

    def intInt(args: ArgumentReader): Boolean

    def intLong(args: ArgumentReader): Boolean

    def longDouble(args: ArgumentReader): Boolean

    def longFloat(args: ArgumentReader): Boolean

    def longLong(args: ArgumentReader): Boolean

    def shortChar(args: ArgumentReader): Boolean

    def shortDouble(args: ArgumentReader): Boolean

    def shortFloat(args: ArgumentReader): Boolean

    def shortInt(args: ArgumentReader): Boolean

    def shortLong(args: ArgumentReader): Boolean

    def shortShort(args: ArgumentReader): Boolean

    def booleanBoolean(args: ArgumentReader): Boolean

    def stringString(args: ArgumentReader): Boolean

    def register(registry: MethodRegistry): Unit = {
      var overloads = registry.overloadRegistryFor(MethodName(IntT, name))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(intInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(intLong))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(intDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(intFloat))

      overloads = registry.overloadRegistryFor(MethodName(LongT, name))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(longLong))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(longDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(longFloat))

      overloads = registry.overloadRegistryFor(MethodName(ShortT, name))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(shortInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(shortLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(shortShort))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(shortChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(shortDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(shortFloat))

      overloads = registry.overloadRegistryFor(MethodName(ByteT, name))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(byteInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(byteLong))
      overloads.addOverload(rhsShort, BooleanT, booleanResult(byteShort))
      overloads.addOverload(rhsByte, BooleanT, booleanResult(byteByte))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(byteChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(byteDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(byteFloat))

      overloads = registry.overloadRegistryFor(MethodName(CharT, name))
      overloads.addOverload(rhsInt, BooleanT, booleanResult(charInt))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(charLong))
      overloads.addOverload(rhsChar, BooleanT, booleanResult(charChar))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(charDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(charFloat))

      overloads = registry.overloadRegistryFor(MethodName(DoubleT, name))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(doubleDouble))

      overloads = registry.overloadRegistryFor(MethodName(FloatT, name))
      overloads.addOverload(rhsLong, BooleanT, booleanResult(floatLong))
      overloads.addOverload(rhsDouble, BooleanT, booleanResult(floatDouble))
      overloads.addOverload(rhsFloat, BooleanT, booleanResult(floatFloat))

      overloads = registry.overloadRegistryFor(MethodName(BooleanT, name))
      overloads.addOverload(rhsBoolean, BooleanT, booleanResult(booleanBoolean))

      overloads = registry.overloadRegistryFor(MethodName(StringT, name))
      overloads.addOverload(rhsString, BooleanT, booleanResult(stringString))
    }
  }

  object lt extends ComparisonOp {
    val name: Name = Name("<")

    def byteByte(args: ArgumentReader): Boolean = args.readByte(0) < args.readByte(1)

    def byteChar(args: ArgumentReader): Boolean = args.readByte(0) < args.readChar(1)

    def byteDouble(args: ArgumentReader): Boolean = args.readByte(0) < args.readDouble(1)

    def byteFloat(args: ArgumentReader): Boolean = args.readByte(0) < args.readFloat(1)

    def byteInt(args: ArgumentReader): Boolean = args.readByte(0) < args.readInt(1)

    def byteLong(args: ArgumentReader): Boolean = args.readByte(0) < args.readLong(1)

    def byteShort(args: ArgumentReader): Boolean = args.readByte(0) < args.readShort(1)

    def charChar(args: ArgumentReader): Boolean = args.readChar(0) < args.readChar(1)

    def charDouble(args: ArgumentReader): Boolean = args.readChar(0) < args.readDouble(1)

    def charFloat(args: ArgumentReader): Boolean = args.readChar(0) < args.readFloat(1)

    def charInt(args: ArgumentReader): Boolean = args.readChar(0) < args.readInt(1)

    def charLong(args: ArgumentReader): Boolean = args.readChar(0) < args.readLong(1)

    def doubleDouble(args: ArgumentReader): Boolean = args.readDouble(0) < args.readDouble(1)

    def floatDouble(args: ArgumentReader): Boolean = args.readFloat(0) < args.readDouble(1)

    def floatFloat(args: ArgumentReader): Boolean = args.readFloat(0) < args.readFloat(1)

    def floatLong(args: ArgumentReader): Boolean = args.readFloat(0) < args.readLong(1)

    def intDouble(args: ArgumentReader): Boolean = args.readInt(0) < args.readDouble(1)

    def intFloat(args: ArgumentReader): Boolean = args.readInt(0) < args.readFloat(1)

    def intInt(args: ArgumentReader): Boolean = args.readInt(0) < args.readInt(1)

    def intLong(args: ArgumentReader): Boolean = args.readInt(0) < args.readLong(1)

    def longDouble(args: ArgumentReader): Boolean = args.readLong(0) < args.readDouble(1)

    def longFloat(args: ArgumentReader): Boolean = args.readLong(0) < args.readFloat(1)

    def longLong(args: ArgumentReader): Boolean = args.readLong(0) < args.readLong(1)

    def shortChar(args: ArgumentReader): Boolean = args.readShort(0) < args.readChar(1)

    def shortDouble(args: ArgumentReader): Boolean = args.readShort(0) < args.readDouble(1)

    def shortFloat(args: ArgumentReader): Boolean = args.readShort(0) < args.readFloat(1)

    def shortInt(args: ArgumentReader): Boolean = args.readShort(0) < args.readInt(1)

    def shortLong(args: ArgumentReader): Boolean = args.readShort(0) < args.readLong(1)

    def shortShort(args: ArgumentReader): Boolean = args.readShort(0) < args.readShort(1)

    def booleanBoolean(args: ArgumentReader): Boolean = args.readBoolean(0) < args.readBoolean(1)

    def stringString(args: ArgumentReader): Boolean = args.readObject(0).asInstanceOf[String] < args.readObject(1).asInstanceOf[String]
  }

  object gt extends ComparisonOp {
    val name: Name = Name(">")

    def byteByte(args: ArgumentReader): Boolean = args.readByte(0) > args.readByte(1)

    def byteChar(args: ArgumentReader): Boolean = args.readByte(0) > args.readChar(1)

    def byteDouble(args: ArgumentReader): Boolean = args.readByte(0) > args.readDouble(1)

    def byteFloat(args: ArgumentReader): Boolean = args.readByte(0) > args.readFloat(1)

    def byteInt(args: ArgumentReader): Boolean = args.readByte(0) > args.readInt(1)

    def byteLong(args: ArgumentReader): Boolean = args.readByte(0) > args.readLong(1)

    def byteShort(args: ArgumentReader): Boolean = args.readByte(0) > args.readShort(1)

    def charChar(args: ArgumentReader): Boolean = args.readChar(0) > args.readChar(1)

    def charDouble(args: ArgumentReader): Boolean = args.readChar(0) > args.readDouble(1)

    def charFloat(args: ArgumentReader): Boolean = args.readChar(0) > args.readFloat(1)

    def charInt(args: ArgumentReader): Boolean = args.readChar(0) > args.readInt(1)

    def charLong(args: ArgumentReader): Boolean = args.readChar(0) > args.readLong(1)

    def doubleDouble(args: ArgumentReader): Boolean = args.readDouble(0) > args.readDouble(1)

    def floatDouble(args: ArgumentReader): Boolean = args.readFloat(0) > args.readDouble(1)

    def floatFloat(args: ArgumentReader): Boolean = args.readFloat(0) > args.readFloat(1)

    def floatLong(args: ArgumentReader): Boolean = args.readFloat(0) > args.readLong(1)

    def intDouble(args: ArgumentReader): Boolean = args.readInt(0) > args.readDouble(1)

    def intFloat(args: ArgumentReader): Boolean = args.readInt(0) > args.readFloat(1)

    def intInt(args: ArgumentReader): Boolean = args.readInt(0) > args.readInt(1)

    def intLong(args: ArgumentReader): Boolean = args.readInt(0) > args.readLong(1)

    def longDouble(args: ArgumentReader): Boolean = args.readLong(0) > args.readDouble(1)

    def longFloat(args: ArgumentReader): Boolean = args.readLong(0) > args.readFloat(1)

    def longLong(args: ArgumentReader): Boolean = args.readLong(0) > args.readLong(1)

    def shortChar(args: ArgumentReader): Boolean = args.readShort(0) > args.readChar(1)

    def shortDouble(args: ArgumentReader): Boolean = args.readShort(0) > args.readDouble(1)

    def shortFloat(args: ArgumentReader): Boolean = args.readShort(0) > args.readFloat(1)

    def shortInt(args: ArgumentReader): Boolean = args.readShort(0) > args.readInt(1)

    def shortLong(args: ArgumentReader): Boolean = args.readShort(0) > args.readLong(1)

    def shortShort(args: ArgumentReader): Boolean = args.readShort(0) > args.readShort(1)

    def booleanBoolean(args: ArgumentReader): Boolean = args.readBoolean(0) > args.readBoolean(1)

    def stringString(args: ArgumentReader): Boolean = args.readObject(0).asInstanceOf[String] > args.readObject(1).asInstanceOf[String]
  }

  object le extends ComparisonOp {
    val name: Name = Name("<=")

    def byteByte(args: ArgumentReader): Boolean = args.readByte(0) <= args.readByte(1)

    def byteChar(args: ArgumentReader): Boolean = args.readByte(0) <= args.readChar(1)

    def byteDouble(args: ArgumentReader): Boolean = args.readByte(0) <= args.readDouble(1)

    def byteFloat(args: ArgumentReader): Boolean = args.readByte(0) <= args.readFloat(1)

    def byteInt(args: ArgumentReader): Boolean = args.readByte(0) <= args.readInt(1)

    def byteLong(args: ArgumentReader): Boolean = args.readByte(0) <= args.readLong(1)

    def byteShort(args: ArgumentReader): Boolean = args.readByte(0) <= args.readShort(1)

    def charChar(args: ArgumentReader): Boolean = args.readChar(0) <= args.readChar(1)

    def charDouble(args: ArgumentReader): Boolean = args.readChar(0) <= args.readDouble(1)

    def charFloat(args: ArgumentReader): Boolean = args.readChar(0) <= args.readFloat(1)

    def charInt(args: ArgumentReader): Boolean = args.readChar(0) <= args.readInt(1)

    def charLong(args: ArgumentReader): Boolean = args.readChar(0) <= args.readLong(1)

    def doubleDouble(args: ArgumentReader): Boolean = args.readDouble(0) <= args.readDouble(1)

    def floatDouble(args: ArgumentReader): Boolean = args.readFloat(0) <= args.readDouble(1)

    def floatFloat(args: ArgumentReader): Boolean = args.readFloat(0) <= args.readFloat(1)

    def floatLong(args: ArgumentReader): Boolean = args.readFloat(0) <= args.readLong(1)

    def intDouble(args: ArgumentReader): Boolean = args.readInt(0) <= args.readDouble(1)

    def intFloat(args: ArgumentReader): Boolean = args.readInt(0) <= args.readFloat(1)

    def intInt(args: ArgumentReader): Boolean = args.readInt(0) <= args.readInt(1)

    def intLong(args: ArgumentReader): Boolean = args.readInt(0) <= args.readLong(1)

    def longDouble(args: ArgumentReader): Boolean = args.readLong(0) <= args.readDouble(1)

    def longFloat(args: ArgumentReader): Boolean = args.readLong(0) <= args.readFloat(1)

    def longLong(args: ArgumentReader): Boolean = args.readLong(0) <= args.readLong(1)

    def shortChar(args: ArgumentReader): Boolean = args.readShort(0) <= args.readChar(1)

    def shortDouble(args: ArgumentReader): Boolean = args.readShort(0) <= args.readDouble(1)

    def shortFloat(args: ArgumentReader): Boolean = args.readShort(0) <= args.readFloat(1)

    def shortInt(args: ArgumentReader): Boolean = args.readShort(0) <= args.readInt(1)

    def shortLong(args: ArgumentReader): Boolean = args.readShort(0) <= args.readLong(1)

    def shortShort(args: ArgumentReader): Boolean = args.readShort(0) <= args.readShort(1)

    def booleanBoolean(args: ArgumentReader): Boolean = args.readBoolean(0) <= args.readBoolean(1)

    def stringString(args: ArgumentReader): Boolean = args.readObject(0).asInstanceOf[String] <= args.readObject(1).asInstanceOf[String]
  }

  object ge extends ComparisonOp {
    val name: Name = Name(">=")

    def byteByte(args: ArgumentReader): Boolean = args.readByte(0) >= args.readByte(1)

    def byteChar(args: ArgumentReader): Boolean = args.readByte(0) >= args.readChar(1)

    def byteDouble(args: ArgumentReader): Boolean = args.readByte(0) >= args.readDouble(1)

    def byteFloat(args: ArgumentReader): Boolean = args.readByte(0) >= args.readFloat(1)

    def byteInt(args: ArgumentReader): Boolean = args.readByte(0) >= args.readInt(1)

    def byteLong(args: ArgumentReader): Boolean = args.readByte(0) >= args.readLong(1)

    def byteShort(args: ArgumentReader): Boolean = args.readByte(0) >= args.readShort(1)

    def charChar(args: ArgumentReader): Boolean = args.readChar(0) >= args.readChar(1)

    def charDouble(args: ArgumentReader): Boolean = args.readChar(0) >= args.readDouble(1)

    def charFloat(args: ArgumentReader): Boolean = args.readChar(0) >= args.readFloat(1)

    def charInt(args: ArgumentReader): Boolean = args.readChar(0) >= args.readInt(1)

    def charLong(args: ArgumentReader): Boolean = args.readChar(0) >= args.readLong(1)

    def doubleDouble(args: ArgumentReader): Boolean = args.readDouble(0) >= args.readDouble(1)

    def floatDouble(args: ArgumentReader): Boolean = args.readFloat(0) >= args.readDouble(1)

    def floatFloat(args: ArgumentReader): Boolean = args.readFloat(0) >= args.readFloat(1)

    def floatLong(args: ArgumentReader): Boolean = args.readFloat(0) >= args.readLong(1)

    def intDouble(args: ArgumentReader): Boolean = args.readInt(0) >= args.readDouble(1)

    def intFloat(args: ArgumentReader): Boolean = args.readInt(0) >= args.readFloat(1)

    def intInt(args: ArgumentReader): Boolean = args.readInt(0) >= args.readInt(1)

    def intLong(args: ArgumentReader): Boolean = args.readInt(0) >= args.readLong(1)

    def longDouble(args: ArgumentReader): Boolean = args.readLong(0) >= args.readDouble(1)

    def longFloat(args: ArgumentReader): Boolean = args.readLong(0) >= args.readFloat(1)

    def longLong(args: ArgumentReader): Boolean = args.readLong(0) >= args.readLong(1)

    def shortChar(args: ArgumentReader): Boolean = args.readShort(0) >= args.readChar(1)

    def shortDouble(args: ArgumentReader): Boolean = args.readShort(0) >= args.readDouble(1)

    def shortFloat(args: ArgumentReader): Boolean = args.readShort(0) >= args.readFloat(1)

    def shortInt(args: ArgumentReader): Boolean = args.readShort(0) >= args.readInt(1)

    def shortLong(args: ArgumentReader): Boolean = args.readShort(0) >= args.readLong(1)

    def shortShort(args: ArgumentReader): Boolean = args.readShort(0) >= args.readShort(1)

    def booleanBoolean(args: ArgumentReader): Boolean = args.readBoolean(0) >= args.readBoolean(1)

    def stringString(args: ArgumentReader): Boolean = args.readObject(0).asInstanceOf[String] >= args.readObject(1).asInstanceOf[String]
  }

  private val rhsName = Name("x")
  private val rhsInt = Vector(FormalParameter.int(rhsName))
  private val rhsLong = Vector(FormalParameter.long(rhsName))
  private val rhsShort = Vector(FormalParameter.short(rhsName))
  private val rhsByte = Vector(FormalParameter.byte(rhsName))
  private val rhsChar = Vector(FormalParameter.char(rhsName))
  private val rhsDouble = Vector(FormalParameter.double(rhsName))
  private val rhsFloat = Vector(FormalParameter.float(rhsName))
  private val rhsBoolean = Vector(FormalParameter.boolean(rhsName))
  private val rhsString = Vector(FormalParameter.string(rhsName))

}
