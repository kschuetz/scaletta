package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.FunctionImpl.{doubleResult, floatResult, intResult, longResult}
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes._

object ArithmeticOps {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.withPureHint(value = true) {
      ScalettaModule.composite(
        ScalettaModule.methodsOnly(add.register),
        ScalettaModule.methodsOnly(subtract.register),
        ScalettaModule.methodsOnly(multiply.register),
        ScalettaModule.methodsOnly(divide.register),
        ScalettaModule.methodsOnly(modulo.register),
      )
    }

  trait BinaryOp {
    def name: Name

    def byteByte(args: ArgumentReader): Int

    def byteChar(args: ArgumentReader): Int

    def byteDouble(args: ArgumentReader): Double

    def byteFloat(args: ArgumentReader): Float

    def byteInt(args: ArgumentReader): Int

    def byteLong(args: ArgumentReader): Long

    def byteShort(args: ArgumentReader): Int

    def charChar(args: ArgumentReader): Int

    def charDouble(args: ArgumentReader): Double

    def charFloat(args: ArgumentReader): Float

    def charInt(args: ArgumentReader): Int

    def charLong(args: ArgumentReader): Long

    def charShort(args: ArgumentReader): Int

    def doubleDouble(args: ArgumentReader): Double

    def floatDouble(args: ArgumentReader): Double

    def floatFloat(args: ArgumentReader): Float

    def intDouble(args: ArgumentReader): Double

    def intFloat(args: ArgumentReader): Float

    def intInt(args: ArgumentReader): Int

    def intLong(args: ArgumentReader): Long

    def longDouble(args: ArgumentReader): Double

    def longFloat(args: ArgumentReader): Float

    def longLong(args: ArgumentReader): Long

    def shortChar(args: ArgumentReader): Int

    def shortDouble(args: ArgumentReader): Double

    def shortFloat(args: ArgumentReader): Float

    def shortInt(args: ArgumentReader): Int

    def shortLong(args: ArgumentReader): Long

    def shortShort(args: ArgumentReader): Int

    def register(registry: MethodRegistry): Unit = {
      var overloads = registry.overloadRegistryFor(MethodName(IntT, name))
      overloads.addOverload(rhsInt, IntT, intResult(intInt))
      overloads.addOverload(rhsLong, LongT, longResult(intLong))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(intDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(intFloat))

      overloads = registry.overloadRegistryFor(MethodName(LongT, name))
      overloads.addOverload(rhsLong, LongT, longResult(longLong))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(longDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(longFloat))

      overloads = registry.overloadRegistryFor(MethodName(ShortT, name))
      overloads.addOverload(rhsInt, IntT, longResult(shortInt))
      overloads.addOverload(rhsLong, LongT, longResult(shortLong))
      overloads.addOverload(rhsShort, IntT, intResult(shortShort))
      overloads.addOverload(rhsChar, IntT, intResult(shortChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(shortDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(shortFloat))

      overloads = registry.overloadRegistryFor(MethodName(ByteT, name))
      overloads.addOverload(rhsInt, IntT, longResult(byteInt))
      overloads.addOverload(rhsLong, LongT, longResult(byteLong))
      overloads.addOverload(rhsShort, IntT, intResult(byteShort))
      overloads.addOverload(rhsByte, IntT, intResult(byteByte))
      overloads.addOverload(rhsChar, IntT, intResult(byteChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(byteDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(byteFloat))

      overloads = registry.overloadRegistryFor(MethodName(ByteT, name))
      overloads.addOverload(rhsInt, IntT, intResult(charInt))
      overloads.addOverload(rhsLong, LongT, longResult(charLong))
      overloads.addOverload(rhsShort, IntT, intResult(charShort))
      overloads.addOverload(rhsChar, IntT, intResult(charChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(charDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(charFloat))

      overloads = registry.overloadRegistryFor(MethodName(DoubleT, name))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(doubleDouble))

      overloads = registry.overloadRegistryFor(MethodName(FloatT, name))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(floatDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(floatFloat))
    }
  }

  object add extends BinaryOp {
    val name: Name = Name("+")

    def byteByte(args: ArgumentReader): Int = args.readByte(0) + args.readByte(1)

    def byteChar(args: ArgumentReader): Int = args.readByte(0) + args.readChar(1)

    def byteDouble(args: ArgumentReader): Double = args.readByte(0) + args.readDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.readByte(0) + args.readFloat(1)

    def byteInt(args: ArgumentReader): Int = args.readByte(0) + args.readInt(1)

    def byteLong(args: ArgumentReader): Long = args.readByte(0) + args.readLong(1)

    def byteShort(args: ArgumentReader): Int = args.readByte(0) + args.readShort(1)

    def charChar(args: ArgumentReader): Int = args.readChar(0) + args.readChar(1)

    def charDouble(args: ArgumentReader): Double = args.readChar(0) + args.readDouble(1)

    def charFloat(args: ArgumentReader): Float = args.readChar(0) + args.readFloat(1)

    def charInt(args: ArgumentReader): Int = args.readChar(0) + args.readInt(1)

    def charLong(args: ArgumentReader): Long = args.readChar(0) + args.readLong(1)

    def charShort(args: ArgumentReader): Int = args.readChar(0) + args.readShort(1)

    def doubleDouble(args: ArgumentReader): Double = args.readDouble(0) + args.readDouble(1)

    def floatDouble(args: ArgumentReader): Double = args.readFloat(0) + args.readDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.readFloat(0) + args.readFloat(1)

    def intDouble(args: ArgumentReader): Double = args.readInt(0) + args.readDouble(1)

    def intFloat(args: ArgumentReader): Float = args.readInt(0) + args.readFloat(1)

    def intInt(args: ArgumentReader): Int = args.readInt(0) + args.readInt(1)

    def intLong(args: ArgumentReader): Long = args.readInt(0) + args.readLong(1)

    def longDouble(args: ArgumentReader): Double = args.readLong(0) + args.readDouble(1)

    def longFloat(args: ArgumentReader): Float = args.readLong(0) + args.readFloat(1)

    def longLong(args: ArgumentReader): Long = args.readLong(0) + args.readLong(1)

    def shortChar(args: ArgumentReader): Int = args.readShort(0) + args.readChar(1)

    def shortDouble(args: ArgumentReader): Double = args.readShort(0) + args.readDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.readShort(0) + args.readFloat(1)

    def shortInt(args: ArgumentReader): Int = args.readShort(0) + args.readInt(1)

    def shortLong(args: ArgumentReader): Long = args.readShort(0) + args.readLong(1)

    def shortShort(args: ArgumentReader): Int = args.readShort(0) + args.readShort(1)
  }

  object subtract extends BinaryOp {
    val name: Name = Name("-")

    def byteByte(args: ArgumentReader): Int = args.readByte(0) - args.readByte(1)

    def byteChar(args: ArgumentReader): Int = args.readByte(0) - args.readChar(1)

    def byteDouble(args: ArgumentReader): Double = args.readByte(0) - args.readDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.readByte(0) - args.readFloat(1)

    def byteInt(args: ArgumentReader): Int = args.readByte(0) - args.readInt(1)

    def byteLong(args: ArgumentReader): Long = args.readByte(0) - args.readLong(1)

    def byteShort(args: ArgumentReader): Int = args.readByte(0) - args.readShort(1)

    def charChar(args: ArgumentReader): Int = args.readChar(0) - args.readChar(1)

    def charDouble(args: ArgumentReader): Double = args.readChar(0) - args.readDouble(1)

    def charFloat(args: ArgumentReader): Float = args.readChar(0) - args.readFloat(1)

    def charInt(args: ArgumentReader): Int = args.readChar(0) - args.readInt(1)

    def charLong(args: ArgumentReader): Long = args.readChar(0) - args.readLong(1)

    def charShort(args: ArgumentReader): Int = args.readChar(0) - args.readShort(1)

    def doubleDouble(args: ArgumentReader): Double = args.readDouble(0) - args.readDouble(1)

    def floatDouble(args: ArgumentReader): Double = args.readFloat(0) - args.readDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.readFloat(0) - args.readFloat(1)

    def intDouble(args: ArgumentReader): Double = args.readInt(0) - args.readDouble(1)

    def intFloat(args: ArgumentReader): Float = args.readInt(0) - args.readFloat(1)

    def intInt(args: ArgumentReader): Int = args.readInt(0) - args.readInt(1)

    def intLong(args: ArgumentReader): Long = args.readInt(0) - args.readLong(1)

    def longDouble(args: ArgumentReader): Double = args.readLong(0) - args.readDouble(1)

    def longFloat(args: ArgumentReader): Float = args.readLong(0) - args.readFloat(1)

    def longLong(args: ArgumentReader): Long = args.readLong(0) - args.readLong(1)

    def shortChar(args: ArgumentReader): Int = args.readShort(0) - args.readChar(1)

    def shortDouble(args: ArgumentReader): Double = args.readShort(0) - args.readDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.readShort(0) - args.readFloat(1)

    def shortInt(args: ArgumentReader): Int = args.readShort(0) - args.readInt(1)

    def shortLong(args: ArgumentReader): Long = args.readShort(0) - args.readLong(1)

    def shortShort(args: ArgumentReader): Int = args.readShort(0) - args.readShort(1)
  }

  object multiply extends BinaryOp {
    val name: Name = Name("*")

    def byteByte(args: ArgumentReader): Int = args.readByte(0) * args.readByte(1)

    def byteChar(args: ArgumentReader): Int = args.readByte(0) * args.readChar(1)

    def byteDouble(args: ArgumentReader): Double = args.readByte(0) * args.readDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.readByte(0) * args.readFloat(1)

    def byteInt(args: ArgumentReader): Int = args.readByte(0) * args.readInt(1)

    def byteLong(args: ArgumentReader): Long = args.readByte(0) * args.readLong(1)

    def byteShort(args: ArgumentReader): Int = args.readByte(0) * args.readShort(1)

    def charChar(args: ArgumentReader): Int = args.readChar(0) * args.readChar(1)

    def charDouble(args: ArgumentReader): Double = args.readChar(0) * args.readDouble(1)

    def charFloat(args: ArgumentReader): Float = args.readChar(0) * args.readFloat(1)

    def charInt(args: ArgumentReader): Int = args.readChar(0) * args.readInt(1)

    def charLong(args: ArgumentReader): Long = args.readChar(0) * args.readLong(1)

    def charShort(args: ArgumentReader): Int = args.readChar(0) * args.readShort(1)

    def doubleDouble(args: ArgumentReader): Double = args.readDouble(0) * args.readDouble(1)

    def floatDouble(args: ArgumentReader): Double = args.readFloat(0) * args.readDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.readFloat(0) * args.readFloat(1)

    def intDouble(args: ArgumentReader): Double = args.readInt(0) * args.readDouble(1)

    def intFloat(args: ArgumentReader): Float = args.readInt(0) * args.readFloat(1)

    def intInt(args: ArgumentReader): Int = args.readInt(0) * args.readInt(1)

    def intLong(args: ArgumentReader): Long = args.readInt(0) * args.readLong(1)

    def longDouble(args: ArgumentReader): Double = args.readLong(0) * args.readDouble(1)

    def longFloat(args: ArgumentReader): Float = args.readLong(0) * args.readFloat(1)

    def longLong(args: ArgumentReader): Long = args.readLong(0) * args.readLong(1)

    def shortChar(args: ArgumentReader): Int = args.readShort(0) * args.readChar(1)

    def shortDouble(args: ArgumentReader): Double = args.readShort(0) * args.readDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.readShort(0) * args.readFloat(1)

    def shortInt(args: ArgumentReader): Int = args.readShort(0) * args.readInt(1)

    def shortLong(args: ArgumentReader): Long = args.readShort(0) * args.readLong(1)

    def shortShort(args: ArgumentReader): Int = args.readShort(0) * args.readShort(1)
  }

  object divide extends BinaryOp {
    val name: Name = Name("/")

    def byteByte(args: ArgumentReader): Int = args.readByte(0) / args.readByte(1)

    def byteChar(args: ArgumentReader): Int = args.readByte(0) / args.readChar(1)

    def byteDouble(args: ArgumentReader): Double = args.readByte(0) / args.readDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.readByte(0) / args.readFloat(1)

    def byteInt(args: ArgumentReader): Int = args.readByte(0) / args.readInt(1)

    def byteLong(args: ArgumentReader): Long = args.readByte(0) / args.readLong(1)

    def byteShort(args: ArgumentReader): Int = args.readByte(0) / args.readShort(1)

    def charChar(args: ArgumentReader): Int = args.readChar(0) / args.readChar(1)

    def charDouble(args: ArgumentReader): Double = args.readChar(0) / args.readDouble(1)

    def charFloat(args: ArgumentReader): Float = args.readChar(0) / args.readFloat(1)

    def charInt(args: ArgumentReader): Int = args.readChar(0) / args.readInt(1)

    def charLong(args: ArgumentReader): Long = args.readChar(0) / args.readLong(1)

    def charShort(args: ArgumentReader): Int = args.readChar(0) / args.readShort(1)

    def doubleDouble(args: ArgumentReader): Double = args.readDouble(0) / args.readDouble(1)

    def floatDouble(args: ArgumentReader): Double = args.readFloat(0) / args.readDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.readFloat(0) / args.readFloat(1)

    def intDouble(args: ArgumentReader): Double = args.readInt(0) / args.readDouble(1)

    def intFloat(args: ArgumentReader): Float = args.readInt(0) / args.readFloat(1)

    def intInt(args: ArgumentReader): Int = args.readInt(0) / args.readInt(1)

    def intLong(args: ArgumentReader): Long = args.readInt(0) / args.readLong(1)

    def longDouble(args: ArgumentReader): Double = args.readLong(0) / args.readDouble(1)

    def longFloat(args: ArgumentReader): Float = args.readLong(0) / args.readFloat(1)

    def longLong(args: ArgumentReader): Long = args.readLong(0) / args.readLong(1)

    def shortChar(args: ArgumentReader): Int = args.readShort(0) / args.readChar(1)

    def shortDouble(args: ArgumentReader): Double = args.readShort(0) / args.readDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.readShort(0) / args.readFloat(1)

    def shortInt(args: ArgumentReader): Int = args.readShort(0) / args.readInt(1)

    def shortLong(args: ArgumentReader): Long = args.readShort(0) / args.readLong(1)

    def shortShort(args: ArgumentReader): Int = args.readShort(0) / args.readShort(1)
  }

  object modulo extends BinaryOp {
    val name: Name = Name("%")

    def byteByte(args: ArgumentReader): Int = args.readByte(0) % args.readByte(1)

    def byteChar(args: ArgumentReader): Int = args.readByte(0) % args.readChar(1)

    def byteDouble(args: ArgumentReader): Double = args.readByte(0) % args.readDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.readByte(0) % args.readFloat(1)

    def byteInt(args: ArgumentReader): Int = args.readByte(0) % args.readInt(1)

    def byteLong(args: ArgumentReader): Long = args.readByte(0) % args.readLong(1)

    def byteShort(args: ArgumentReader): Int = args.readByte(0) % args.readShort(1)

    def charChar(args: ArgumentReader): Int = args.readChar(0) % args.readChar(1)

    def charDouble(args: ArgumentReader): Double = args.readChar(0) % args.readDouble(1)

    def charFloat(args: ArgumentReader): Float = args.readChar(0) % args.readFloat(1)

    def charInt(args: ArgumentReader): Int = args.readChar(0) % args.readInt(1)

    def charLong(args: ArgumentReader): Long = args.readChar(0) % args.readLong(1)

    def charShort(args: ArgumentReader): Int = args.readChar(0) % args.readShort(1)

    def doubleDouble(args: ArgumentReader): Double = args.readDouble(0) % args.readDouble(1)

    def floatDouble(args: ArgumentReader): Double = args.readFloat(0) % args.readDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.readFloat(0) % args.readFloat(1)

    def intDouble(args: ArgumentReader): Double = args.readInt(0) % args.readDouble(1)

    def intFloat(args: ArgumentReader): Float = args.readInt(0) % args.readFloat(1)

    def intInt(args: ArgumentReader): Int = args.readInt(0) % args.readInt(1)

    def intLong(args: ArgumentReader): Long = args.readInt(0) % args.readLong(1)

    def longDouble(args: ArgumentReader): Double = args.readLong(0) % args.readDouble(1)

    def longFloat(args: ArgumentReader): Float = args.readLong(0) % args.readFloat(1)

    def longLong(args: ArgumentReader): Long = args.readLong(0) % args.readLong(1)

    def shortChar(args: ArgumentReader): Int = args.readShort(0) % args.readChar(1)

    def shortDouble(args: ArgumentReader): Double = args.readShort(0) % args.readDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.readShort(0) % args.readFloat(1)

    def shortInt(args: ArgumentReader): Int = args.readShort(0) % args.readInt(1)

    def shortLong(args: ArgumentReader): Long = args.readShort(0) % args.readLong(1)

    def shortShort(args: ArgumentReader): Int = args.readShort(0) % args.readShort(1)
  }

  private val rhsName = Name("x")
  private val rhsInt = Vector(FormalParameter.int(rhsName))
  private val rhsLong = Vector(FormalParameter.long(rhsName))
  private val rhsShort = Vector(FormalParameter.short(rhsName))
  private val rhsByte = Vector(FormalParameter.byte(rhsName))
  private val rhsChar = Vector(FormalParameter.char(rhsName))
  private val rhsDouble = Vector(FormalParameter.double(rhsName))
  private val rhsFloat = Vector(FormalParameter.float(rhsName))
}
