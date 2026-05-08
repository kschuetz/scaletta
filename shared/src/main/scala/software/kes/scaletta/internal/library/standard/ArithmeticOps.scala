package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins.FormalParameter
import software.kes.scaletta.internal.builtins.FunctionImpl.{doubleResult, floatResult, intResult, longResult}
import software.kes.scaletta.internal.runtime.CoreTypes
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

    def charByte(args: ArgumentReader): Int

    def charChar(args: ArgumentReader): Int

    def charDouble(args: ArgumentReader): Double

    def charFloat(args: ArgumentReader): Float

    def charInt(args: ArgumentReader): Int

    def charLong(args: ArgumentReader): Long

    def charShort(args: ArgumentReader): Int

    def doubleByte(args: ArgumentReader): Double

    def doubleChar(args: ArgumentReader): Double

    def doubleDouble(args: ArgumentReader): Double

    def doubleFloat(args: ArgumentReader): Double

    def doubleInt(args: ArgumentReader): Double

    def doubleLong(args: ArgumentReader): Double

    def doubleShort(args: ArgumentReader): Double

    def floatByte(args: ArgumentReader): Float

    def floatChar(args: ArgumentReader): Float

    def floatDouble(args: ArgumentReader): Double

    def floatFloat(args: ArgumentReader): Float

    def floatInt(args: ArgumentReader): Float

    def floatLong(args: ArgumentReader): Float

    def floatShort(args: ArgumentReader): Float

    def intByte(args: ArgumentReader): Int

    def intChar(args: ArgumentReader): Int

    def intDouble(args: ArgumentReader): Double

    def intFloat(args: ArgumentReader): Float

    def intInt(args: ArgumentReader): Int

    def intLong(args: ArgumentReader): Long

    def intShort(args: ArgumentReader): Int

    def longByte(args: ArgumentReader): Long

    def longChar(args: ArgumentReader): Long

    def longDouble(args: ArgumentReader): Double

    def longFloat(args: ArgumentReader): Float

    def longInt(args: ArgumentReader): Long

    def longLong(args: ArgumentReader): Long

    def longShort(args: ArgumentReader): Long

    def shortByte(args: ArgumentReader): Int

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
      overloads.addOverload(rhsShort, IntT, intResult(intShort))
      overloads.addOverload(rhsByte, IntT, intResult(intByte))
      overloads.addOverload(rhsChar, IntT, intResult(intChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(intDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(intFloat))

      overloads = registry.overloadRegistryFor(MethodName(LongT, name))
      overloads.addOverload(rhsInt, LongT, longResult(longInt))
      overloads.addOverload(rhsLong, LongT, longResult(longLong))
      overloads.addOverload(rhsShort, LongT, longResult(longShort))
      overloads.addOverload(rhsByte, LongT, longResult(longByte))
      overloads.addOverload(rhsChar, LongT, longResult(longChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(longDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(longFloat))

      overloads = registry.overloadRegistryFor(MethodName(ShortT, name))
      overloads.addOverload(rhsInt, IntT, longResult(shortInt))
      overloads.addOverload(rhsLong, LongT, longResult(shortLong))
      overloads.addOverload(rhsShort, IntT, intResult(shortShort))
      overloads.addOverload(rhsByte, IntT, intResult(shortByte))
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
      overloads.addOverload(rhsByte, IntT, intResult(charByte))
      overloads.addOverload(rhsChar, IntT, intResult(charChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(charDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(charFloat))

      overloads = registry.overloadRegistryFor(MethodName(DoubleT, name))
      overloads.addOverload(rhsInt, DoubleT, doubleResult(doubleInt))
      overloads.addOverload(rhsLong, DoubleT, doubleResult(doubleLong))
      overloads.addOverload(rhsShort, DoubleT, doubleResult(doubleShort))
      overloads.addOverload(rhsByte, DoubleT, doubleResult(doubleByte))
      overloads.addOverload(rhsChar, DoubleT, doubleResult(doubleChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(doubleDouble))
      overloads.addOverload(rhsFloat, DoubleT, doubleResult(doubleFloat))

      overloads = registry.overloadRegistryFor(MethodName(FloatT, name))
      overloads.addOverload(rhsInt, FloatT, floatResult(floatInt))
      overloads.addOverload(rhsLong, FloatT, floatResult(floatLong))
      overloads.addOverload(rhsShort, FloatT, floatResult(floatShort))
      overloads.addOverload(rhsByte, FloatT, floatResult(floatByte))
      overloads.addOverload(rhsChar, FloatT, floatResult(floatChar))
      overloads.addOverload(rhsDouble, DoubleT, doubleResult(floatDouble))
      overloads.addOverload(rhsFloat, FloatT, floatResult(floatFloat))
    }
  }

  object add extends BinaryOp {
    val name: Name = Name("+")

    def byteByte(args: ArgumentReader): Int = args.unsafeReadByte(0) + args.unsafeReadByte(1)

    def byteChar(args: ArgumentReader): Int = args.unsafeReadByte(0) + args.unsafeReadChar(1)

    def byteDouble(args: ArgumentReader): Double = args.unsafeReadByte(0) + args.unsafeReadDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.unsafeReadByte(0) + args.unsafeReadFloat(1)

    def byteInt(args: ArgumentReader): Int = args.unsafeReadByte(0) + args.unsafeReadInt(1)

    def byteLong(args: ArgumentReader): Long = args.unsafeReadByte(0) + args.unsafeReadLong(1)

    def byteShort(args: ArgumentReader): Int = args.unsafeReadByte(0) + args.unsafeReadShort(1)

    def charByte(args: ArgumentReader): Int = args.unsafeReadChar(0) + args.unsafeReadByte(1)

    def charChar(args: ArgumentReader): Int = args.unsafeReadChar(0) + args.unsafeReadChar(1)

    def charDouble(args: ArgumentReader): Double = args.unsafeReadChar(0) + args.unsafeReadDouble(1)

    def charFloat(args: ArgumentReader): Float = args.unsafeReadChar(0) + args.unsafeReadFloat(1)

    def charInt(args: ArgumentReader): Int = args.unsafeReadChar(0) + args.unsafeReadInt(1)

    def charLong(args: ArgumentReader): Long = args.unsafeReadChar(0) + args.unsafeReadLong(1)

    def charShort(args: ArgumentReader): Int = args.unsafeReadChar(0) + args.unsafeReadShort(1)

    def doubleByte(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadByte(1)

    def doubleChar(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadChar(1)

    def doubleDouble(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadDouble(1)

    def doubleFloat(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadFloat(1)

    def doubleInt(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadInt(1)

    def doubleLong(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadLong(1)

    def doubleShort(args: ArgumentReader): Double = args.unsafeReadDouble(0) + args.unsafeReadShort(1)

    def floatByte(args: ArgumentReader): Float = args.unsafeReadFloat(0) + args.unsafeReadByte(1)

    def floatChar(args: ArgumentReader): Float = args.unsafeReadFloat(0) + args.unsafeReadChar(1)

    def floatDouble(args: ArgumentReader): Double = args.unsafeReadFloat(0) + args.unsafeReadDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.unsafeReadFloat(0) + args.unsafeReadFloat(1)

    def floatInt(args: ArgumentReader): Float = args.unsafeReadFloat(0) + args.unsafeReadInt(1)

    def floatLong(args: ArgumentReader): Float = args.unsafeReadFloat(0) + args.unsafeReadLong(1)

    def floatShort(args: ArgumentReader): Float = args.unsafeReadFloat(0) + args.unsafeReadShort(1)

    def intByte(args: ArgumentReader): Int = args.unsafeReadInt(0) + args.unsafeReadByte(1)

    def intChar(args: ArgumentReader): Int = args.unsafeReadInt(0) + args.unsafeReadChar(1)

    def intDouble(args: ArgumentReader): Double = args.unsafeReadInt(0) + args.unsafeReadDouble(1)

    def intFloat(args: ArgumentReader): Float = args.unsafeReadInt(0) + args.unsafeReadFloat(1)

    def intInt(args: ArgumentReader): Int = args.unsafeReadInt(0) + args.unsafeReadInt(1)

    def intLong(args: ArgumentReader): Long = args.unsafeReadInt(0) + args.unsafeReadLong(1)

    def intShort(args: ArgumentReader): Int = args.unsafeReadInt(0) + args.unsafeReadShort(1)

    def longByte(args: ArgumentReader): Long = args.unsafeReadLong(0) + args.unsafeReadByte(1)

    def longChar(args: ArgumentReader): Long = args.unsafeReadLong(0) + args.unsafeReadChar(1)

    def longDouble(args: ArgumentReader): Double = args.unsafeReadLong(0) + args.unsafeReadDouble(1)

    def longFloat(args: ArgumentReader): Float = args.unsafeReadLong(0) + args.unsafeReadFloat(1)

    def longInt(args: ArgumentReader): Long = args.unsafeReadLong(0) + args.unsafeReadInt(1)

    def longLong(args: ArgumentReader): Long = args.unsafeReadLong(0) + args.unsafeReadLong(1)

    def longShort(args: ArgumentReader): Long = args.unsafeReadLong(0) + args.unsafeReadShort(1)

    def shortByte(args: ArgumentReader): Int = args.unsafeReadShort(0) + args.unsafeReadByte(1)

    def shortChar(args: ArgumentReader): Int = args.unsafeReadShort(0) + args.unsafeReadChar(1)

    def shortDouble(args: ArgumentReader): Double = args.unsafeReadShort(0) + args.unsafeReadDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.unsafeReadShort(0) + args.unsafeReadFloat(1)

    def shortInt(args: ArgumentReader): Int = args.unsafeReadShort(0) + args.unsafeReadInt(1)

    def shortLong(args: ArgumentReader): Long = args.unsafeReadShort(0) + args.unsafeReadLong(1)

    def shortShort(args: ArgumentReader): Int = args.unsafeReadShort(0) + args.unsafeReadShort(1)
  }

  object subtract extends BinaryOp {
    val name: Name = Name("-")

    def byteByte(args: ArgumentReader): Int = args.unsafeReadByte(0) - args.unsafeReadByte(1)

    def byteChar(args: ArgumentReader): Int = args.unsafeReadByte(0) - args.unsafeReadChar(1)

    def byteDouble(args: ArgumentReader): Double = args.unsafeReadByte(0) - args.unsafeReadDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.unsafeReadByte(0) - args.unsafeReadFloat(1)

    def byteInt(args: ArgumentReader): Int = args.unsafeReadByte(0) - args.unsafeReadInt(1)

    def byteLong(args: ArgumentReader): Long = args.unsafeReadByte(0) - args.unsafeReadLong(1)

    def byteShort(args: ArgumentReader): Int = args.unsafeReadByte(0) - args.unsafeReadShort(1)

    def charByte(args: ArgumentReader): Int = args.unsafeReadChar(0) - args.unsafeReadByte(1)

    def charChar(args: ArgumentReader): Int = args.unsafeReadChar(0) - args.unsafeReadChar(1)

    def charDouble(args: ArgumentReader): Double = args.unsafeReadChar(0) - args.unsafeReadDouble(1)

    def charFloat(args: ArgumentReader): Float = args.unsafeReadChar(0) - args.unsafeReadFloat(1)

    def charInt(args: ArgumentReader): Int = args.unsafeReadChar(0) - args.unsafeReadInt(1)

    def charLong(args: ArgumentReader): Long = args.unsafeReadChar(0) - args.unsafeReadLong(1)

    def charShort(args: ArgumentReader): Int = args.unsafeReadChar(0) - args.unsafeReadShort(1)

    def doubleByte(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadByte(1)

    def doubleChar(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadChar(1)

    def doubleDouble(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadDouble(1)

    def doubleFloat(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadFloat(1)

    def doubleInt(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadInt(1)

    def doubleLong(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadLong(1)

    def doubleShort(args: ArgumentReader): Double = args.unsafeReadDouble(0) - args.unsafeReadShort(1)

    def floatByte(args: ArgumentReader): Float = args.unsafeReadFloat(0) - args.unsafeReadByte(1)

    def floatChar(args: ArgumentReader): Float = args.unsafeReadFloat(0) - args.unsafeReadChar(1)

    def floatDouble(args: ArgumentReader): Double = args.unsafeReadFloat(0) - args.unsafeReadDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.unsafeReadFloat(0) - args.unsafeReadFloat(1)

    def floatInt(args: ArgumentReader): Float = args.unsafeReadFloat(0) - args.unsafeReadInt(1)

    def floatLong(args: ArgumentReader): Float = args.unsafeReadFloat(0) - args.unsafeReadLong(1)

    def floatShort(args: ArgumentReader): Float = args.unsafeReadFloat(0) - args.unsafeReadShort(1)

    def intByte(args: ArgumentReader): Int = args.unsafeReadInt(0) - args.unsafeReadByte(1)

    def intChar(args: ArgumentReader): Int = args.unsafeReadInt(0) - args.unsafeReadChar(1)

    def intDouble(args: ArgumentReader): Double = args.unsafeReadInt(0) - args.unsafeReadDouble(1)

    def intFloat(args: ArgumentReader): Float = args.unsafeReadInt(0) - args.unsafeReadFloat(1)

    def intInt(args: ArgumentReader): Int = args.unsafeReadInt(0) - args.unsafeReadInt(1)

    def intLong(args: ArgumentReader): Long = args.unsafeReadInt(0) - args.unsafeReadLong(1)

    def intShort(args: ArgumentReader): Int = args.unsafeReadInt(0) - args.unsafeReadShort(1)

    def longByte(args: ArgumentReader): Long = args.unsafeReadLong(0) - args.unsafeReadByte(1)

    def longChar(args: ArgumentReader): Long = args.unsafeReadLong(0) - args.unsafeReadChar(1)

    def longDouble(args: ArgumentReader): Double = args.unsafeReadLong(0) - args.unsafeReadDouble(1)

    def longFloat(args: ArgumentReader): Float = args.unsafeReadLong(0) - args.unsafeReadFloat(1)

    def longInt(args: ArgumentReader): Long = args.unsafeReadLong(0) - args.unsafeReadInt(1)

    def longLong(args: ArgumentReader): Long = args.unsafeReadLong(0) - args.unsafeReadLong(1)

    def longShort(args: ArgumentReader): Long = args.unsafeReadLong(0) - args.unsafeReadShort(1)

    def shortByte(args: ArgumentReader): Int = args.unsafeReadShort(0) - args.unsafeReadByte(1)

    def shortChar(args: ArgumentReader): Int = args.unsafeReadShort(0) - args.unsafeReadChar(1)

    def shortDouble(args: ArgumentReader): Double = args.unsafeReadShort(0) - args.unsafeReadDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.unsafeReadShort(0) - args.unsafeReadFloat(1)

    def shortInt(args: ArgumentReader): Int = args.unsafeReadShort(0) - args.unsafeReadInt(1)

    def shortLong(args: ArgumentReader): Long = args.unsafeReadShort(0) - args.unsafeReadLong(1)

    def shortShort(args: ArgumentReader): Int = args.unsafeReadShort(0) - args.unsafeReadShort(1)
  }

  object multiply extends BinaryOp {
    val name: Name = Name("*")

    def byteByte(args: ArgumentReader): Int = args.unsafeReadByte(0) * args.unsafeReadByte(1)

    def byteChar(args: ArgumentReader): Int = args.unsafeReadByte(0) * args.unsafeReadChar(1)

    def byteDouble(args: ArgumentReader): Double = args.unsafeReadByte(0) * args.unsafeReadDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.unsafeReadByte(0) * args.unsafeReadFloat(1)

    def byteInt(args: ArgumentReader): Int = args.unsafeReadByte(0) * args.unsafeReadInt(1)

    def byteLong(args: ArgumentReader): Long = args.unsafeReadByte(0) * args.unsafeReadLong(1)

    def byteShort(args: ArgumentReader): Int = args.unsafeReadByte(0) * args.unsafeReadShort(1)

    def charByte(args: ArgumentReader): Int = args.unsafeReadChar(0) * args.unsafeReadByte(1)

    def charChar(args: ArgumentReader): Int = args.unsafeReadChar(0) * args.unsafeReadChar(1)

    def charDouble(args: ArgumentReader): Double = args.unsafeReadChar(0) * args.unsafeReadDouble(1)

    def charFloat(args: ArgumentReader): Float = args.unsafeReadChar(0) * args.unsafeReadFloat(1)

    def charInt(args: ArgumentReader): Int = args.unsafeReadChar(0) * args.unsafeReadInt(1)

    def charLong(args: ArgumentReader): Long = args.unsafeReadChar(0) * args.unsafeReadLong(1)

    def charShort(args: ArgumentReader): Int = args.unsafeReadChar(0) * args.unsafeReadShort(1)

    def doubleByte(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadByte(1)

    def doubleChar(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadChar(1)

    def doubleDouble(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadDouble(1)

    def doubleFloat(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadFloat(1)

    def doubleInt(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadInt(1)

    def doubleLong(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadLong(1)

    def doubleShort(args: ArgumentReader): Double = args.unsafeReadDouble(0) * args.unsafeReadShort(1)

    def floatByte(args: ArgumentReader): Float = args.unsafeReadFloat(0) * args.unsafeReadByte(1)

    def floatChar(args: ArgumentReader): Float = args.unsafeReadFloat(0) * args.unsafeReadChar(1)

    def floatDouble(args: ArgumentReader): Double = args.unsafeReadFloat(0) * args.unsafeReadDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.unsafeReadFloat(0) * args.unsafeReadFloat(1)

    def floatInt(args: ArgumentReader): Float = args.unsafeReadFloat(0) * args.unsafeReadInt(1)

    def floatLong(args: ArgumentReader): Float = args.unsafeReadFloat(0) * args.unsafeReadLong(1)

    def floatShort(args: ArgumentReader): Float = args.unsafeReadFloat(0) * args.unsafeReadShort(1)

    def intByte(args: ArgumentReader): Int = args.unsafeReadInt(0) * args.unsafeReadByte(1)

    def intChar(args: ArgumentReader): Int = args.unsafeReadInt(0) * args.unsafeReadChar(1)

    def intDouble(args: ArgumentReader): Double = args.unsafeReadInt(0) * args.unsafeReadDouble(1)

    def intFloat(args: ArgumentReader): Float = args.unsafeReadInt(0) * args.unsafeReadFloat(1)

    def intInt(args: ArgumentReader): Int = args.unsafeReadInt(0) * args.unsafeReadInt(1)

    def intLong(args: ArgumentReader): Long = args.unsafeReadInt(0) * args.unsafeReadLong(1)

    def intShort(args: ArgumentReader): Int = args.unsafeReadInt(0) * args.unsafeReadShort(1)

    def longByte(args: ArgumentReader): Long = args.unsafeReadLong(0) * args.unsafeReadByte(1)

    def longChar(args: ArgumentReader): Long = args.unsafeReadLong(0) * args.unsafeReadChar(1)

    def longDouble(args: ArgumentReader): Double = args.unsafeReadLong(0) * args.unsafeReadDouble(1)

    def longFloat(args: ArgumentReader): Float = args.unsafeReadLong(0) * args.unsafeReadFloat(1)

    def longInt(args: ArgumentReader): Long = args.unsafeReadLong(0) * args.unsafeReadInt(1)

    def longLong(args: ArgumentReader): Long = args.unsafeReadLong(0) * args.unsafeReadLong(1)

    def longShort(args: ArgumentReader): Long = args.unsafeReadLong(0) * args.unsafeReadShort(1)

    def shortByte(args: ArgumentReader): Int = args.unsafeReadShort(0) * args.unsafeReadByte(1)

    def shortChar(args: ArgumentReader): Int = args.unsafeReadShort(0) * args.unsafeReadChar(1)

    def shortDouble(args: ArgumentReader): Double = args.unsafeReadShort(0) * args.unsafeReadDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.unsafeReadShort(0) * args.unsafeReadFloat(1)

    def shortInt(args: ArgumentReader): Int = args.unsafeReadShort(0) * args.unsafeReadInt(1)

    def shortLong(args: ArgumentReader): Long = args.unsafeReadShort(0) * args.unsafeReadLong(1)

    def shortShort(args: ArgumentReader): Int = args.unsafeReadShort(0) * args.unsafeReadShort(1)
  }

  object divide extends BinaryOp {
    val name: Name = Name("/")

    def byteByte(args: ArgumentReader): Int = args.unsafeReadByte(0) / args.unsafeReadByte(1)

    def byteChar(args: ArgumentReader): Int = args.unsafeReadByte(0) / args.unsafeReadChar(1)

    def byteDouble(args: ArgumentReader): Double = args.unsafeReadByte(0) / args.unsafeReadDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.unsafeReadByte(0) / args.unsafeReadFloat(1)

    def byteInt(args: ArgumentReader): Int = args.unsafeReadByte(0) / args.unsafeReadInt(1)

    def byteLong(args: ArgumentReader): Long = args.unsafeReadByte(0) / args.unsafeReadLong(1)

    def byteShort(args: ArgumentReader): Int = args.unsafeReadByte(0) / args.unsafeReadShort(1)

    def charByte(args: ArgumentReader): Int = args.unsafeReadChar(0) / args.unsafeReadByte(1)

    def charChar(args: ArgumentReader): Int = args.unsafeReadChar(0) / args.unsafeReadChar(1)

    def charDouble(args: ArgumentReader): Double = args.unsafeReadChar(0) / args.unsafeReadDouble(1)

    def charFloat(args: ArgumentReader): Float = args.unsafeReadChar(0) / args.unsafeReadFloat(1)

    def charInt(args: ArgumentReader): Int = args.unsafeReadChar(0) / args.unsafeReadInt(1)

    def charLong(args: ArgumentReader): Long = args.unsafeReadChar(0) / args.unsafeReadLong(1)

    def charShort(args: ArgumentReader): Int = args.unsafeReadChar(0) / args.unsafeReadShort(1)

    def doubleByte(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadByte(1)

    def doubleChar(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadChar(1)

    def doubleDouble(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadDouble(1)

    def doubleFloat(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadFloat(1)

    def doubleInt(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadInt(1)

    def doubleLong(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadLong(1)

    def doubleShort(args: ArgumentReader): Double = args.unsafeReadDouble(0) / args.unsafeReadShort(1)

    def floatByte(args: ArgumentReader): Float = args.unsafeReadFloat(0) / args.unsafeReadByte(1)

    def floatChar(args: ArgumentReader): Float = args.unsafeReadFloat(0) / args.unsafeReadChar(1)

    def floatDouble(args: ArgumentReader): Double = args.unsafeReadFloat(0) / args.unsafeReadDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.unsafeReadFloat(0) / args.unsafeReadFloat(1)

    def floatInt(args: ArgumentReader): Float = args.unsafeReadFloat(0) / args.unsafeReadInt(1)

    def floatLong(args: ArgumentReader): Float = args.unsafeReadFloat(0) / args.unsafeReadLong(1)

    def floatShort(args: ArgumentReader): Float = args.unsafeReadFloat(0) / args.unsafeReadShort(1)

    def intByte(args: ArgumentReader): Int = args.unsafeReadInt(0) / args.unsafeReadByte(1)

    def intChar(args: ArgumentReader): Int = args.unsafeReadInt(0) / args.unsafeReadChar(1)

    def intDouble(args: ArgumentReader): Double = args.unsafeReadInt(0) / args.unsafeReadDouble(1)

    def intFloat(args: ArgumentReader): Float = args.unsafeReadInt(0) / args.unsafeReadFloat(1)

    def intInt(args: ArgumentReader): Int = args.unsafeReadInt(0) / args.unsafeReadInt(1)

    def intLong(args: ArgumentReader): Long = args.unsafeReadInt(0) / args.unsafeReadLong(1)

    def intShort(args: ArgumentReader): Int = args.unsafeReadInt(0) / args.unsafeReadShort(1)

    def longByte(args: ArgumentReader): Long = args.unsafeReadLong(0) / args.unsafeReadByte(1)

    def longChar(args: ArgumentReader): Long = args.unsafeReadLong(0) / args.unsafeReadChar(1)

    def longDouble(args: ArgumentReader): Double = args.unsafeReadLong(0) / args.unsafeReadDouble(1)

    def longFloat(args: ArgumentReader): Float = args.unsafeReadLong(0) / args.unsafeReadFloat(1)

    def longInt(args: ArgumentReader): Long = args.unsafeReadLong(0) / args.unsafeReadInt(1)

    def longLong(args: ArgumentReader): Long = args.unsafeReadLong(0) / args.unsafeReadLong(1)

    def longShort(args: ArgumentReader): Long = args.unsafeReadLong(0) / args.unsafeReadShort(1)

    def shortByte(args: ArgumentReader): Int = args.unsafeReadShort(0) / args.unsafeReadByte(1)

    def shortChar(args: ArgumentReader): Int = args.unsafeReadShort(0) / args.unsafeReadChar(1)

    def shortDouble(args: ArgumentReader): Double = args.unsafeReadShort(0) / args.unsafeReadDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.unsafeReadShort(0) / args.unsafeReadFloat(1)

    def shortInt(args: ArgumentReader): Int = args.unsafeReadShort(0) / args.unsafeReadInt(1)

    def shortLong(args: ArgumentReader): Long = args.unsafeReadShort(0) / args.unsafeReadLong(1)

    def shortShort(args: ArgumentReader): Int = args.unsafeReadShort(0) / args.unsafeReadShort(1)
  }

  object modulo extends BinaryOp {
    val name: Name = Name("%")

    def byteByte(args: ArgumentReader): Int = args.unsafeReadByte(0) % args.unsafeReadByte(1)

    def byteChar(args: ArgumentReader): Int = args.unsafeReadByte(0) % args.unsafeReadChar(1)

    def byteDouble(args: ArgumentReader): Double = args.unsafeReadByte(0) % args.unsafeReadDouble(1)

    def byteFloat(args: ArgumentReader): Float = args.unsafeReadByte(0) % args.unsafeReadFloat(1)

    def byteInt(args: ArgumentReader): Int = args.unsafeReadByte(0) % args.unsafeReadInt(1)

    def byteLong(args: ArgumentReader): Long = args.unsafeReadByte(0) % args.unsafeReadLong(1)

    def byteShort(args: ArgumentReader): Int = args.unsafeReadByte(0) % args.unsafeReadShort(1)

    def charByte(args: ArgumentReader): Int = args.unsafeReadChar(0) % args.unsafeReadByte(1)

    def charChar(args: ArgumentReader): Int = args.unsafeReadChar(0) % args.unsafeReadChar(1)

    def charDouble(args: ArgumentReader): Double = args.unsafeReadChar(0) % args.unsafeReadDouble(1)

    def charFloat(args: ArgumentReader): Float = args.unsafeReadChar(0) % args.unsafeReadFloat(1)

    def charInt(args: ArgumentReader): Int = args.unsafeReadChar(0) % args.unsafeReadInt(1)

    def charLong(args: ArgumentReader): Long = args.unsafeReadChar(0) % args.unsafeReadLong(1)

    def charShort(args: ArgumentReader): Int = args.unsafeReadChar(0) % args.unsafeReadShort(1)

    def doubleByte(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadByte(1)

    def doubleChar(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadChar(1)

    def doubleDouble(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadDouble(1)

    def doubleFloat(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadFloat(1)

    def doubleInt(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadInt(1)

    def doubleLong(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadLong(1)

    def doubleShort(args: ArgumentReader): Double = args.unsafeReadDouble(0) % args.unsafeReadShort(1)

    def floatByte(args: ArgumentReader): Float = args.unsafeReadFloat(0) % args.unsafeReadByte(1)

    def floatChar(args: ArgumentReader): Float = args.unsafeReadFloat(0) % args.unsafeReadChar(1)

    def floatDouble(args: ArgumentReader): Double = args.unsafeReadFloat(0) % args.unsafeReadDouble(1)

    def floatFloat(args: ArgumentReader): Float = args.unsafeReadFloat(0) % args.unsafeReadFloat(1)

    def floatInt(args: ArgumentReader): Float = args.unsafeReadFloat(0) % args.unsafeReadInt(1)

    def floatLong(args: ArgumentReader): Float = args.unsafeReadFloat(0) % args.unsafeReadLong(1)

    def floatShort(args: ArgumentReader): Float = args.unsafeReadFloat(0) % args.unsafeReadShort(1)

    def intByte(args: ArgumentReader): Int = args.unsafeReadInt(0) % args.unsafeReadByte(1)

    def intChar(args: ArgumentReader): Int = args.unsafeReadInt(0) % args.unsafeReadChar(1)

    def intDouble(args: ArgumentReader): Double = args.unsafeReadInt(0) % args.unsafeReadDouble(1)

    def intFloat(args: ArgumentReader): Float = args.unsafeReadInt(0) % args.unsafeReadFloat(1)

    def intInt(args: ArgumentReader): Int = args.unsafeReadInt(0) % args.unsafeReadInt(1)

    def intLong(args: ArgumentReader): Long = args.unsafeReadInt(0) % args.unsafeReadLong(1)

    def intShort(args: ArgumentReader): Int = args.unsafeReadInt(0) % args.unsafeReadShort(1)

    def longByte(args: ArgumentReader): Long = args.unsafeReadLong(0) % args.unsafeReadByte(1)

    def longChar(args: ArgumentReader): Long = args.unsafeReadLong(0) % args.unsafeReadChar(1)

    def longDouble(args: ArgumentReader): Double = args.unsafeReadLong(0) % args.unsafeReadDouble(1)

    def longFloat(args: ArgumentReader): Float = args.unsafeReadLong(0) % args.unsafeReadFloat(1)

    def longInt(args: ArgumentReader): Long = args.unsafeReadLong(0) % args.unsafeReadInt(1)

    def longLong(args: ArgumentReader): Long = args.unsafeReadLong(0) % args.unsafeReadLong(1)

    def longShort(args: ArgumentReader): Long = args.unsafeReadLong(0) % args.unsafeReadShort(1)

    def shortByte(args: ArgumentReader): Int = args.unsafeReadShort(0) % args.unsafeReadByte(1)

    def shortChar(args: ArgumentReader): Int = args.unsafeReadShort(0) % args.unsafeReadChar(1)

    def shortDouble(args: ArgumentReader): Double = args.unsafeReadShort(0) % args.unsafeReadDouble(1)

    def shortFloat(args: ArgumentReader): Float = args.unsafeReadShort(0) % args.unsafeReadFloat(1)

    def shortInt(args: ArgumentReader): Int = args.unsafeReadShort(0) % args.unsafeReadInt(1)

    def shortLong(args: ArgumentReader): Long = args.unsafeReadShort(0) % args.unsafeReadLong(1)

    def shortShort(args: ArgumentReader): Int = args.unsafeReadShort(0) % args.unsafeReadShort(1)
  }

  private val rhsName = Name("x")
  private val rhsInt = Vector(FormalParameter(rhsName, IntT))
  private val rhsLong = Vector(FormalParameter(rhsName, LongT))
  private val rhsShort = Vector(FormalParameter(rhsName, CoreTypes.ShortT))
  private val rhsByte = Vector(FormalParameter(rhsName, CoreTypes.ByteT))
  private val rhsChar = Vector(FormalParameter(rhsName, CoreTypes.CharT))
  private val rhsDouble = Vector(FormalParameter(rhsName, DoubleT))
  private val rhsFloat = Vector(FormalParameter(rhsName, FloatT))
}
