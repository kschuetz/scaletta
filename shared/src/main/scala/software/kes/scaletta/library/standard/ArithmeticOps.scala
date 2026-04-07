package software.kes.scaletta.library.standard

import software.kes.scaletta.api.{ArgumentReader, MethodRegistry}
import software.kes.scaletta.builtins.FunctionImpl.{doubleResult, floatResult, intResult, longResult}
import software.kes.scaletta.builtins.{FormalParameter, ParameterGroup}
import software.kes.scaletta.runtime.CoreTypes
import software.kes.scaletta.runtime.CoreTypes._
import software.kes.scaletta.symbols.Name

object ArithmeticOps {

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
      registry.addPureMethod(IntT, name, rhsInt, IntT, intResult(intInt))
      registry.addPureMethod(IntT, name, rhsLong, LongT, longResult(intLong))
      registry.addPureMethod(IntT, name, rhsShort, IntT, intResult(intShort))
      registry.addPureMethod(IntT, name, rhsByte, IntT, intResult(intByte))
      registry.addPureMethod(IntT, name, rhsChar, IntT, intResult(intChar))
      registry.addPureMethod(IntT, name, rhsDouble, DoubleT, doubleResult(intDouble))
      registry.addPureMethod(IntT, name, rhsFloat, FloatT, floatResult(intFloat))

      registry.addPureMethod(LongT, name, rhsInt, LongT, longResult(longInt))
      registry.addPureMethod(LongT, name, rhsLong, LongT, longResult(longLong))
      registry.addPureMethod(LongT, name, rhsShort, LongT, longResult(longShort))
      registry.addPureMethod(LongT, name, rhsByte, LongT, longResult(longByte))
      registry.addPureMethod(LongT, name, rhsChar, LongT, longResult(longChar))
      registry.addPureMethod(LongT, name, rhsDouble, DoubleT, doubleResult(longDouble))
      registry.addPureMethod(LongT, name, rhsFloat, FloatT, floatResult(longFloat))

      registry.addPureMethod(ShortT, name, rhsInt, IntT, longResult(shortInt))
      registry.addPureMethod(ShortT, name, rhsLong, LongT, longResult(shortLong))
      registry.addPureMethod(ShortT, name, rhsShort, IntT, intResult(shortShort))
      registry.addPureMethod(ShortT, name, rhsByte, IntT, intResult(shortByte))
      registry.addPureMethod(ShortT, name, rhsChar, IntT, intResult(shortChar))
      registry.addPureMethod(ShortT, name, rhsDouble, DoubleT, doubleResult(shortDouble))
      registry.addPureMethod(ShortT, name, rhsFloat, FloatT, floatResult(shortFloat))

      registry.addPureMethod(ByteT, name, rhsInt, IntT, longResult(byteInt))
      registry.addPureMethod(ByteT, name, rhsLong, LongT, longResult(byteLong))
      registry.addPureMethod(ByteT, name, rhsShort, IntT, intResult(byteShort))
      registry.addPureMethod(ByteT, name, rhsByte, IntT, intResult(byteByte))
      registry.addPureMethod(ByteT, name, rhsChar, IntT, intResult(byteChar))
      registry.addPureMethod(ByteT, name, rhsDouble, DoubleT, doubleResult(byteDouble))
      registry.addPureMethod(ByteT, name, rhsFloat, FloatT, floatResult(byteFloat))

      registry.addPureMethod(CharT, name, rhsInt, IntT, intResult(charInt))
      registry.addPureMethod(CharT, name, rhsLong, LongT, longResult(charLong))
      registry.addPureMethod(CharT, name, rhsShort, IntT, intResult(charShort))
      registry.addPureMethod(CharT, name, rhsByte, IntT, intResult(charByte))
      registry.addPureMethod(CharT, name, rhsChar, IntT, intResult(charChar))
      registry.addPureMethod(CharT, name, rhsDouble, DoubleT, doubleResult(charDouble))
      registry.addPureMethod(CharT, name, rhsFloat, FloatT, floatResult(charFloat))

      registry.addPureMethod(DoubleT, name, rhsInt, DoubleT, doubleResult(doubleInt))
      registry.addPureMethod(DoubleT, name, rhsLong, DoubleT, doubleResult(doubleLong))
      registry.addPureMethod(DoubleT, name, rhsShort, DoubleT, doubleResult(doubleShort))
      registry.addPureMethod(DoubleT, name, rhsByte, DoubleT, doubleResult(doubleByte))
      registry.addPureMethod(DoubleT, name, rhsChar, DoubleT, doubleResult(doubleChar))
      registry.addPureMethod(DoubleT, name, rhsDouble, DoubleT, doubleResult(doubleDouble))
      registry.addPureMethod(DoubleT, name, rhsFloat, DoubleT, doubleResult(doubleFloat))

      registry.addPureMethod(FloatT, name, rhsInt, FloatT, floatResult(floatInt))
      registry.addPureMethod(FloatT, name, rhsLong, FloatT, floatResult(floatLong))
      registry.addPureMethod(FloatT, name, rhsShort, FloatT, floatResult(floatShort))
      registry.addPureMethod(FloatT, name, rhsByte, FloatT, floatResult(floatByte))
      registry.addPureMethod(FloatT, name, rhsChar, FloatT, floatResult(floatChar))
      registry.addPureMethod(FloatT, name, rhsDouble, DoubleT, doubleResult(floatDouble))
      registry.addPureMethod(FloatT, name, rhsFloat, FloatT, floatResult(floatFloat))
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
  private val rhsInt = ParameterGroup.single(FormalParameter(rhsName, IntT))
  private val rhsLong = ParameterGroup.single(FormalParameter(rhsName, LongT))
  private val rhsShort = ParameterGroup.single(FormalParameter(rhsName, CoreTypes.ShortT))
  private val rhsByte = ParameterGroup.single(FormalParameter(rhsName, CoreTypes.ByteT))
  private val rhsChar = ParameterGroup.single(FormalParameter(rhsName, CoreTypes.CharT))
  private val rhsDouble = ParameterGroup.single(FormalParameter(rhsName, DoubleT))
  private val rhsFloat = ParameterGroup.single(FormalParameter(rhsName, FloatT))
}
