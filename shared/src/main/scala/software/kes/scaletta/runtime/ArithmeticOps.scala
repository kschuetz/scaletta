package software.kes.scaletta.runtime

import software.kes.scaletta.api.ArgumentReader

object ArithmeticOps {
  object add {
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

  object subtract {
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

  object multiply {
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

  object divide {
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

  object modulo {
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
}
