package software.kes.scaletta.spike

trait Conversion {
  type In
  type Out

  def convert(in: In): Out
}

object Conversion {
  object IntToLong extends Conversion {
    type In = Int
    type Out = Long

    def convert(in: Int): Long = in
  }

  object IntToDouble extends Conversion {
    type In = Int
    type Out = Double

    def convert(in: Int): Double = in.toDouble
  }

  object IntToFloat extends Conversion {
    type In = Int
    type Out = Float

    def convert(in: Int): Float = in.toFloat
  }

  object IntToString extends Conversion {
    type In = Int
    type Out = String

    def convert(in: Int): String = in.toString
  }

  object LongToDouble extends Conversion {
    type In = Long
    type Out = Double

    def convert(in: Long): Double = in.toDouble
  }

  object LongToFloat extends Conversion {
    type In = Long
    type Out = Float

    def convert(in: Long): Float = in.toFloat
  }

  object LongToString extends Conversion {
    type In = Long
    type Out = String

    def convert(in: Long): String = in.toString
  }

  object ShortToInt extends Conversion {
    type In = Short
    type Out = Int

    def convert(in: Short): Int = in
  }

  object ShortToLong extends Conversion {
    type In = Short
    type Out = Long

    def convert(in: Short): Long = in
  }

  object ByteToInt extends Conversion {
    type In = Byte
    type Out = Int

    def convert(in: Byte): Int = in
  }

  object ByteToShort extends Conversion {
    type In = Byte
    type Out = Short

    def convert(in: Byte): Short = in
  }

  object ByteToLong extends Conversion {
    type In = Byte
    type Out = Long

    def convert(in: Byte): Long = in
  }

  object CharToInt extends Conversion {
    type In = Char
    type Out = Int

    def convert(in: Char): Int = in
  }

  object CharToLong extends Conversion {
    type In = Char
    type Out = Long

    def convert(in: Char): Long = in
  }

  object CharToString extends Conversion {
    type In = Char
    type Out = String

    def convert(in: Char): String = in.toString
  }
}
