package software.kes.scaletta.spike

import software.kes.scaletta.spike.Conversion._

object Operations {
  trait Binary {
    type L

    type R

    type Result

    def resultType: Type

    def run(lhs: L, rhs: R): Result

    def unsafeRun(lhs: Any, rhs: Any): Result =
      run(lhs.asInstanceOf[L], rhs.asInstanceOf[R])
  }

  trait Binary1[Operand] extends Binary {
    type L = Operand
    type R = Operand
  }

  def add(lhs: Type, rhs: Type): Option[Binary] =
    lhs match {
      case Type.IntT => addInt(rhs)
      case Type.LongT => addLong(rhs)
      case Type.ShortT => addShort(lhs)
      case Type.ByteT => addByte(lhs)
      case Type.DoubleT => addDouble(lhs)
      case Type.FloatT => addFloat(lhs)
      case Type.StringT => addString(lhs)
      case Type.CharT => addChar(lhs)
      case _ => None
    }

  private def addInt(rhs: Type): Option[Binary] =
    rhs match {
      case Type.IntT => Some(IntPlusInt)
      case Type.LongT => Some(convertLeft(IntToLong)(LongPlusLong))
      case Type.ShortT => Some(convertRight(ShortToInt)(IntPlusInt))
      case Type.ByteT => Some(convertRight(ByteToInt)(IntPlusInt))
      case Type.DoubleT => Some(convertLeft(IntToDouble)(DoublePlusDouble))
      case Type.FloatT => Some(convertLeft(IntToFloat)(FloatPlusFloat))
      case Type.StringT => Some(convertLeft(IntToString)(StringPlusString))
      case Type.CharT => Some(convertRight(CharToInt)(IntPlusInt))
      case _ => None
    }

  private def addLong(rhs: Type): Option[Binary] =
    rhs match {
      case Type.IntT => Some(convertLeft(IntToLong)(LongPlusLong))
      case Type.LongT => Some(LongPlusLong)
      case Type.ShortT => Some(convertRight(ShortToLong)(LongPlusLong))
      case Type.ByteT => Some(convertRight(ByteToLong)(LongPlusLong))
      case Type.DoubleT => Some(convertLeft(LongToDouble)(DoublePlusDouble))
      case Type.FloatT => Some(convertLeft(LongToFloat)(FloatPlusFloat))
      case Type.StringT => Some(convertLeft(LongToString)(StringPlusString))
      case Type.CharT => Some(convertRight(CharToLong)(LongPlusLong))
      case _ => None
    }

  private def addShort(rhs: Type): Option[Binary] = ???

  private def addByte(rhs: Type): Option[Binary] = ???

  private def addDouble(rhs: Type): Option[Binary] = ???

  private def addFloat(rhs: Type): Option[Binary] = ???

  private def addString(rhs: Type): Option[Binary] = ???

  private def addChar(rhs: Type): Option[Binary] = ???

  object IntPlusInt extends Binary1[Int] {
    type Result = Int

    def run(lhs: Int, rhs: Int): Int = lhs + rhs

    def resultType: Type = Type.IntT
  }

  object LongPlusLong extends Binary1[Long] {
    type Result = Long

    def run(lhs: Long, rhs: Long): Long = lhs + rhs

    def resultType: Type = Type.LongT
  }

  object FloatPlusFloat extends Binary1[Float] {
    type Result = Float

    def run(lhs: Float, rhs: Float): Float = lhs + rhs

    def resultType: Type = Type.FloatT
  }

  object DoublePlusDouble extends Binary1[Double] {
    type Result = Double

    def run(lhs: Double, rhs: Double): Double = lhs + rhs

    def resultType: Type = Type.DoubleT
  }

  object StringPlusString extends Binary1[String] {
    type Result = String

    def run(lhs: String, rhs: String): String = lhs + rhs

    def resultType: Type = Type.StringT
  }

  def convertLeft(conversion: Conversion)
                 (underlying: Binary1[conversion.Out]): Binary =
    new Binary {
      type L = conversion.In
      type R = conversion.Out
      type Result = underlying.Result

      def run(lhs: conversion.In, rhs: conversion.Out): underlying.Result =
        underlying.run(conversion.convert(lhs), rhs)

      def resultType: Type = underlying.resultType
    }

  def convertRight(conversion: Conversion)
                  (underlying: Binary1[conversion.Out]): Binary =
    new Binary {
      type L = conversion.Out
      type R = conversion.In
      type Result = underlying.Result

      def run(lhs: conversion.Out, rhs: conversion.In): underlying.Result =
        underlying.run(lhs, conversion.convert(rhs))

      def resultType: Type = underlying.resultType
    }

}
