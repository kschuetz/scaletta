package software.kes.scaletta.internal.format

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.format.FormatSpecifier._

import scala.scalajs.js

final class ValueFormatterSpec extends AnyFunSpec with Matchers {

  private val valueFormatter = ValueFormatter.create()

  describe("valueFormatter on Scala.js") {

    describe("applyFormat General") {
      it("should format strings") {
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false)), "scaletta") shouldBe Right("scaletta")
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = true)), "scaletta") shouldBe Right("SCALETTA")
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false), width = Some(11), justification = Justification.Right), "scaletta") shouldBe Right("   scaletta")
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false), width = Some(11), justification = Justification.Left), "scaletta") shouldBe Right("scaletta   ")
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false), precision = Some(4)), "scaletta") shouldBe Right("scal")
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false)), null) shouldBe Right("null")
        valueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = true)), null) shouldBe Right("NULL")
      }

      it("should format booleans") {
        valueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = false)), true) shouldBe Right("true")
        valueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = true)), false) shouldBe Right("FALSE")
        valueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = false)), null) shouldBe Right("false")
        valueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = false)), "other") shouldBe Right("true")
      }

      it("should format hash codes") {
        val obj = "test"
        val expected = java.lang.Integer.toHexString(obj.hashCode())
        valueFormatter.applyFormat(General(GeneralConversion.HashCodeConversion(isUpperCase = false)), obj) shouldBe Right(expected)
        valueFormatter.applyFormat(General(GeneralConversion.HashCodeConversion(isUpperCase = true)), obj) shouldBe Right(expected.toUpperCase)
        valueFormatter.applyFormat(General(GeneralConversion.HashCodeConversion(isUpperCase = false)), null) shouldBe Right("null")
      }
    }

    describe("applyFormat Character") {
      it("should format char values") {
        valueFormatter.applyFormat(Character(isUpperCase = false), 'k') shouldBe Right("k")
        valueFormatter.applyFormat(Character(isUpperCase = true), 'k') shouldBe Right("K")
        valueFormatter.applyFormat(Character(isUpperCase = false, width = Some(5), justification = Justification.Left), 'k') shouldBe Right("k    ")
        valueFormatter.applyFormat(Character(isUpperCase = false, width = Some(5), justification = Justification.Right), 'k') shouldBe Right("    k")
      }

      it("should format numeric code points") {
        valueFormatter.applyFormat(Character(isUpperCase = false), 65) shouldBe Right("A")
        valueFormatter.applyFormat(Character(isUpperCase = true), 97) shouldBe Right("A")
      }

      it("should return IllegalCodePoint error on invalid code point") {
        val spec = Character(isUpperCase = false)
        val result = valueFormatter.applyFormat(spec, -1)
        result match {
          case Left(RuntimeFormattingError.IllegalCodePoint(s, cp)) =>
            s shouldBe spec
            cp shouldBe -1
          case other =>
            fail(s"Expected IllegalCodePoint, got $other")
        }
      }

      it("should return IncompatibleType error when non-character value is passed") {
        val spec = Character(isUpperCase = false)
        val result = valueFormatter.applyFormat(spec, "invalid")
        result match {
          case Left(RuntimeFormattingError.IncompatibleType(s, actualType, _)) =>
            s shouldBe spec
            actualType shouldBe classOf[String].getName
          case other =>
            fail(s"Expected IncompatibleType, got $other")
        }
      }
    }

    describe("applyFormat DecimalIntegral") {
      it("should format Int, Long, Short, Byte, and BigInt") {
        val spec = DecimalIntegral()
        valueFormatter.applyFormat(spec, 43) shouldBe Right("43")
        valueFormatter.applyFormat(spec, 43L) shouldBe Right("43")
        valueFormatter.applyFormat(spec, 43.toShort) shouldBe Right("43")
        valueFormatter.applyFormat(spec, 43.toByte) shouldBe Right("43")
        valueFormatter.applyFormat(spec, BigInt(43)) shouldBe Right("43")
        valueFormatter.applyFormat(spec, new java.math.BigInteger("43")) shouldBe Right("43")
      }

      it("should support flags and alignment") {
        valueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.ZeroPad(5)), 43) shouldBe Right("00043")
        valueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.Left(5)), 43) shouldBe Right("43   ")
        valueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.Right(5)), 43) shouldBe Right("   43")
        valueFormatter.applyFormat(DecimalIntegral(sign = SignFormat.AlwaysSign), 43) shouldBe Right("+43")
        valueFormatter.applyFormat(DecimalIntegral(sign = SignFormat.SpaceForPositive), 43) shouldBe Right(" 43")
        valueFormatter.applyFormat(DecimalIntegral(sign = SignFormat.EncloseNegative), -43) shouldBe Right("(43)")
        valueFormatter.applyFormat(DecimalIntegral(groupingSeparator = true), 1000003) shouldBe Right("1,000,003")
        valueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.Left(0)), 43) shouldBe Right("43")
        valueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.ZeroPad(0)), 43) shouldBe Right("43")
      }

      it("should return IncompatibleType when given a float or string") {
        val spec = DecimalIntegral()
        val result = valueFormatter.applyFormat(spec, 3.14)
        result match {
          case Left(RuntimeFormattingError.IncompatibleType(s, _, _)) =>
            s shouldBe spec
          case other =>
            fail(s"Expected IncompatibleType, got $other")
        }
      }

      it("should return NullValueNotAllowed when given null") {
        val spec = DecimalIntegral()
        val result = valueFormatter.applyFormat(spec, null)
        result shouldBe Left(RuntimeFormattingError.NullValueNotAllowed(spec))
      }
    }

    describe("applyFormat OctalIntegral") {
      it("should format octal integers") {
        valueFormatter.applyFormat(OctalIntegral(), 17) shouldBe Right("21")
        valueFormatter.applyFormat(OctalIntegral(alternateForm = true), 17) shouldBe Right("021")
        valueFormatter.applyFormat(OctalIntegral(alignment = Alignment.ZeroPad(5)), 17) shouldBe Right("00021")
        valueFormatter.applyFormat(OctalIntegral(alignment = Alignment.Left(5)), 17) shouldBe Right("21   ")
        valueFormatter.applyFormat(OctalIntegral(), BigInt(17)) shouldBe Right("21")
      }
    }

    describe("applyFormat HexIntegral") {
      it("should format hexadecimal integers") {
        valueFormatter.applyFormat(HexIntegral(isUpperCase = false), 255) shouldBe Right("ff")
        valueFormatter.applyFormat(HexIntegral(isUpperCase = true), 255) shouldBe Right("FF")
        valueFormatter.applyFormat(HexIntegral(isUpperCase = false, alternateForm = true), 255) shouldBe Right("0xff")
        valueFormatter.applyFormat(HexIntegral(isUpperCase = true, alternateForm = true), 255) shouldBe Right("0XFF")
        valueFormatter.applyFormat(HexIntegral(isUpperCase = false, alignment = Alignment.ZeroPad(6)), 255) shouldBe Right("0000ff")
        valueFormatter.applyFormat(HexIntegral(isUpperCase = false), BigInt(255)) shouldBe Right("ff")
      }
    }

    describe("applyFormat FloatingPoint") {
      it("should format Float, Double, and BigDecimal") {
        valueFormatter.applyFormat(FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2)), 3.14159) shouldBe Right("3.14")
        valueFormatter.applyFormat(FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2)), 3.14159f) shouldBe Right("3.14")
        valueFormatter.applyFormat(FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2)), BigDecimal("3.14159")) shouldBe Right("3.14")
      }

      it("should support scientific and general scientific notations") {
        val d = 1234.567
        val resSci = valueFormatter.applyFormat(FloatingPoint(FloatConversion.Scientific(isUpperCase = false), precision = Some(2)), d)
        resSci shouldBe Right("1.23e+03")

        val resSciUpper = valueFormatter.applyFormat(FloatingPoint(FloatConversion.Scientific(isUpperCase = true), precision = Some(2)), d)
        resSciUpper shouldBe Right("1.23E+03")

        val resGen = valueFormatter.applyFormat(FloatingPoint(FloatConversion.GeneralScientific(isUpperCase = false), precision = Some(4)), d)
        resGen.isRight shouldBe true
      }

      it("should return IncompatibleType for hexadecimal floating-point conversion (%a/%A)") {
        val spec = FloatingPoint(FloatConversion.HexadecimalFloat(isUpperCase = false))
        val result = valueFormatter.applyFormat(spec, 3.14)
        result match {
          case Left(RuntimeFormattingError.IncompatibleType(s, _, _)) =>
            s shouldBe spec
          case other =>
            fail(s"Expected IncompatibleType, got $other")
        }
      }
    }

    describe("applyFormat DateTime") {
      it("should format Date and Long timestamp values") {
        val date = new js.Date(js.Date.UTC(2026, 10, 24, 15, 30, 45, 0))
        // Format using date components
        val epochMs = date.getTime().toLong
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.EpochMillis, isUpperCase = false), epochMs) shouldBe Right(epochMs.toString)
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.EpochSeconds, isUpperCase = false), epochMs) shouldBe Right((epochMs / 1000).toString)
      }

      it("should format specific date/time suffixes from js.Date") {
        val date = new js.Date(2026, 10, 24, 15, 30, 45, 123)
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Year4Digit, isUpperCase = false), date) shouldBe Right("2026")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Year2Digit, isUpperCase = false), date) shouldBe Right("26")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.MonthNumber, isUpperCase = false), date) shouldBe Right("11")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.MonthNameFull, isUpperCase = false), date) shouldBe Right("November")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.MonthNameAbbr, isUpperCase = false), date) shouldBe Right("Nov")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.DayOfMonthPadded, isUpperCase = false), date) shouldBe Right("24")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Hour24, isUpperCase = false), date) shouldBe Right("15")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Hour12, isUpperCase = false), date) shouldBe Right("03")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Minute, isUpperCase = false), date) shouldBe Right("30")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Second, isUpperCase = false), date) shouldBe Right("45")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Millisecond, isUpperCase = false), date) shouldBe Right("123")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Time24Short, isUpperCase = false), date) shouldBe Right("15:30")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.Time24Long, isUpperCase = false), date) shouldBe Right("15:30:45")
        valueFormatter.applyFormat(DateTime(DateTimeSuffix.IsoDate, isUpperCase = false), date) shouldBe Right("2026-11-24")
      }

      it("should return IncompatibleType when given an invalid date type") {
        val spec = DateTime(DateTimeSuffix.Hour24, isUpperCase = false)
        val result = valueFormatter.applyFormat(spec, "not a date")
        result.isLeft shouldBe true
      }
    }
  }
}
