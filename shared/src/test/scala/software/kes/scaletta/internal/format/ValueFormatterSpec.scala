package software.kes.scaletta.internal.format

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.format.FormatSpecifier._

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import java.util.Locale

final class ValueFormatterSpec extends AnyFunSpec with Matchers {

  describe("ValueFormatter") {

    describe("applyFormat General") {
      it("should format strings") {
        ValueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false)), "scaletta") shouldBe Right("scaletta")
        ValueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = true)), "scaletta") shouldBe Right("SCALETTA")
        ValueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false), width = Some(11), justification = Justification.Right), "scaletta") shouldBe Right("   scaletta")
        ValueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false), width = Some(11), justification = Justification.Left), "scaletta") shouldBe Right("scaletta   ")
        ValueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false), precision = Some(4)), "scaletta") shouldBe Right("scal")
        ValueFormatter.applyFormat(General(GeneralConversion.StringConversion(isUpperCase = false)), null) shouldBe Right("null")
      }

      it("should format booleans") {
        ValueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = false)), true) shouldBe Right("true")
        ValueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = true)), false) shouldBe Right("FALSE")
        ValueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = false)), null) shouldBe Right("false")
        ValueFormatter.applyFormat(General(GeneralConversion.BooleanConversion(isUpperCase = false)), "other") shouldBe Right("true")
      }

      it("should format hash codes") {
        val obj = "test"
        val expected = java.lang.String.format(Locale.ROOT, "%h", obj)
        ValueFormatter.applyFormat(General(GeneralConversion.HashCodeConversion(isUpperCase = false)), obj) shouldBe Right(expected)
        ValueFormatter.applyFormat(General(GeneralConversion.HashCodeConversion(isUpperCase = true)), obj) shouldBe Right(expected.toUpperCase(Locale.ROOT))
        ValueFormatter.applyFormat(General(GeneralConversion.HashCodeConversion(isUpperCase = false)), null) shouldBe Right("null")
      }
    }

    describe("applyFormat Character") {
      it("should format char values") {
        ValueFormatter.applyFormat(Character(isUpperCase = false), 'k') shouldBe Right("k")
        ValueFormatter.applyFormat(Character(isUpperCase = true), 'k') shouldBe Right("K")
        ValueFormatter.applyFormat(Character(isUpperCase = false, width = Some(5), justification = Justification.Left), 'k') shouldBe Right("k    ")
        ValueFormatter.applyFormat(Character(isUpperCase = false, width = Some(5), justification = Justification.Right), 'k') shouldBe Right("    k")
      }

      it("should format numeric code points") {
        ValueFormatter.applyFormat(Character(isUpperCase = false), 65) shouldBe Right("A")
        ValueFormatter.applyFormat(Character(isUpperCase = true), 97) shouldBe Right("A")
      }

      it("should return IllegalCodePoint error on invalid code point") {
        val spec = Character(isUpperCase = false)
        val result = ValueFormatter.applyFormat(spec, -1)
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
        val result = ValueFormatter.applyFormat(spec, "invalid")
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
        ValueFormatter.applyFormat(spec, 43) shouldBe Right("43")
        ValueFormatter.applyFormat(spec, 43L) shouldBe Right("43")
        ValueFormatter.applyFormat(spec, 43.toShort) shouldBe Right("43")
        ValueFormatter.applyFormat(spec, 43.toByte) shouldBe Right("43")
        ValueFormatter.applyFormat(spec, BigInt(43)) shouldBe Right("43")
        ValueFormatter.applyFormat(spec, new java.math.BigInteger("43")) shouldBe Right("43")
      }

      it("should support flags and alignment") {
        ValueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.ZeroPad(5)), 43) shouldBe Right("00043")
        ValueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.Left(5)), 43) shouldBe Right("43   ")
        ValueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.Right(5)), 43) shouldBe Right("   43")
        ValueFormatter.applyFormat(DecimalIntegral(sign = SignFormat.AlwaysSign), 43) shouldBe Right("+43")
        ValueFormatter.applyFormat(DecimalIntegral(sign = SignFormat.SpaceForPositive), 43) shouldBe Right(" 43")
        ValueFormatter.applyFormat(DecimalIntegral(sign = SignFormat.EncloseNegative), -43) shouldBe Right("(43)")
        ValueFormatter.applyFormat(DecimalIntegral(groupingSeparator = true), 1000000) shouldBe Right("1,000,000")
        ValueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.Left(0)), 43) shouldBe Right("43")
        ValueFormatter.applyFormat(DecimalIntegral(alignment = Alignment.ZeroPad(0)), 43) shouldBe Right("43")
      }

      it("should return IncompatibleType when given a float or string") {
        val spec = DecimalIntegral()
        val result = ValueFormatter.applyFormat(spec, 3.14)
        result match {
          case Left(RuntimeFormattingError.IncompatibleType(s, _, _)) =>
            s shouldBe spec
          case other =>
            fail(s"Expected IncompatibleType, got $other")
        }
      }

      it("should return NullValueNotAllowed when given null") {
        val spec = DecimalIntegral()
        val result = ValueFormatter.applyFormat(spec, null)
        result shouldBe Left(RuntimeFormattingError.NullValueNotAllowed(spec))
      }
    }

    describe("applyFormat OctalIntegral") {
      it("should format octal integers") {
        ValueFormatter.applyFormat(OctalIntegral(), 17) shouldBe Right("21")
        ValueFormatter.applyFormat(OctalIntegral(alternateForm = true), 17) shouldBe Right("021")
        ValueFormatter.applyFormat(OctalIntegral(alignment = Alignment.ZeroPad(5)), 17) shouldBe Right("00021")
        ValueFormatter.applyFormat(OctalIntegral(alignment = Alignment.Left(5)), 17) shouldBe Right("21   ")
        ValueFormatter.applyFormat(OctalIntegral(), BigInt(17)) shouldBe Right("21")
      }
    }

    describe("applyFormat HexIntegral") {
      it("should format hexadecimal integers") {
        ValueFormatter.applyFormat(HexIntegral(isUpperCase = false), 255) shouldBe Right("ff")
        ValueFormatter.applyFormat(HexIntegral(isUpperCase = true), 255) shouldBe Right("FF")
        ValueFormatter.applyFormat(HexIntegral(isUpperCase = false, alternateForm = true), 255) shouldBe Right("0xff")
        ValueFormatter.applyFormat(HexIntegral(isUpperCase = true, alternateForm = true), 255) shouldBe Right("0XFF")
        ValueFormatter.applyFormat(HexIntegral(isUpperCase = false, alignment = Alignment.ZeroPad(6)), 255) shouldBe Right("0000ff")
        ValueFormatter.applyFormat(HexIntegral(isUpperCase = false), BigInt(255)) shouldBe Right("ff")
      }
    }

    describe("applyFormat FloatingPoint") {
      it("should format Float, Double, and BigDecimal") {
        ValueFormatter.applyFormat(FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2)), 3.14159) shouldBe Right("3.14")
        ValueFormatter.applyFormat(FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2)), 3.14159f) shouldBe Right("3.14")
        ValueFormatter.applyFormat(FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2)), BigDecimal("3.14159")) shouldBe Right("3.14")
      }

      it("should support scientific and general scientific notations") {
        val d = 1234.567
        val resSci = ValueFormatter.applyFormat(FloatingPoint(FloatConversion.Scientific(isUpperCase = false), precision = Some(2)), d)
        resSci.isRight shouldBe true
        val resSciUpper = ValueFormatter.applyFormat(FloatingPoint(FloatConversion.Scientific(isUpperCase = true), precision = Some(2)), d)
        resSciUpper.isRight shouldBe true
        val resGen = ValueFormatter.applyFormat(FloatingPoint(FloatConversion.GeneralScientific(isUpperCase = false), precision = Some(4)), d)
        resGen.isRight shouldBe true
      }

      it("should respect explicit locale") {
        val spec = FloatingPoint(FloatConversion.DecimalFloat, precision = Some(2))
        ValueFormatter.applyFormat(spec, 3.14, Locale.GERMANY) shouldBe Right("3,14")
        ValueFormatter.applyFormat(spec, 3.14, Locale.US) shouldBe Right("3.14")
      }
    }

    describe("applyFormat DateTime") {
      it("should format temporal values") {
        val zdt = ZonedDateTime.of(2026, 11, 24, 15, 30, 45, 0, ZoneId.of("UTC"))
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.Year4Digit, isUpperCase = false), zdt) shouldBe Right("2026")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.MonthNumber, isUpperCase = false), zdt) shouldBe Right("11")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.DayOfMonthPadded, isUpperCase = false), zdt) shouldBe Right("24")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.Hour24, isUpperCase = false), zdt) shouldBe Right("15")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.Minute, isUpperCase = false), zdt) shouldBe Right("30")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.Second, isUpperCase = false), zdt) shouldBe Right("45")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.IsoDate, isUpperCase = false), zdt) shouldBe Right("2026-11-24")
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.Time24Long, isUpperCase = false), zdt) shouldBe Right("15:30:45")
      }

      it("should format LocalDate") {
        val ld = LocalDate.of(2026, 11, 24)
        ValueFormatter.applyFormat(DateTime(DateTimeSuffix.IsoDate, isUpperCase = false), ld) shouldBe Right("2026-11-24")
      }

      it("should return IncompatibleType or GeneralFormattingError on invalid type") {
        val spec = DateTime(DateTimeSuffix.Hour24, isUpperCase = false)
        val result = ValueFormatter.applyFormat(spec, "not a date")
        result.isLeft shouldBe true
      }
    }
  }
}
