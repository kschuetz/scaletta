package software.kes.scaletta.internal.format

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.format.FormatSpecifier._

final class FormatSpecifierSpec extends AnyFunSpec with Matchers {

  describe("FormatSpecifier") {

    describe("General") {
      it("should construct General specifier with defaults") {
        val spec: FormatSpecifier = General(GeneralConversion.StringConversion(isUpperCase = false))
        spec shouldBe General(
          conversion = GeneralConversion.StringConversion(isUpperCase = false),
          width = None,
          precision = None,
          justification = Justification.Right
        )
      }

      it("should clamp negative width and precision to 0") {
        val spec = General(
          conversion = GeneralConversion.BooleanConversion(isUpperCase = true),
          width = Some(-11),
          precision = Some(-7),
          justification = Justification.Left
        )
        spec.width shouldBe Some(0)
        spec.precision shouldBe Some(0)
        spec.justification shouldBe Justification.Left
      }

      it("should preserve positive width and precision") {
        val spec = General(
          conversion = GeneralConversion.HashCodeConversion(isUpperCase = false),
          width = Some(17),
          precision = Some(5),
          justification = Justification.Right
        )
        spec.width shouldBe Some(17)
        spec.precision shouldBe Some(5)
      }
    }

    describe("Character") {
      it("should construct Character specifier") {
        val spec = Character(isUpperCase = false, width = Some(13), justification = Justification.Left)
        spec.isUpperCase shouldBe false
        spec.width shouldBe Some(13)
        spec.justification shouldBe Justification.Left
      }

      it("should clamp negative width to 0") {
        val spec = Character(isUpperCase = true, width = Some(-19))
        spec.width shouldBe Some(0)
      }
    }

    describe("DecimalIntegral") {
      it("should construct DecimalIntegral with defaults") {
        val spec = DecimalIntegral()
        spec.alignment shouldBe Alignment.Unspecified
        spec.sign shouldBe SignFormat.Default
        spec.groupingSeparator shouldBe false
      }

      it("should support alignment, sign formatting, and grouping separator") {
        val spec = DecimalIntegral(
          alignment = Alignment.ZeroPad(11),
          sign = SignFormat.AlwaysSign,
          groupingSeparator = true
        )
        spec.alignment shouldBe Alignment.ZeroPad(11)
        spec.sign shouldBe SignFormat.AlwaysSign
        spec.groupingSeparator shouldBe true
      }
    }

    describe("OctalIntegral") {
      it("should construct OctalIntegral with defaults") {
        val spec = OctalIntegral()
        spec.alignment shouldBe Alignment.Unspecified
        spec.alternateForm shouldBe false
      }

      it("should support alternate form and alignment") {
        val spec = OctalIntegral(alignment = Alignment.Left(7), alternateForm = true)
        spec.alignment shouldBe Alignment.Left(7)
        spec.alternateForm shouldBe true
      }
    }

    describe("HexIntegral") {
      it("should construct HexIntegral with uppercase and alternate form") {
        val spec = HexIntegral(isUpperCase = true, alignment = Alignment.Right(23), alternateForm = true)
        spec.isUpperCase shouldBe true
        spec.alignment shouldBe Alignment.Right(23)
        spec.alternateForm shouldBe true
      }
    }

    describe("FloatingPoint") {
      it("should construct DecimalFloat with defaults") {
        val spec = FloatingPoint(FloatConversion.DecimalFloat)
        spec.conversion shouldBe FloatConversion.DecimalFloat
        spec.alignment shouldBe Alignment.Unspecified
        spec.sign shouldBe SignFormat.Default
        spec.alternateForm shouldBe false
        spec.groupingSeparator shouldBe false
        spec.precision shouldBe None
      }

      it("should clamp negative precision to 0") {
        val spec = FloatingPoint(
          conversion = FloatConversion.Scientific(isUpperCase = true),
          alignment = Alignment.ZeroPad(31),
          sign = SignFormat.SpaceForPositive,
          alternateForm = true,
          groupingSeparator = true,
          precision = Some(-13)
        )
        spec.precision shouldBe Some(0)
      }
    }

    describe("DateTime") {
      it("should construct DateTime specifier with valid suffix") {
        val spec = DateTime(
          suffix = DateTimeSuffix.Hour24,
          isUpperCase = false,
          width = Some(29),
          justification = Justification.Left
        )
        spec.suffix shouldBe DateTimeSuffix.Hour24
        spec.isUpperCase shouldBe false
        spec.width shouldBe Some(29)
        spec.justification shouldBe Justification.Left
      }

      it("should clamp negative width to 0") {
        val spec = DateTime(suffix = DateTimeSuffix.IsoDate, isUpperCase = true, width = Some(-37))
        spec.width shouldBe Some(0)
      }

      it("should lookup DateTimeSuffix from valid characters") {
        DateTimeSuffix.fromChar('H') shouldBe Some(DateTimeSuffix.Hour24)
        DateTimeSuffix.fromChar('M') shouldBe Some(DateTimeSuffix.Minute)
        DateTimeSuffix.fromChar('S') shouldBe Some(DateTimeSuffix.Second)
        DateTimeSuffix.fromChar('B') shouldBe Some(DateTimeSuffix.MonthNameFull)
        DateTimeSuffix.fromChar('F') shouldBe Some(DateTimeSuffix.IsoDate)
        DateTimeSuffix.fromChar('c') shouldBe Some(DateTimeSuffix.FullDateTime)
        DateTimeSuffix.fromChar('x') shouldBe None
        DateTimeSuffix.fromChar('@') shouldBe None
      }
    }

    describe("Alignment") {
      it("should clamp negative widths in Alignment constructors") {
        Alignment.Left(-7) shouldBe Alignment.Left(0)
        Alignment.Right(-11) shouldBe Alignment.Right(0)
        Alignment.ZeroPad(-13) shouldBe Alignment.ZeroPad(0)

        Alignment.left(-17) shouldBe Alignment.Left(0)
        Alignment.right(-19) shouldBe Alignment.Right(0)
        Alignment.zeroPad(-23) shouldBe Alignment.ZeroPad(0)
      }
    }

    describe("toPattern") {
      it("should reconstruct General string patterns") {
        General(GeneralConversion.StringConversion(isUpperCase = false)).toPattern shouldBe "%s"
        General(GeneralConversion.StringConversion(isUpperCase = true)).toPattern shouldBe "%S"
        General(GeneralConversion.StringConversion(isUpperCase = false), width = Some(11), precision = Some(5), justification = Justification.Left).toPattern shouldBe "%-11.5s"
        General(GeneralConversion.StringConversion(isUpperCase = false), width = Some(13), precision = Some(7), justification = Justification.Right).toPattern shouldBe "%13.7s"

        FormatSpecifier.toPattern(General(GeneralConversion.StringConversion(isUpperCase = false))) shouldBe "%s"
      }

      it("should reconstruct General boolean and hashcode patterns") {
        General(GeneralConversion.BooleanConversion(isUpperCase = false)).toPattern shouldBe "%b"
        General(GeneralConversion.BooleanConversion(isUpperCase = true)).toPattern shouldBe "%B"
        General(GeneralConversion.HashCodeConversion(isUpperCase = false)).toPattern shouldBe "%h"
        General(GeneralConversion.HashCodeConversion(isUpperCase = true)).toPattern shouldBe "%H"
      }

      it("should reconstruct Character patterns") {
        Character(isUpperCase = false).toPattern shouldBe "%c"
        Character(isUpperCase = true).toPattern shouldBe "%C"
        Character(isUpperCase = false, width = Some(7), justification = Justification.Left).toPattern shouldBe "%-7c"
        Character(isUpperCase = true, width = Some(11), justification = Justification.Right).toPattern shouldBe "%11C"
      }

      it("should reconstruct DecimalIntegral patterns") {
        DecimalIntegral().toPattern shouldBe "%d"
        DecimalIntegral(alignment = Alignment.ZeroPad(11), sign = SignFormat.AlwaysSign, groupingSeparator = true).toPattern shouldBe "%+,011d"
        DecimalIntegral(alignment = Alignment.Left(13), sign = SignFormat.SpaceForPositive).toPattern shouldBe "% -13d"
        DecimalIntegral(alignment = Alignment.Right(17), sign = SignFormat.EncloseNegative).toPattern shouldBe "%(17d"
      }

      it("should reconstruct OctalIntegral patterns") {
        OctalIntegral().toPattern shouldBe "%o"
        OctalIntegral(alignment = Alignment.ZeroPad(7), alternateForm = true).toPattern shouldBe "%#07o"
        OctalIntegral(alignment = Alignment.Left(11)).toPattern shouldBe "%-11o"
      }

      it("should reconstruct HexIntegral patterns") {
        HexIntegral(isUpperCase = false).toPattern shouldBe "%x"
        HexIntegral(isUpperCase = true, alignment = Alignment.ZeroPad(13), alternateForm = true).toPattern shouldBe "%#013X"
        HexIntegral(isUpperCase = false, alignment = Alignment.Left(17)).toPattern shouldBe "%-17x"
      }

      it("should reconstruct FloatingPoint patterns") {
        FloatingPoint(FloatConversion.DecimalFloat).toPattern shouldBe "%f"
        FloatingPoint(
          conversion = FloatConversion.Scientific(isUpperCase = false),
          alignment = Alignment.ZeroPad(19),
          sign = SignFormat.AlwaysSign,
          alternateForm = true,
          groupingSeparator = true,
          precision = Some(5)
        ).toPattern shouldBe "%+#,019.5e"
        FloatingPoint(FloatConversion.GeneralScientific(isUpperCase = true), alignment = Alignment.Left(23), precision = Some(3)).toPattern shouldBe "%-23.3G"
        FloatingPoint(FloatConversion.HexadecimalFloat(isUpperCase = true), precision = Some(2)).toPattern shouldBe "%.2A"
      }

      it("should reconstruct DateTime patterns") {
        DateTime(DateTimeSuffix.Hour24, isUpperCase = false).toPattern shouldBe "%tH"
        DateTime(DateTimeSuffix.MonthNameFull, isUpperCase = true, width = Some(29), justification = Justification.Left).toPattern shouldBe "%-29TB"
      }
    }
  }
}
