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
  }
}
