package software.kes.scaletta.internal.format

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.format.FormatSpecifier._
import software.kes.scaletta.internal.format.FormatSpecifierError._
import software.kes.scaletta.internal.format.FormatSpecifierParser.Result
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}

final class FormatSpecifierParserSpec extends AnyFunSpec with Matchers {

  describe("FormatSpecifierParser") {

    describe("when followsVariable is false") {
      it("should parse an empty string") {
        FormatSpecifierParser.parse("", followsVariable = false) shouldBe Result.Success(
          None,
          Pos("", CharIndex(0), CharIndex(0))
        )
      }

      it("should parse a plain string without format specifiers or escapes") {
        FormatSpecifierParser.parse("hello world", followsVariable = false) shouldBe Result.Success(
          None,
          Pos("hello world", CharIndex(0), CharIndex(10))
        )
      }

      it("should unescape %% and %n anywhere in the string") {
        val expectedNewline = System.lineSeparator()
        val result = FormatSpecifierParser.parse("100%% complete%nnext line", followsVariable = false)
        result shouldBe Result.Success(
          None,
          Pos(s"100% complete${expectedNewline}next line", CharIndex(0), CharIndex(24))
        )
      }

      it("should return Failure for lone % or invalid escapes") {
        FormatSpecifierParser.parse("%", followsVariable = false) shouldBe Result.Failure(
          Pos(LonePercent, CharIndex(0), CharIndex(0))
        )
        FormatSpecifierParser.parse("foo%", followsVariable = false) shouldBe Result.Failure(
          Pos(LonePercent, CharIndex(3), CharIndex(3))
        )
        FormatSpecifierParser.parse("%foo", followsVariable = false) shouldBe Result.Failure(
          Pos(InvalidEscape('f'), CharIndex(0), CharIndex(1))
        )
        FormatSpecifierParser.parse("foo % bar", followsVariable = false) shouldBe Result.Failure(
          Pos(InvalidEscape(' '), CharIndex(4), CharIndex(5))
        )
      }

      it("should return Failure if it starts with what would be a format specifier") {
        FormatSpecifierParser.parse("%d", followsVariable = false) shouldBe Result.Failure(
          Pos(InvalidEscape('d'), CharIndex(0), CharIndex(1))
        )
        FormatSpecifierParser.parse("%10s", followsVariable = false) shouldBe Result.Failure(
          Pos(InvalidEscape('1'), CharIndex(0), CharIndex(1))
        )
      }
    }

    describe("when followsVariable is true") {
      it("should parse an empty string") {
        FormatSpecifierParser.parse("", followsVariable = true) shouldBe Result.Success(
          None,
          Pos("", CharIndex(0), CharIndex(0))
        )
      }

      it("should parse a plain string not starting with %") {
        FormatSpecifierParser.parse(" kilograms", followsVariable = true) shouldBe Result.Success(
          None,
          Pos(" kilograms", CharIndex(0), CharIndex(9))
        )
      }

      it("should treat leading %% and %n as literal escapes rather than format specifiers") {
        val expectedNewline = System.lineSeparator()
        FormatSpecifierParser.parse("%% remainder", followsVariable = true) shouldBe Result.Success(
          None,
          Pos("% remainder", CharIndex(0), CharIndex(11))
        )
        FormatSpecifierParser.parse("%n remainder", followsVariable = true) shouldBe Result.Success(
          None,
          Pos(s"${expectedNewline} remainder", CharIndex(0), CharIndex(11))
        )
      }

      describe("General conversions (s, S, b, B, h, H)") {
        it("should parse simple %s and %S") {
          FormatSpecifierParser.parse("%s", followsVariable = true) shouldBe Result.Success(
            Some(Pos(General(GeneralConversion.StringConversion(isUpperCase = false)), CharIndex(0), CharIndex(1))),
            Pos("", CharIndex(2), CharIndex(2))
          )
          FormatSpecifierParser.parse("%S", followsVariable = true) shouldBe Result.Success(
            Some(Pos(General(GeneralConversion.StringConversion(isUpperCase = true)), CharIndex(0), CharIndex(1))),
            Pos("", CharIndex(2), CharIndex(2))
          )
        }

        it("should parse %b, %B, %h, %H with width and precision") {
          FormatSpecifierParser.parse("%13.7b", followsVariable = true) shouldBe Result.Success(
            Some(Pos(General(GeneralConversion.BooleanConversion(isUpperCase = false), Some(13), Some(7)), CharIndex(0), CharIndex(5))),
            Pos("", CharIndex(6), CharIndex(6))
          )
          FormatSpecifierParser.parse("%-17.5H tail", followsVariable = true) shouldBe Result.Success(
            Some(Pos(General(GeneralConversion.HashCodeConversion(isUpperCase = true), Some(17), Some(5), Justification.Left), CharIndex(0), CharIndex(6))),
            Pos(" tail", CharIndex(7), CharIndex(11))
          )
        }

        it("should reject invalid flags for general conversions") {
          FormatSpecifierParser.parse("%013s", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag('0', 's'), CharIndex(0), CharIndex(4))
          )
          FormatSpecifierParser.parse("%+s", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag('+', 's'), CharIndex(0), CharIndex(2))
          )
          FormatSpecifierParser.parse("%#s", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag('#', 's'), CharIndex(0), CharIndex(2))
          )
          FormatSpecifierParser.parse("%,s", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag(',', 's'), CharIndex(0), CharIndex(2))
          )
        }
      }

      describe("Character conversions (c, C)") {
        it("should parse %c and %C with width and justification") {
          FormatSpecifierParser.parse("%c", followsVariable = true) shouldBe Result.Success(
            Some(Pos(Character(isUpperCase = false), CharIndex(0), CharIndex(1))),
            Pos("", CharIndex(2), CharIndex(2))
          )
          FormatSpecifierParser.parse("%-11C", followsVariable = true) shouldBe Result.Success(
            Some(Pos(Character(isUpperCase = true, width = Some(11), justification = Justification.Left), CharIndex(0), CharIndex(4))),
            Pos("", CharIndex(5), CharIndex(5))
          )
        }

        it("should reject precision for character conversions") {
          FormatSpecifierParser.parse("%.3c", followsVariable = true) shouldBe Result.Failure(
            Pos(PrecisionNotPermitted('c'), CharIndex(0), CharIndex(3))
          )
        }
      }

      describe("Decimal integral conversion (d)") {
        it("should parse %d with default settings") {
          FormatSpecifierParser.parse("%d", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DecimalIntegral(), CharIndex(0), CharIndex(1))),
            Pos("", CharIndex(2), CharIndex(2))
          )
        }

        it("should parse %d with alignment, sign formatting, and grouping") {
          FormatSpecifierParser.parse("%+0,19d items", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DecimalIntegral(alignment = Alignment.ZeroPad(19), sign = SignFormat.AlwaysSign, groupingSeparator = true), CharIndex(0), CharIndex(6))),
            Pos(" items", CharIndex(7), CharIndex(12))
          )
          FormatSpecifierParser.parse("%-23d", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DecimalIntegral(alignment = Alignment.Left(23)), CharIndex(0), CharIndex(4))),
            Pos("", CharIndex(5), CharIndex(5))
          )
          FormatSpecifierParser.parse("% 31d", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DecimalIntegral(alignment = Alignment.Right(31), sign = SignFormat.SpaceForPositive), CharIndex(0), CharIndex(4))),
            Pos("", CharIndex(5), CharIndex(5))
          )
          FormatSpecifierParser.parse("%(37d", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DecimalIntegral(alignment = Alignment.Right(37), sign = SignFormat.EncloseNegative), CharIndex(0), CharIndex(4))),
            Pos("", CharIndex(5), CharIndex(5))
          )
        }

        it("should reject alternate form (#) or precision for %d") {
          FormatSpecifierParser.parse("%#d", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag('#', 'd'), CharIndex(0), CharIndex(2))
          )
          FormatSpecifierParser.parse("%.5d", followsVariable = true) shouldBe Result.Failure(
            Pos(PrecisionNotPermitted('d'), CharIndex(0), CharIndex(3))
          )
        }
      }

      describe("Octal (o) and Hexadecimal (x, X) integral conversions") {
        it("should parse %o with alternate form and width") {
          FormatSpecifierParser.parse("%#07o", followsVariable = true) shouldBe Result.Success(
            Some(Pos(OctalIntegral(alignment = Alignment.ZeroPad(7), alternateForm = true), CharIndex(0), CharIndex(4))),
            Pos("", CharIndex(5), CharIndex(5))
          )
        }

        it("should parse %x and %X with alternate form and left alignment") {
          FormatSpecifierParser.parse("%#-11x", followsVariable = true) shouldBe Result.Success(
            Some(Pos(HexIntegral(isUpperCase = false, alignment = Alignment.Left(11), alternateForm = true), CharIndex(0), CharIndex(5))),
            Pos("", CharIndex(6), CharIndex(6))
          )
          FormatSpecifierParser.parse("%X", followsVariable = true) shouldBe Result.Success(
            Some(Pos(HexIntegral(isUpperCase = true), CharIndex(0), CharIndex(1))),
            Pos("", CharIndex(2), CharIndex(2))
          )
        }

        it("should reject sign flags, grouping separator, or precision for %o, %x, %X") {
          FormatSpecifierParser.parse("%+o", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag('+', 'o'), CharIndex(0), CharIndex(2))
          )
          FormatSpecifierParser.parse("%,x", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag(',', 'x'), CharIndex(0), CharIndex(2))
          )
          FormatSpecifierParser.parse("%.3X", followsVariable = true) shouldBe Result.Failure(
            Pos(PrecisionNotPermitted('X'), CharIndex(0), CharIndex(3))
          )
        }
      }

      describe("Floating point conversions (f, e, E, g, G, a, A)") {
        it("should parse %f with precision and grouping") {
          FormatSpecifierParser.parse("%,11.3f meters", followsVariable = true) shouldBe Result.Success(
            Some(Pos(FloatingPoint(
              conversion = FloatConversion.DecimalFloat,
              alignment = Alignment.Right(11),
              groupingSeparator = true,
              precision = Some(3)
            ), CharIndex(0), CharIndex(6))),
            Pos(" meters", CharIndex(7), CharIndex(13))
          )
        }

        it("should parse scientific and general scientific notations") {
          FormatSpecifierParser.parse("%+017.5e", followsVariable = true) shouldBe Result.Success(
            Some(Pos(FloatingPoint(
              conversion = FloatConversion.Scientific(isUpperCase = false),
              alignment = Alignment.ZeroPad(17),
              sign = SignFormat.AlwaysSign,
              precision = Some(5)
            ), CharIndex(0), CharIndex(7))),
            Pos("", CharIndex(8), CharIndex(8))
          )
          FormatSpecifierParser.parse("%#G", followsVariable = true) shouldBe Result.Success(
            Some(Pos(FloatingPoint(
              conversion = FloatConversion.GeneralScientific(isUpperCase = true),
              alternateForm = true
            ), CharIndex(0), CharIndex(2))),
            Pos("", CharIndex(3), CharIndex(3))
          )
        }

        it("should parse hexadecimal floating point %a and %A") {
          FormatSpecifierParser.parse("%-13.2a", followsVariable = true) shouldBe Result.Success(
            Some(Pos(FloatingPoint(
              conversion = FloatConversion.HexadecimalFloat(isUpperCase = false),
              alignment = Alignment.Left(13),
              precision = Some(2)
            ), CharIndex(0), CharIndex(6))),
            Pos("", CharIndex(7), CharIndex(7))
          )
          FormatSpecifierParser.parse("%A", followsVariable = true) shouldBe Result.Success(
            Some(Pos(FloatingPoint(FloatConversion.HexadecimalFloat(isUpperCase = true)), CharIndex(0), CharIndex(1))),
            Pos("", CharIndex(2), CharIndex(2))
          )
        }

        it("should reject grouping separator for %a and %A") {
          FormatSpecifierParser.parse("%,a", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag(',', 'a'), CharIndex(0), CharIndex(2))
          )
        }
      }

      describe("Date/Time conversions (t, T)") {
        it("should parse %t and %T with valid suffixes") {
          FormatSpecifierParser.parse("%tH", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DateTime(DateTimeSuffix.Hour24, isUpperCase = false), CharIndex(0), CharIndex(2))),
            Pos("", CharIndex(3), CharIndex(3))
          )
          FormatSpecifierParser.parse("%-29TB", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DateTime(DateTimeSuffix.MonthNameFull, isUpperCase = true, width = Some(29), justification = Justification.Left), CharIndex(0), CharIndex(5))),
            Pos("", CharIndex(6), CharIndex(6))
          )
          FormatSpecifierParser.parse("%tF date", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DateTime(DateTimeSuffix.IsoDate, isUpperCase = false), CharIndex(0), CharIndex(2))),
            Pos(" date", CharIndex(3), CharIndex(7))
          )
        }

        it("should reject invalid date/time suffixes or incomplete specifiers") {
          FormatSpecifierParser.parse("%t", followsVariable = true) shouldBe Result.Failure(
            Pos(MissingDateTimeSuffix, CharIndex(0), CharIndex(1))
          )
          FormatSpecifierParser.parse("%tX", followsVariable = true) shouldBe Result.Failure(
            Pos(UnknownDateTimeSuffix('X'), CharIndex(0), CharIndex(2))
          )
          FormatSpecifierParser.parse("%t@", followsVariable = true) shouldBe Result.Failure(
            Pos(UnknownDateTimeSuffix('@'), CharIndex(0), CharIndex(2))
          )
        }

        it("should reject precision or unsupported flags for date/time") {
          FormatSpecifierParser.parse("%.2tH", followsVariable = true) shouldBe Result.Failure(
            Pos(PrecisionNotPermitted('t'), CharIndex(0), CharIndex(4))
          )
          FormatSpecifierParser.parse("%+tH", followsVariable = true) shouldBe Result.Failure(
            Pos(IncompatibleFlag('+', 't'), CharIndex(0), CharIndex(3))
          )
        }
      }

      describe("Suffix unescaping and error handling") {
        it("should unescape %% and %n in suffix following a specifier") {
          val expectedNewline = System.lineSeparator()
          FormatSpecifierParser.parse("%d%% total%nnext", followsVariable = true) shouldBe Result.Success(
            Some(Pos(DecimalIntegral(), CharIndex(0), CharIndex(1))),
            Pos(s"% total${expectedNewline}next", CharIndex(2), CharIndex(15))
          )
        }

        it("should return Failure if suffix contains invalid lone %") {
          FormatSpecifierParser.parse("%d remaining % error", followsVariable = true) shouldBe Result.Failure(
            Pos(InvalidEscape(' '), CharIndex(13), CharIndex(14))
          )
          FormatSpecifierParser.parse("%s %", followsVariable = true) shouldBe Result.Failure(
            Pos(LonePercent, CharIndex(3), CharIndex(3))
          )
        }

        it("should return Failure for conflicting flags") {
          FormatSpecifierParser.parse("%-011d", followsVariable = true) shouldBe Result.Failure(
            Pos(ConflictingFlags('-', '0'), CharIndex(0), CharIndex(5))
          )
          FormatSpecifierParser.parse("%+ 13f", followsVariable = true) shouldBe Result.Failure(
            Pos(ConflictingSignFlags(Vector('+', ' ')), CharIndex(0), CharIndex(5))
          )
          FormatSpecifierParser.parse("%+ (17d", followsVariable = true) shouldBe Result.Failure(
            Pos(ConflictingSignFlags(Vector('+', ' ', '(')), CharIndex(0), CharIndex(6))
          )
        }

        it("should return Failure for incomplete precision specification") {
          FormatSpecifierParser.parse("%.s", followsVariable = true) shouldBe Result.Failure(
            Pos(MissingPrecision, CharIndex(0), CharIndex(1))
          )
          FormatSpecifierParser.parse("%11.", followsVariable = true) shouldBe Result.Failure(
            Pos(MissingPrecision, CharIndex(0), CharIndex(3))
          )
        }

        it("should return Failure for unknown conversion character") {
          FormatSpecifierParser.parse("%w", followsVariable = true) shouldBe Result.Failure(
            Pos(UnknownConversion('w'), CharIndex(0), CharIndex(1))
          )
        }

        it("should return Failure for missing conversion") {
          FormatSpecifierParser.parse("%", followsVariable = true) shouldBe Result.Failure(
            Pos(MissingConversion, CharIndex(0), CharIndex(0))
          )
          FormatSpecifierParser.parse("%+19", followsVariable = true) shouldBe Result.Failure(
            Pos(MissingConversion, CharIndex(0), CharIndex(3))
          )
        }

        it("should return Failure for width and precision overflow") {
          FormatSpecifierParser.parse("%99999999999999999999d", followsVariable = true) shouldBe Result.Failure(
            Pos(WidthOverflow, CharIndex(0), CharIndex(20))
          )
          FormatSpecifierParser.parse("%.99999999999999999999f", followsVariable = true) shouldBe Result.Failure(
            Pos(PrecisionOverflow, CharIndex(0), CharIndex(21))
          )
        }
      }
    }
  }
}
