package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.scanner.ScanError._
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class NumericLiteralsSpec extends AnyFunSpec with Matchers {
  private val buffer = CharBuffer.create()

  describe("numericLiteral") {
    describe("not a numeric literal") {
      it("empty") {
        check("", None)
      }

      it("abc") {
        check("abc", None)
      }

      it("-") {
        check("-", None)
      }

      it("-a") {
        check("-a", None)
      }

      it(".") {
        check(".", None)
      }

      it(".a") {
        check(".a", None)
      }

      it("-.") {
        check("-.", None)
      }

      it(".-") {
        check("-.", None)
      }

      it("-.a") {
        check("-.a", None)
      }

      it("_") {
        check("_", None)
      }

      it("-_0") {
        check("-_0", None)
      }

      it("._0") {
        check("._0", None)
      }

      it("-._0") {
        check("-._0", None)
      }
    }

    describe("int") {
      it("0") {
        check("0", Some(success(Token.IntLiteral(0), 0, 0)))
        check("00", Some(success(Token.IntLiteral(0), 0, 1)))
        check("0_0", Some(success(Token.IntLiteral(0), 0, 2)))
        check("0__0", Some(success(Token.IntLiteral(0), 0, 3)))
        check("000", Some(success(Token.IntLiteral(0), 0, 2)))
      }

      it("-0") {
        check("-0", Some(success(Token.IntLiteral(0), 0, 1)))
        check("-00", Some(success(Token.IntLiteral(0), 0, 2)))
        check("-0_0", Some(success(Token.IntLiteral(0), 0, 3)))
        check("-0__0", Some(success(Token.IntLiteral(0), 0, 4)))
      }

      it("123456789") {
        check("123456789", Some(success(Token.IntLiteral(123456789), 0, 8)))
        check("0123456789", Some(success(Token.IntLiteral(123456789), 0, 9)))
        check("123_456_789", Some(success(Token.IntLiteral(123456789), 0, 10)))
        check("0_123_456_789", Some(success(Token.IntLiteral(123456789), 0, 12)))
        check("0_0__1_23__4_56_78____9", Some(success(Token.IntLiteral(123456789), 0, 22)))
      }

      it("zeroes in middle") {
        check("1_000_000_1", Some(success(Token.IntLiteral(1_000_000_1), 0, 10)))
      }

      it("many leading zeroes") {
        check("000000000000000000000000000000000000000000000000000000000000000123",
          Some(success(Token.IntLiteral(123), 0, 65)))
      }

      it("Int.MinValue") {
        check("-2147483648", Some(success(Token.IntLiteral(Int.MinValue), 0, 10)))
        check("-2_147_483_648", Some(success(Token.IntLiteral(Int.MinValue), 0, 13)))
      }

      it("Int.MinValue with leading zeroes") {
        check("-00002147483648", Some(success(Token.IntLiteral(Int.MinValue), 0, 14)))
        check("-00_002_147_483_648", Some(success(Token.IntLiteral(Int.MinValue), 0, 18)))
      }

      it("Int.MinValue-1") {
        check("-2147483649", Some(failure(IntegerNumberTooLarge, 0, 10)))
        check("-2_147_483_649", Some(failure(IntegerNumberTooLarge, 0, 13)))
      }

      it("Int.MaxValue") {
        check("2147483647", Some(success(Token.IntLiteral(Int.MaxValue), 0, 9)))
        check("2_147_483_647", Some(success(Token.IntLiteral(Int.MaxValue), 0, 12)))
      }

      it("Int.MaxValue with leading zeroes") {
        check("00002147483647", Some(success(Token.IntLiteral(Int.MaxValue), 0, 13)))
        check("00_002_147_483_647", Some(success(Token.IntLiteral(Int.MaxValue), 0, 17)))
      }

      it("Int.MaxValue+1") {
        check("2147483648", Some(failure(IntegerNumberTooLarge, 0, 9)))
        check("2_147_483_648", Some(failure(IntegerNumberTooLarge, 0, 12)))
      }

      it("illegal separator") {
        check("0_", Some(failure(IllegalSeparator, 1, 1)))
        check("0__", Some(failure(IllegalSeparator, 2, 2)))
        check("0_a", Some(failure(IllegalSeparator, 1, 1)))
        check("0_L", Some(failure(IllegalSeparator, 1, 1)))
        check("1_0_", Some(failure(IllegalSeparator, 3, 3)))
        check("1_0_a", Some(failure(IllegalSeparator, 3, 3)))
        check("1_0_L", Some(failure(IllegalSeparator, 3, 3)))
      }
    }

    describe("long") {
      it("0") {
        check("0L", Some(success(Token.LongLiteral(0), 0, 1)))
        check("00l", Some(success(Token.LongLiteral(0), 0, 2)))
        check("0_0L", Some(success(Token.LongLiteral(0), 0, 3)))
        check("0__0l", Some(success(Token.LongLiteral(0), 0, 4)))
        check("000L", Some(success(Token.LongLiteral(0), 0, 3)))
      }

      it("-0") {
        check("-0l", Some(success(Token.LongLiteral(0), 0, 2)))
        check("-00L", Some(success(Token.LongLiteral(0), 0, 3)))
        check("-0_0l", Some(success(Token.LongLiteral(0), 0, 4)))
        check("-0__0L", Some(success(Token.LongLiteral(0), 0, 5)))
      }

      it("123456789") {
        check("123456789l", Some(success(Token.LongLiteral(123456789), 0, 9)))
        check("0123456789L", Some(success(Token.LongLiteral(123456789), 0, 10)))
        check("123_456_789l", Some(success(Token.LongLiteral(123456789), 0, 11)))
        check("0_123_456_789L", Some(success(Token.LongLiteral(123456789), 0, 13)))
        check("0_0__1_23__4_56_78____9l", Some(success(Token.LongLiteral(123456789), 0, 23)))
      }

      it("zeroes in middle") {
        check("1_000_000_1L", Some(success(Token.LongLiteral(1_000_000_1), 0, 11)))
      }

      it("many leading zeroes") {
        check("000000000000000000000000000000000000000000000000000000000000000123L",
          Some(success(Token.LongLiteral(123), 0, 66)))
      }

      it("Int.MinValue") {
        check("-2147483648L", Some(success(Token.LongLiteral(Int.MinValue), 0, 11)))
        check("-2_147_483_648l", Some(success(Token.LongLiteral(Int.MinValue), 0, 14)))
      }

      it("Int.MinValue with leading zeroes") {
        check("-00002147483648l", Some(success(Token.LongLiteral(Int.MinValue), 0, 15)))
        check("-00_002_147_483_648L", Some(success(Token.LongLiteral(Int.MinValue), 0, 19)))
      }

      it("Int.MinValue-1") {
        check("-2147483649L", Some(success(Token.LongLiteral(Int.MinValue.toLong - 1), 0, 11)))
        check("-2_147_483_649l", Some(success(Token.LongLiteral(Int.MinValue.toLong - 1), 0, 14)))
      }

      it("Long.MinValue") {
        check("-9223372036854775808L", Some(success(Token.LongLiteral(Long.MinValue), 0, 20)))
        check("-9_223_372_036_854_775_808l", Some(success(Token.LongLiteral(Long.MinValue), 0, 26)))
      }

      it("Long.MinValue with leading zeroes") {
        check("-00009223372036854775808L", Some(success(Token.LongLiteral(Long.MinValue), 0, 24)))
        check("-00_009_223_372_036_854_775_808l", Some(success(Token.LongLiteral(Long.MinValue), 0, 31)))
      }

      it("Long.MinValue-1") {
        check("-9223372036854775809l", Some(failure(IntegerNumberTooLarge, 0, 20)))
        check("-9_223_372_036_854_775_809L", Some(failure(IntegerNumberTooLarge, 0, 26)))
      }

      it("Int.MaxValue") {
        check("2147483647L", Some(success(Token.LongLiteral(Int.MaxValue), 0, 10)))
        check("2_147_483_647l", Some(success(Token.LongLiteral(Int.MaxValue), 0, 13)))
      }

      it("Int.MaxValue with leading zeroes") {
        check("00002147483647l", Some(success(Token.LongLiteral(Int.MaxValue), 0, 14)))
        check("00_002_147_483_647L", Some(success(Token.LongLiteral(Int.MaxValue), 0, 18)))
      }

      it("Int.MaxValue+1") {
        check("2147483648L", Some(success(Token.LongLiteral(Int.MaxValue.toLong + 1), 0, 10)))
        check("2_147_483_648l", Some(success(Token.LongLiteral(Int.MaxValue.toLong + 1), 0, 13)))
      }

      it("Long.MaxValue") {
        check("9223372036854775807L", Some(success(Token.LongLiteral(Long.MaxValue), 0, 19)))
        check("9_223_372_036_854_775_807l", Some(success(Token.LongLiteral(Long.MaxValue), 0, 25)))
      }

      it("Long.MaxValue with leading zeroes") {
        check("00009223372036854775807l", Some(success(Token.LongLiteral(Long.MaxValue), 0, 23)))
        check("00_009_223_372_036_854_775_807L", Some(success(Token.LongLiteral(Long.MaxValue), 0, 30)))
      }

      it("Long.MaxValue+1") {
        check("9223372036854775808L", Some(failure(IntegerNumberTooLarge, 0, 19)))
        check("9_223_372_036_854_775_808l", Some(failure(IntegerNumberTooLarge, 0, 25)))
      }
    }

    describe("binary") {
      it("basic") {
        check("0b1", Some(success(Token.IntLiteral(1), 0, 2)))
      }

      it("underscores") {
        check("0b_1", Some(success(Token.IntLiteral(1), 0, 3)))
        check("0b1_", Some(failure(IllegalSeparator, 3, 3)))
      }

      it("int (small)") {
        check("0b10101010", Some(success(Token.IntLiteral(170), 0, 9)))
        check("0b1010_1010", Some(success(Token.IntLiteral(170), 0, 10)))
      }

      it("long (small)") {
        check("0b10101010L", Some(success(Token.LongLiteral(170), 0, 10)))
        check("0b1010_1010L", Some(success(Token.LongLiteral(170), 0, 11)))
      }

      it("large") {
        check("0b101010101010101010101010101010101010L", Some(success(Token.LongLiteral(45812984490L), 0, 38)))
        check("0b1010_1010_1010_1010_1010_1010_1010_1010_1010L", Some(success(Token.LongLiteral(45812984490L), 0, 46)))
      }

      it("negative int (small)") {
        check("-0b10101010", Some(success(Token.IntLiteral(-170), 0, 10)))
        check("-0b1010_1010", Some(success(Token.IntLiteral(-170), 0, 11)))
      }

      it("negative long (small)") {
        check("-0b10101010L", Some(success(Token.LongLiteral(-170), 0, 11)))
        check("-0b1010_1010L", Some(success(Token.LongLiteral(-170), 0, 12)))
      }

      it("negative large") {
        check("-0b101010101010101010101010101010101010L",
          Some(success(Token.LongLiteral(-45812984490L), 0, 39)))
        check("-0b1010_1010_1010_1010_1010_1010_1010_1010_1010L",
          Some(success(Token.LongLiteral(-45812984490L), 0, 47)))
      }
    }

    describe("hex") {
      it("basic") {
        check("0x1", Some(success(Token.IntLiteral(1), 0, 2)))
      }

      it("underscores") {
        check("0x_1", Some(success(Token.IntLiteral(1), 0, 3)))
        check("0x1_", Some(failure(IllegalSeparator, 3, 3)))
      }

      it("int (small)") {
        check("0x1234abcd", Some(success(Token.IntLiteral(0x1234abcd), 0, 9)))
        check("0x1234_abcd", Some(success(Token.IntLiteral(0x1234abcd), 0, 10)))
      }

      it("long (small)") {
        check("0x1234ABCDL", Some(success(Token.LongLiteral(0x1234abcd), 0, 10)))
        check("0x1234_ABCDL", Some(success(Token.LongLiteral(0x1234abcd), 0, 11)))
      }

      it("large") {
        check("0x12345678abcdEF01l", Some(success(Token.LongLiteral(0x1234_5678_abcd_ef01L), 0, 18)))
        check("0x1234_5678_abcd_EF01l", Some(success(Token.LongLiteral(0x1234_5678_abcd_ef01L), 0, 21)))
      }

      it("negative int (small)") {
        check("-0x1234AbCd", Some(success(Token.IntLiteral(-0x1234abcd), 0, 10)))
        check("-0x1234_AbCd", Some(success(Token.IntLiteral(-0x1234abcd), 0, 11)))
      }

      it("negative long (small)") {
        check("-0x1234aBcDl", Some(success(Token.LongLiteral(-0x1234abcd), 0, 11)))
        check("-0x1234_aBcDl", Some(success(Token.LongLiteral(-0x1234abcd), 0, 12)))
      }

      it("negative (large)") {
        check("-0x12345678AbcdeF01L",
          Some(success(Token.LongLiteral(-0x1234_5678_abcd_ef01L), 0, 19)))
        check("-0x1234_5678_Abcd_eF01L",
          Some(success(Token.LongLiteral(-0x1234_5678_abcd_ef01L), 0, 22)))
      }
    }

    describe("double") {
      it("basic") {
        check("123.456", Some(success(Token.DoubleLiteral(123.456), 0, 6)))
        check("123_456.789_012", Some(success(Token.DoubleLiteral(123456.789012), 0, 14)))
      }

      it("leading dot") {
        check(".123456", Some(success(Token.DoubleLiteral(0.123456), 0, 6)))
        check(".123_456", Some(success(Token.DoubleLiteral(0.123456), 0, 7)))
      }

      it("negative") {
        check("-123.456", Some(success(Token.DoubleLiteral(-123.456), 0, 7)))
        check("-.123456", Some(success(Token.DoubleLiteral(-0.123456), 0, 7)))
      }

      it("zeroes") {
        check("0.0", Some(success(Token.DoubleLiteral(0.0), 0, 2)))
        check("-0.0", Some(success(Token.DoubleLiteral(-0.0), 0, 3)))
        check(".0", Some(success(Token.DoubleLiteral(0.0), 0, 1)))
        check("-.0", Some(success(Token.DoubleLiteral(-0.0), 0, 2)))
      }

      it("scientific notation") {
        check("1e10", Some(success(Token.DoubleLiteral(1e10), 0, 3)))
        check("1.2E-5", Some(success(Token.DoubleLiteral(1.2e-5), 0, 5)))
        check("-1.2e+5", Some(success(Token.DoubleLiteral(-1.2e5), 0, 6)))
        check(".1e2", Some(success(Token.DoubleLiteral(10.0), 0, 3)))
        check("1.0e1_0", Some(success(Token.DoubleLiteral(1.0e10), 0, 6)))
        check("1.0e+1_0", Some(success(Token.DoubleLiteral(1.0e10), 0, 7)))
        check("1.0e-1_0", Some(success(Token.DoubleLiteral(1.0e-10), 0, 7)))
      }

      it("suffix") {
        check("123d", Some(success(Token.DoubleLiteral(123.0), 0, 3)))
        check("123.456D", Some(success(Token.DoubleLiteral(123.456), 0, 7)))
        check("1e10d", Some(success(Token.DoubleLiteral(1e10), 0, 4)))
      }

      it("invalid") {
        check("1.2.3", Some(success(Token.DoubleLiteral(1.2), 0, 2)), checkRemainder = false)
        check("1e", Some(failure(InvalidLiteralNumber, 0, 1)))
        check("1e+", Some(failure(InvalidLiteralNumber, 0, 1)))
        check("1e-", Some(failure(InvalidLiteralNumber, 0, 1)))
        check("1.2dF", Some(failure(InvalidLiteralNumber, 0, 4)))
        check("1.2fD", Some(failure(InvalidLiteralNumber, 0, 4)))
        // TODO: fix check("1.e2", Some(success(Token.IntLiteral(1), 0, 0))) // This is interesting, let's see how it behaves
      }

      it("underscores") {
        check("1_2.3_4", Some(success(Token.DoubleLiteral(12.34), 0, 6)))
        check("1_2.3_4e1_0", Some(success(Token.DoubleLiteral(12.34e10), 0, 10)))
        check("1_.2", Some(failure(IllegalSeparator, 1, 1)))
        check("1.2_", Some(failure(IllegalSeparator, 3, 3)))
        check("1.2_e10", Some(failure(IllegalSeparator, 3, 3)))
        check("1e_10", Some(failure(IllegalSeparator, 2, 2)))
        check("1e10_", Some(failure(IllegalSeparator, 4, 4)))
        check("1e+1_0", Some(success(Token.DoubleLiteral(1e10), 0, 5)))
        check("1e+_10", Some(failure(IllegalSeparator, 3, 3)))
      }

      it("precision") {
        check("1e308", Some(success(Token.DoubleLiteral(1e308), 0, 4)))
        check("1.7976931348623157e308", Some(success(Token.DoubleLiteral(1.7976931348623157e308), 0, 21)))
        check("1e309", Some(failure(FloatingPointPrecisionTooLarge, 0, 4)))
        check("1e-308", Some(success(Token.DoubleLiteral(1e-308), 0, 5)))
        check("4.9e-324", Some(success(Token.DoubleLiteral(4.9e-324), 0, 7)))
        check("1e-325", Some(failure(FloatingPointPrecisionTooSmall, 0, 5)))
      }

      it("deceptive decimal points") {
        check("0.", Some(success(Token.IntLiteral(0), 0, 0)), checkRemainder = false)
        check("1.d", Some(success(Token.IntLiteral(1), 0, 0)), checkRemainder = false)
        check("1._2", Some(success(Token.IntLiteral(1), 0, 0)), checkRemainder = false)
        check("1.e2", Some(success(Token.IntLiteral(1), 0, 0)), checkRemainder = false)
      }
    }

    describe("float") {
      it("basic") {
        check("123.456f", Some(success(Token.FloatLiteral(123.456f), 0, 7)))
        check(".123456F", Some(success(Token.FloatLiteral(0.123456f), 0, 7)))
        check("123_456.789_012f", Some(success(Token.FloatLiteral(123456.789012f), 0, 15)))
      }

      it("zeroes") {
        check("0.0f", Some(success(Token.FloatLiteral(0.0f), 0, 3)))
        check("-0.0F", Some(success(Token.FloatLiteral(-0.0f), 0, 4)))
        check(".0f", Some(success(Token.FloatLiteral(0.0f), 0, 2)))
        check("-.0F", Some(success(Token.FloatLiteral(-0.0f), 0, 3)))
      }

      it("scientific notation") {
        check("1e10f", Some(success(Token.FloatLiteral(1e10f), 0, 4)))
        check("1.2E-5F", Some(success(Token.FloatLiteral(1.2e-5f), 0, 6)))
        check("1.0e1_0f", Some(success(Token.FloatLiteral(1e10f), 0, 7)))
        check("1.0e+1_0F", Some(success(Token.FloatLiteral(1e10f), 0, 8)))
        check("1.0e-1_0f", Some(success(Token.FloatLiteral(1e-10f), 0, 8)))
      }

      it("underscores") {
        check("1_2.3_4f", Some(success(Token.FloatLiteral(12.34f), 0, 7)))
        check("1_2.3_4e1_0F", Some(success(Token.FloatLiteral(12.34e10f), 0, 11)))
        check("1_.2f", Some(failure(IllegalSeparator, 1, 1)))
        check("1.2_f", Some(failure(IllegalSeparator, 3, 3)))
        check("1.2_e10F", Some(failure(IllegalSeparator, 3, 3)))
        check("1e_10f", Some(failure(IllegalSeparator, 2, 2)))
        check("1e10_F", Some(failure(IllegalSeparator, 4, 4)))
      }

      it("precision") {
        check("1e38f", Some(success(Token.FloatLiteral(1e38f), 0, 4)))
        check("3.4028235e38f", Some(success(Token.FloatLiteral(3.4028235e38f), 0, 12)))
        check("1e39f", Some(failure(FloatingPointPrecisionTooLarge, 0, 4)))
        check("1e-38f", Some(success(Token.FloatLiteral(1e-38f), 0, 5)))
        check("1.4e-45f", Some(success(Token.FloatLiteral(1.4e-45f), 0, 7)))
        check("1e-46f", Some(failure(FloatingPointPrecisionTooSmall, 0, 5)))
      }

      it("deceptive decimal points") {
        check("1.f", Some(success(Token.IntLiteral(1), 0, 0)), checkRemainder = false)
        check("1._2f", Some(success(Token.IntLiteral(1), 0, 0)), checkRemainder = false)
      }

    }
  }

  private def check(input: String,
                    expected: Option[Pos[Token]],
                    checkRemainder: Boolean = true)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { (reader, lineMap) =>
      val result = Literals.tryNumericLiteral(reader, buffer)
      withClue(input) {
        result shouldBe expected
      }
    }

    if (checkRemainder) {
      expected match {
        case Some(Pos(Token.Error(_), _, _)) => ()
        case Some(_) =>
          TestReaderFactory.fromString(input + " $") { (reader, lineMap) =>
            val result = Literals.tryNumericLiteral(reader, buffer)
            result shouldBe expected
            reader.get() shouldBe Some(' ')
            reader.get() shouldBe Some('$')
          }
        case _ => ()
      }
    }
  }

}
