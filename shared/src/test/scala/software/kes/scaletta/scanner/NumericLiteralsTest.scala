package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.scanner.ScannerError.{IllegalSeparator, IntegerNumberTooLarge}
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class NumericLiteralsTest extends AnyFunSpec with Matchers {
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

    describe("binary") {
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
  }

  private def check(input: String, expected: Option[Pos[Either[ScannerError, Token]]]): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      val result = Literals.tryNumericLiteral(reader, buffer)
      withClue(input) {
        result shouldBe expected
      }
    }

    expected match {
      case Some(Pos(Right(_), _, _)) =>
        TestReaderFactory.fromString(input + " $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe expected
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      case _ => ()
    }
  }

}
