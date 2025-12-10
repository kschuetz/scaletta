package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.scanner.ScannerError.IntegerNumberTooLarge
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class NumericLiteralsTest extends AnyFunSpec with Matchers {
  private val buffer = CharBuffer.create()

  describe("numericLiteral") {
    describe("int") {
      it("0") {
        check("0", Some(success(Token.IntLiteral(0), 0, 0)))
      }

      it("-0") {
        check("-0", Some(success(Token.IntLiteral(0), 0, 1)))
      }

      it("000") {
        check("000", Some(success(Token.IntLiteral(0), 0, 2)))
      }

      it("many leading zeroes") {
        check("000000000000000000000000000000000000000000000000000000000000000123",
          Some(success(Token.IntLiteral(123), 0, 65)))
      }
    }

    it("Int.MinValue") {
      check("-2147483648", Some(success(Token.IntLiteral(Int.MinValue), 0, 10)))
    }

    it("Int.MinValue with leading zeroes") {
      check("-00002147483648", Some(success(Token.IntLiteral(Int.MinValue), 0, 14)))
    }

    it("Int.MinValue-1") {
      check("-2147483649", Some(failure(IntegerNumberTooLarge, 0, 10)))
    }

    it("Int.MaxValue") {
      check("2147483647", Some(success(Token.IntLiteral(Int.MaxValue), 0, 9)))
    }

    it("Int.MaxValue with leading zeroes") {
      check("00002147483647", Some(success(Token.IntLiteral(Int.MaxValue), 0, 13)))
    }

    it("Int.MaxValue+1") {
      check("2147483648", Some(failure(IntegerNumberTooLarge, 0, 9)))
    }
  }

  describe("binary") {

    it("int (small)") {
      check("0b1010_1010", Some(success(Token.IntLiteral(170), 0, 10)))
    }

    it("long (small)") {
      check("0b1010_1010L", Some(success(Token.LongLiteral(170), 0, 11)))
    }

    it("large") {
      check("0b1010_1010_1010_1010_1010_1010_1010_1010_1010L", Some(success(Token.LongLiteral(45812984490L), 0, 46)))
    }

    it("negative int (small)") {
      check("-0b1010_1010", Some(success(Token.IntLiteral(-170), 0, 11)))
    }

    it("negative long (small)") {
      check("-0b1010_1010L", Some(success(Token.LongLiteral(-170), 0, 12)))
    }

    it("negative large") {
      check("-0b1010_1010_1010_1010_1010_1010_1010_1010_1010L",
        Some(success(Token.LongLiteral(-45812984490L), 0, 47)))
    }
  }

  describe("hex") {
    it("int (small)") {
      check("0x1234_abcd", Some(success(Token.IntLiteral(0x1234abcd), 0, 10)))
    }

    it("long (small)") {
      check("0x1234_ABCDL", Some(success(Token.LongLiteral(0x1234abcd), 0, 11)))
    }

    it("large") {
      check("0x1234_5678_abcd_EF01l", Some(success(Token.LongLiteral(0x1234_5678_abcd_ef01L), 0, 21)))
    }

    it("negative int (small)") {
      check("-0x1234_AbCd", Some(success(Token.IntLiteral(-0x1234abcd), 0, 11)))
    }

    it("negative long (small)") {
      check("-0x1234_aBcDl", Some(success(Token.LongLiteral(-0x1234abcd), 0, 12)))
    }

    it("negative (large)") {
      check("-0x1234_5678_Abcd_eF01L",
        Some(success(Token.LongLiteral(-0x1234_5678_abcd_ef01L), 0, 22)))
    }
  }

  private def check(input: String, expected: Option[Pos[Either[ScannerError, Token]]]): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      Literals.tryNumericLiteral(reader, buffer) shouldBe expected
    }

    TestReaderFactory.fromString(input + " $") { reader =>
      Literals.tryNumericLiteral(reader, buffer) shouldBe expected
      reader.get() shouldBe Some(' ')
      reader.get() shouldBe Some('$')
    }
  }

}
