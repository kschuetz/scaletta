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
        TestReaderFactory.fromString("0 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(0), 0, 0))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("-0") {
        TestReaderFactory.fromString("-0 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(0), 0, 1))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("000") {
        TestReaderFactory.fromString("000 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(0), 0, 2))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("many leading zeroes") {
        TestReaderFactory.fromString("000000000000000000000000000000000000000000000000000000000000000123 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(123), 0, 65))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MinValue") {
        TestReaderFactory.fromString("-2147483648 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(Int.MinValue), 0, 10))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MinValue with leading zeroes") {
        TestReaderFactory.fromString("-00002147483648 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(Int.MinValue), 0, 14))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MinValue-1") {
        TestReaderFactory.fromString("-2147483649 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(failure(IntegerNumberTooLarge, 0, 10))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MaxValue") {
        TestReaderFactory.fromString("2147483647 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(Int.MaxValue), 0, 9))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MaxValue with leading zeroes") {
        TestReaderFactory.fromString("00002147483647 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(Int.MaxValue), 0, 13))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MaxValue+1") {
        TestReaderFactory.fromString("2147483648 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(failure(IntegerNumberTooLarge, 0, 9))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }

    describe("binary") {

      it("int (small)") {
        TestReaderFactory.fromString("0b1010_1010 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(170), 0, 10))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("long (small)") {
        TestReaderFactory.fromString("0b1010_1010L $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(170), 0, 11))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("large") {
        TestReaderFactory.fromString("0b1010_1010_1010_1010_1010_1010_1010_1010_1010L $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(45812984490L), 0, 46))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative int (small)") {
        TestReaderFactory.fromString("-0b1010_1010 $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(-170), 0, 11))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative long (small)") {
        TestReaderFactory.fromString("-0b1010_1010L $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(-170), 0, 12))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative large") {
        TestReaderFactory.fromString("-0b1010_1010_1010_1010_1010_1010_1010_1010_1010L $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(-45812984490L), 0, 47))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }

    describe("hex") {
      it("int (small)") {
        TestReaderFactory.fromString("0x1234_abcd $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(0x1234abcd), 0, 10))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("long (small)") {
        TestReaderFactory.fromString("0x1234_ABCDL $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(0x1234abcd), 0, 11))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("large") {
        TestReaderFactory.fromString("0x1234_5678_abcd_EF01l $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(0x1234_5678_abcd_ef01L), 0, 21))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative int (small)") {
        TestReaderFactory.fromString("-0x1234_AbCd $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.IntLiteral(-0x1234abcd), 0, 11))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative long (small)") {
        TestReaderFactory.fromString("-0x1234_aBcDl $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(-0x1234abcd), 0, 12))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative (large)") {
        TestReaderFactory.fromString("-0x1234_5678_Abcd_eF01L $") { reader =>
          Literals.tryNumericLiteral(reader, buffer) shouldBe
            Some(success(Token.LongLiteral(-0x1234_5678_abcd_ef01L), 0, 22))
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }
  }

}
