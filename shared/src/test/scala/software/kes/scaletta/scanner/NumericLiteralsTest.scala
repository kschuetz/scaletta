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
        TestReaderFactory.fromString(" $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(0), -1, -1)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("-0") {
        TestReaderFactory.fromString(" $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(0), -2, -1)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("000") {
        TestReaderFactory.fromString("00 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(0), -1, 1)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("many leading zeroes") {
        TestReaderFactory.fromString("00000000000000000000000000000000000000000000000000000000000000123 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(123), -1, 64)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MinValue") {
        TestReaderFactory.fromString("147483648 $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '2', reader, buffer) shouldBe
            success(Token.IntLiteral(Int.MinValue), -2, 8)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MinValue with leading zeroes") {
        TestReaderFactory.fromString("0002147483648 $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(Int.MinValue), -2, 12)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MinValue-1") {
        TestReaderFactory.fromString("147483649 $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '2', reader, buffer) shouldBe
            failure(IntegerNumberTooLarge, -2, 8)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MaxValue") {
        TestReaderFactory.fromString("147483647 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '2', reader, buffer) shouldBe
            success(Token.IntLiteral(Int.MaxValue), -1, 8)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MaxValue with leading zeroes") {
        TestReaderFactory.fromString("0002147483647 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(Int.MaxValue), -1, 12)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("Int.MaxValue+1") {
        TestReaderFactory.fromString("147483648 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '2', reader, buffer) shouldBe
            failure(IntegerNumberTooLarge, -1, 8)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }

    describe("binary") {

      it("int (small)") {
        TestReaderFactory.fromString("b1010_1010 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(170), -1, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("long (small)") {
        TestReaderFactory.fromString("b1010_1010L $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(170), -1, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("large") {
        TestReaderFactory.fromString("b1010_1010_1010_1010_1010_1010_1010_1010_1010L $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(45812984490L), -1, 45)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative int (small)") {
        TestReaderFactory.fromString("b1010_1010 $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(-170), -2, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative long (small)") {
        TestReaderFactory.fromString("b1010_1010L $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-170), -2, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative large)") {
        TestReaderFactory.fromString("b1010_1010_1010_1010_1010_1010_1010_1010_1010L $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-45812984490L), -2, 45)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }

    describe("hex") {
      it("int (small)") {
        TestReaderFactory.fromString("x1234_abcd $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(0x1234abcd), -1, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("long (small)") {
        TestReaderFactory.fromString("x1234_ABCDL $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(0x1234abcd), -1, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("large") {
        TestReaderFactory.fromString("x1234_5678_abcd_EF01l $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(0x1234_5678_abcd_ef01L), -1, 20)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative int (small)") {
        TestReaderFactory.fromString("x1234_AbCd $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(-0x1234abcd), -2, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative long (small)") {
        TestReaderFactory.fromString("x1234_aBcDl $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-0x1234abcd), -2, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative (large)") {
        TestReaderFactory.fromString("x1234_5678_Abcd_eF01L $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-0x1234_5678_abcd_ef01L), -2, 20)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }
  }

}
