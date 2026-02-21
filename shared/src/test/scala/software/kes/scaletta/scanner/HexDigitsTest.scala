package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.TestReaderFactory

class HexDigitsTest extends AnyFunSpec with Matchers {
  describe("HexDigits") {
    describe("digitValue") {
      it("should return correct values for 0-9") {
        ('0' to '9').zipWithIndex.foreach { case (ch, expected) =>
          HexDigits.digitValue(ch) shouldBe expected.toByte
        }
      }

      it("should return correct values for a-f") {
        ('a' to 'f').zipWithIndex.foreach { case (ch, expected) =>
          HexDigits.digitValue(ch) shouldBe (expected + 10).toByte
        }
      }

      it("should return correct values for A-F") {
        ('A' to 'F').zipWithIndex.foreach { case (ch, expected) =>
          HexDigits.digitValue(ch) shouldBe (expected + 10).toByte
        }
      }

      it("should return -1 for invalid characters") {
        HexDigits.digitValue('g') shouldBe -1.toByte
        HexDigits.digitValue('G') shouldBe -1.toByte
        HexDigits.digitValue(' ') shouldBe -1.toByte
        HexDigits.digitValue('\n') shouldBe -1.toByte
      }
    }

    describe("scanOne") {
      it("should scan a valid hex digit and return its value") {
        TestReaderFactory.fromString("a") { reader =>
          HexDigits.scanOne(reader) shouldBe Some(('a', 10.toByte))
          reader.get() shouldBe None
        }
      }

      it("should not consume and return None for invalid hex digit") {
        TestReaderFactory.fromString("g") { reader =>
          HexDigits.scanOne(reader) shouldBe None
          reader.get() shouldBe Some('g')
        }
      }

      it("should return None for empty input") {
        TestReaderFactory.fromString("") { reader =>
          HexDigits.scanOne(reader) shouldBe None
        }
      }
    }

    describe("scanN") {
      it("should scan exactly N hex digits") {
        TestReaderFactory.fromString("abcd") { reader =>
          HexDigits.scanN(4, reader) shouldBe Right(0xabcd)
          reader.get() shouldBe None
        }
      }

      it("should stop after N hex digits even if more follow (lowercase)") {
        TestReaderFactory.fromString("abcdef") { reader =>
          HexDigits.scanN(4, reader) shouldBe Right(0xabcd)
          reader.get() shouldBe Some('e')
          reader.get() shouldBe Some('f')
        }
      }

      it("should stop after N hex digits even if more follow (mixed case)") {
        TestReaderFactory.fromString("AbCdEf") { reader =>
          HexDigits.scanN(4, reader) shouldBe Right(0xabcd)
          reader.get() shouldBe Some('E')
          reader.get() shouldBe Some('f')
        }
      }

      it("should backtrack and return Left(Some(ch)) if N digits are not available") {
        TestReaderFactory.fromString("abc") { reader =>
          HexDigits.scanN(4, reader) shouldBe Left(None) // EOF reached
          reader.get() shouldBe Some('a')
          reader.get() shouldBe Some('b')
          reader.get() shouldBe Some('c')
        }
      }

      it("should backtrack if an invalid character is encountered before N digits") {
        TestReaderFactory.fromString("ab g") { reader =>
          HexDigits.scanN(4, reader) shouldBe Left(Some(' '))
          reader.get() shouldBe Some('a')
          reader.get() shouldBe Some('b')
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('g')
        }
      }

      it("should backtrack if EOF is encountered before N digits") {
        TestReaderFactory.fromString("ab") { reader =>
          HexDigits.scanN(4, reader) shouldBe Left(None)
          reader.get() shouldBe Some('a')
          reader.get() shouldBe Some('b')
          reader.get() shouldBe None
        }
      }

      it("should handle large N correctly") {
        TestReaderFactory.fromString("12345678") { reader =>
          HexDigits.scanN(8, reader) shouldBe Right(0x12345678)
        }
      }
    }
  }
}
