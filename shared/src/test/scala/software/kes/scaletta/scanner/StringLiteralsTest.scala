package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.LineEndingInterpolators._
import software.kes.scaletta.testsupport.ScannerTestHelpers.success
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class StringLiteralsTest extends AnyFunSpec with Matchers {
  private val buffer = CharBuffer.create()

  describe("stringLiteral") {
    describe("single-line") {
      it("empty string") {
        TestReaderFactory.fromString("\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.StringLiteral(""), -1, 0)
          reader.get() shouldBe None
        }
      }

      it("simple string, no escapes") {
        TestReaderFactory.fromString("this is a simple string\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.StringLiteral("this is a simple string"), -1, 23)
          reader.get() shouldBe None
        }
      }

      it("string with escapes") {
        TestReaderFactory.fromString(raw"this \n string \t has \f escapes \\ " + "\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("this \n string \t has \f escapes \\ "), -1, 36)
          reader.get() shouldBe None
        }
      }

      it("string with escaped quotes") {
        TestReaderFactory.fromString("before \\\"quotes\\\" after\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("before \"quotes\" after"), -1, 23)
          reader.get() shouldBe None
        }
      }

      it("string with unicode sequences") {
        TestReaderFactory.fromString("⇒ is the same as \\u21d2!\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("⇒ is the same as ⇒!"), -1, 24)
          reader.get() shouldBe None
        }
      }
    }

    describe("multi-line") {
      it("empty string") {
        TestReaderFactory.fromString("\"\"\"\"\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.MultiLineString(""), -1, 4)
          reader.get() shouldBe None
        }
      }

      it("simple string, no new lines, no escapes") {
        TestReaderFactory.fromString("\"\"this is a simple string\"\"\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.MultiLineString("this is a simple string"), -1, 27)
          reader.get() shouldBe None
        }
      }

      it("string with new lines and escapes") {
        TestReaderFactory.fromString(lf"\"\"line 1\nline 2\nline 3\\nthis\\tline\\fhas\\\\escapes \"\"\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("line 1\nline 2\nline 3\nthis\tline\fhas\\escapes "), -1, 51)
          reader.get() shouldBe None
        }
      }

      it("string with escaped quotes") {
        TestReaderFactory.fromString("\"\"before \\\"quotes\\\" after\"\"\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("before \"quotes\" after"), -1, 27)
          reader.get() shouldBe None
        }
      }

      it("string with unescaped quotes") {
        TestReaderFactory.fromString("\"\"before \"single\" \"\"double\"\" after\"\"\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("before \"single\" \"\"double\"\" after"), -1, 36)
          reader.get() shouldBe None
        }
      }

      it("string with unicode sequences") {
        TestReaderFactory.fromString("\"\"⇒ is the same as \\u21d2!\"\"\"") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("⇒ is the same as ⇒!"), -1, 28)
          reader.get() shouldBe None
        }
      }

      it("mixed line endings and LineMap accuracy") {
        val input = "\"\"line 1\r\nline 2\rline 3\n\"\"\""
        TestReaderFactory.fromString(input) { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("line 1\nline 2\nline 3\n"), -1, 26)

          val lineMap = reader.lineMap
          lineMap.indexToPosition(CharIndex(0)).line shouldBe LineIndex(0)
          lineMap.indexToPosition(CharIndex(10)).line.value shouldBe 1 // After \r\n (8, 9)
          lineMap.indexToPosition(CharIndex(17)).line.value shouldBe 2 // After \r (16)
          lineMap.indexToPosition(CharIndex(24)).line.value shouldBe 3 // After \n (23)
          lineMap.indexToPosition(CharIndex(27)).line.value shouldBe 3 // After """ - it's still on line 3!
        }
      }
    }
  }
}
