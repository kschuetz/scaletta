package software.kes.scaletta.internal.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.line
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.testsupport.LineEndingInterpolators._
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}
import software.kes.scaletta.util.CharBuffer

class StringLiteralsSpec extends AnyFunSpec with Matchers with AssertExpectedTokens {
  private val buffer = CharBuffer.create()

  describe("stringLiteral") {
    describe("single-line") {
      it("empty string") {
        TestReaderFactory.fromString("\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.StringLiteral(""), -1, 0)
          reader.get() shouldBe None
        }
      }

      it("simple string, no escapes") {
        TestReaderFactory.fromString("this is a simple string\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.StringLiteral("this is a simple string"), -1, 23)
          reader.get() shouldBe None
        }
      }

      it("string with escapes") {
        TestReaderFactory.fromString(raw"this \n string \t has \f escapes \\ " + "\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("this \n string \t has \f escapes \\ "), -1, 36)
          reader.get() shouldBe None
        }
      }

      it("string with escaped quotes") {
        TestReaderFactory.fromString("before \\\"quotes\\\" after\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("before \"quotes\" after"), -1, 23)
          reader.get() shouldBe None
        }
      }

      it("string with unicode sequences") {
        TestReaderFactory.fromString("⇒ is the same as \\u21d2!\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("⇒ is the same as ⇒!"), -1, 24)
          reader.get() shouldBe None
        }
      }

      it("invalid escape sequence") {
        TestReaderFactory.fromString("invalid \\z escape\"") { (reader, lineMap) =>
          val result = Literals.stringLiteral(reader, buffer)
          result.value shouldBe Token.Error(ScanError.InvalidEscapeCharacter)
          result.begin.value shouldBe 8
        }
      }

      it("incomplete unicode escape") {
        TestReaderFactory.fromString("bad \\u12 escape\"") { (reader, lineMap) =>
          val result = Literals.stringLiteral(reader, buffer)
          result.value shouldBe Token.Error(ScanError.InvalidEscapeCharacter)
          result.begin.value shouldBe 4
        }
      }

      it("unclosed string literal") {
        TestReaderFactory.fromString("this string does not end") { (reader, lineMap) =>
          val result = Literals.stringLiteral(reader, buffer)
          result.value shouldBe Token.Error(ScanError.UnclosedStringLiteral)
          result.begin.value shouldBe -1
          result.end.value shouldBe 23
        }
      }

      it("unclosed multi-line string literal") {
        TestReaderFactory.fromString("\"\"this multi-line string does not end") { (reader, lineMap) =>
          val result = Literals.stringLiteral(reader, buffer)
          result.value shouldBe Token.Error(ScanError.UnclosedMultiLineString)
          result.begin.value shouldBe -1
          result.end.value shouldBe 36
        }
      }
    }

    describe("multi-line") {
      it("empty string") {
        TestReaderFactory.fromString("\"\"\"\"\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.MultiLineString(""), -1, 4)
          reader.get() shouldBe None
        }
      }

      it("simple string, no new lines, no escapes") {
        TestReaderFactory.fromString("\"\"this is a simple string\"\"\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.MultiLineString("this is a simple string"), -1, 27)
          reader.get() shouldBe None
        }
      }

      it("string with new lines and escapes") {
        TestReaderFactory.fromString(lf"\"\"line 1\nline 2\nline 3\\nthis\\tline\\fhas\\\\escapes \"\"\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("line 1\nline 2\nline 3\nthis\tline\fhas\\escapes "), -1, 51)
          reader.get() shouldBe None
        }
      }

      it("string with escaped quotes") {
        TestReaderFactory.fromString("\"\"before \\\"quotes\\\" after\"\"\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("before \"quotes\" after"), -1, 27)
          reader.get() shouldBe None
        }
      }

      it("string with unescaped quotes") {
        TestReaderFactory.fromString("\"\"before \"single\" \"\"double\"\" after\"\"\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("before \"single\" \"\"double\"\" after"), -1, 36)
          reader.get() shouldBe None
        }
      }

      it("string with unicode sequences") {
        TestReaderFactory.fromString("\"\"⇒ is the same as \\u21d2!\"\"\"") { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("⇒ is the same as ⇒!"), -1, 28)
          reader.get() shouldBe None
        }
      }

      it("mixed line endings and LineMap accuracy") {
        val input = "\"\"line 1\r\nline 2\rline 3\n\"\"\""
        TestReaderFactory.fromString(input) { (reader, lineMap) =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("line 1\nline 2\nline 3\n"), -1, 26)

          val lineMap = reader.lineMap
          lineMap.indexToPosition(CharIndex(0)).line shouldBe line(1)
          lineMap.indexToPosition(CharIndex(10)).line.value shouldBe 2 // After \r\n (8, 9)
          lineMap.indexToPosition(CharIndex(17)).line.value shouldBe 3 // After \r (16)
          lineMap.indexToPosition(CharIndex(24)).line.value shouldBe 4 // After \n (23)
          lineMap.indexToPosition(CharIndex(27)).line.value shouldBe 4 // After """ - it's still on line 4!
        }
      }
    }

    describe("Scanner-based tests") {
      describe("single-line") {
        it("empty string") {
          check("\"\"", success(Token.StringLiteral(""), 0, 1))
        }

        it("simple string, no escapes") {
          val input = "\"this is a simple string\""
          check(input, success(Token.StringLiteral("this is a simple string"), 0, 24))
        }

        it("string with escapes") {
          val input = "\"this \\n string \\t has \\f escapes \\\\ \""
          check(input, success(Token.StringLiteral("this \n string \t has \f escapes \\ "), 0, 37))
        }

        it("string with escaped quotes") {
          val input = "\"before \\\"quotes\\\" after\""
          check(input, success(Token.StringLiteral("before \"quotes\" after"), 0, 24))
        }

        it("string with unicode sequences") {
          val input = "\"⇒ is the same as \\u21d2!\""
          check(input, success(Token.StringLiteral("⇒ is the same as ⇒!"), 0, 25))
        }
      }

      describe("multi-line") {
        it("empty string") {
          check("\"\"\"\"\"\"", success(Token.MultiLineString(""), 0, 5))
        }

        it("simple string, no new lines, no escapes") {
          val input = "\"\"\"this is a simple string\"\"\""
          check(input, success(Token.MultiLineString("this is a simple string"), 0, 28))
        }

        it("string with new lines and escapes") {
          val input = "\"\"\"line 1\nline 2\nline 3\\nthis\\tline\\fhas\\\\escapes \"\"\""
          check(input, success(Token.MultiLineString("line 1\nline 2\nline 3\nthis\tline\fhas\\escapes "), 0, 52))
        }

        it("string with escaped quotes") {
          val input = "\"\"\"before \\\"quotes\\\" after\"\"\""
          check(input, success(Token.MultiLineString("before \"quotes\" after"), 0, 28))
        }

        it("string with unescaped quotes") {
          val input = "\"\"\"before \"single\" \"\"double\"\" after\"\"\""
          check(input, success(Token.MultiLineString("before \"single\" \"\"double\"\" after"), 0, 37))
        }

        it("string with unicode sequences") {
          val input = "\"\"\"⇒ is the same as \\u21d2!\"\"\""
          check(input, success(Token.MultiLineString("⇒ is the same as ⇒!"), 0, 29))
        }
      }

      it("unclosed single-line string") {
        val input = "\"abc"
        check(input, failure(ScanError.UnclosedStringLiteral, 0, 3))
      }

      it("unclosed multi-line string") {
        val input = "\"\"\"abc"
        check(input, failure(ScanError.UnclosedMultiLineString, 0, 5))
      }
    }
  }

  private def check(input: String,
                    expectedTokens: Pos[Token]*)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { (reader, lineMap) =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default)
      val actualTokens = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
      assertExpectedTokens(input, lineMap, expectedTokens.toVector, actualTokens)
    }
  }
}
