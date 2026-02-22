package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}
import software.kes.scaletta.util.CharBuffer

class CharLiteralsTest extends AnyFunSpec with Matchers with AssertExpectedTokens {
  private val buffer = CharBuffer.create()

  describe("charLiteral") {
    it("'a'") {
      check("'a'", Some(success(Token.CharLiteral('a'), 0, 2)))
    }

    it("space") {
      check("' '", Some(success(Token.CharLiteral(' '), 0, 2)))
    }

    it("\\n") {
      check("'\\n'", Some(success(Token.CharLiteral('\n'), 0, 3)))
    }

    it("\\u0041") {
      check("'\\u0041'", Some(success(Token.CharLiteral('A'), 0, 7)))
    }

    it("\\u21d2") {
      check("'\\u21d2'", Some(success(Token.CharLiteral('⇒'), 0, 7)))
    }

    it("⇒") {
      check("'⇒'", Some(success(Token.CharLiteral('⇒'), 0, 2)))
    }

    it("unclosed 1") {
      check("'a", Some(failure(ScannerError.UnclosedCharacterLiteral, 0, 1)))
    }

    it("empty") {
      check("''", Some(failure(ScannerError.EmptyCharacterLiteral, 0, 1)))
    }
  }

  describe("stringLiteral") {
    describe("single-line") {
      it("empty string") {
        check("\"\"", Some(success(Token.StringLiteral(""), 0, 1)))
      }

      it("simple string, no escapes") {
        val input = "\"this is a simple string\""
        check(input, Some(success(Token.StringLiteral("this is a simple string"), 0, 24)))
      }

      it("string with escapes") {
        val input = "\"this \\n string \\t has \\f escapes \\\\ \""
        check(input, Some(success(Token.StringLiteral("this \n string \t has \f escapes \\ "), 0, 37)))
      }

      it("string with escaped quotes") {
        val input = "\"before \\\"quotes\\\" after\""
        check(input, Some(success(Token.StringLiteral("before \"quotes\" after"), 0, 24)))
      }

      it("string with unicode sequences") {
        val input = "\"⇒ is the same as \\u21d2!\""
        check(input, Some(success(Token.StringLiteral("⇒ is the same as ⇒!"), 0, 25)))
      }
    }

    describe("multi-line") {
      it("empty string") {
        check("\"\"\"\"\"\"", Some(success(Token.MultiLineString(""), 0, 5)))
      }

      it("simple string, no new lines, no escapes") {
        val input = "\"\"\"this is a simple string\"\"\""
        check(input, Some(success(Token.MultiLineString("this is a simple string"), 0, 28)))
      }

      it("string with new lines and escapes") {
        val input = "\"\"\"line 1\nline 2\nline 3\\nthis\\tline\\fhas\\\\escapes \"\"\""
        check(input, Some(success(Token.MultiLineString("line 1\nline 2\nline 3\nthis\tline\fhas\\escapes "), 0, 52)))
      }

      it("string with escaped quotes") {
        val input = "\"\"\"before \\\"quotes\\\" after\"\"\""
        check(input, Some(success(Token.MultiLineString("before \"quotes\" after"), 0, 28)))
      }

      it("string with unescaped quotes") {
        val input = "\"\"\"before \"single\" \"\"double\"\" after\"\"\""
        check(input, Some(success(Token.MultiLineString("before \"single\" \"\"double\"\" after"), 0, 37)))
      }

      it("string with unicode sequences") {
        val input = "\"\"\"⇒ is the same as \\u21d2!\"\"\""
        check(input, Some(success(Token.MultiLineString("⇒ is the same as ⇒!"), 0, 29)))
      }
    }

    it("unclosed single-line string") {
      val input = "\"abc"
      check(input, Some(failure(ScannerError.UnclosedStringLiteral, 0, 3)))
    }

    it("unclosed multi-line string") {
      val input = "\"\"\"abc"
      check(input, Some(failure(ScannerError.UnclosedMultiLineString, 0, 5)))
    }
  }

  private def check(input: String,
                    expectedTokens: Option[Pos[Token]]*)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default)
      val actualTokens = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
      assertExpectedTokens(input, expectedTokens.toVector.flatten, actualTokens)
    }
  }
}
