package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}
import software.kes.scaletta.util.CharBuffer

class CharLiteralsSpec extends AnyFunSpec with Matchers with AssertExpectedTokens {
  private val buffer = CharBuffer.create()

  describe("charLiteral") {
    it("'a'") {
      check("'a'", success(Token.CharLiteral('a'), 0, 2))
    }

    it("simple char (no start quote)") {
      // Direct call to Literals.charLiteral usually assumes the opening ' is already consumed.
      // But CharLiteralsTest.check uses Scanner, so we use full literals.
      check("'a'", success(Token.CharLiteral('a'), 0, 2))
    }

    it("space") {
      check("' '", success(Token.CharLiteral(' '), 0, 2))
    }

    it("\\n") {
      check("'\\n'", success(Token.CharLiteral('\n'), 0, 3))
    }

    it("\\u0041") {
      check("'\\u0041'", success(Token.CharLiteral('A'), 0, 7))
    }

    it("\\u21d2") {
      check("'\\u21d2'", success(Token.CharLiteral('⇒'), 0, 7))
    }

    it("escape sequence (\\n)") {
      check("'\\n'", success(Token.CharLiteral('\n'), 0, 3))
    }

    it("unicode escape sequence (\\u21d2)") {
      check("'\\u21d2'", success(Token.CharLiteral('⇒'), 0, 7))
    }

    it("⇒") {
      check("'⇒'", success(Token.CharLiteral('⇒'), 0, 2))
    }

    it("unclosed 1") {
      check("'a", failure(ScanError.UnclosedCharacterLiteral, 0, 1))
    }

    it("unclosed char literal (no end quote)") {
      check("'a", failure(ScanError.UnclosedCharacterLiteral, 0, 1))
    }

    it("empty") {
      check("''", failure(ScanError.EmptyCharacterLiteral, 0, 1))
    }

    it("invalid escape sequence") {
      check("'\\z'", failure(ScanError.InvalidEscapeCharacter, 1, 1), success(Token.Identifier.Lower("z"), 2, 2), failure(ScanError.UnclosedCharacterLiteral, 3, 3))
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
