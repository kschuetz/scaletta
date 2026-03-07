package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.reporting.{CharIndex, LineIndex, Pos}
import software.kes.scaletta.scanner.ScanError._
import software.kes.scaletta.testsupport.LineEndingInterpolators._
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}

class StringInterpolatorsTest extends AnyFunSpec with Matchers with AssertExpectedTokens {
  describe("string interpolators") {
    it("simple variable interpolation") {
      check("s\"hello $name\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        success(Token.InterpolatedPart("hello "), 2, 7),
        success(Token.Identifier.Lower("name"), 9, 12),
        success(Token.EndInterpolatedString, 13, 13)
      )
    }

    it("expression interpolation with braces") {
      check("s\"value: ${foo.bar}\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        success(Token.InterpolatedPart("value: "), 2, 8),
        success(Token.BeginInterpolatedEscape, 9, 10),
        success(Token.Identifier.Lower("foo"), 11, 13),
        success(Token.Dot, 14, 14),
        success(Token.Identifier.Lower("bar"), 15, 17),
        success(Token.EndInterpolatedEscape, 18, 18),
        success(Token.EndInterpolatedString, 19, 19)
      )
    }

    it("escaped dollar signs") {
      check("s\"price: $$100\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        success(Token.InterpolatedPart("price: $100"), 2, 13),
        success(Token.EndInterpolatedString, 14, 14)
      )
    }

    it("multiple interpolations") {
      check("s\"$a $b\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        success(Token.Identifier.Lower("a"), 3, 3),
        success(Token.InterpolatedPart(" "), 4, 4),
        success(Token.Identifier.Lower("b"), 6, 6),
        success(Token.EndInterpolatedString, 7, 7)
      )
    }

    describe("multi-line interpolators") {
      it("containing multiple lines and an identifier") {
        check(lf"s\"\"\"line 1\n$$name\"\"\"",
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
          success(Token.InterpolatedPart("line 1\n"), 4, 10),
          success(Token.Identifier.Lower("name"), 12, 15),
          success(Token.EndInterpolatedString, 16, 18)
        )
      }

      it("containing multiple lines and no identifier") {
        check(lf"s\"\"\"line 1\nline 2\"\"\"",
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
          success(Token.InterpolatedPart("line 1\nline 2"), 4, 16),
          success(Token.EndInterpolatedString, 17, 19)
        )
      }

      it("containing one line and an identifier") {
        check("s\"\"\"line 1 $name\"\"\"",
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
          success(Token.InterpolatedPart("line 1 "), 4, 10),
          success(Token.Identifier.Lower("name"), 12, 15),
          success(Token.EndInterpolatedString, 16, 18)
        )
      }

      it("containing one line and no identifier") {
        check("s\"\"\"line 1\"\"\"",
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
          success(Token.InterpolatedPart("line 1"), 4, 9),
          success(Token.EndInterpolatedString, 10, 12)
        )
      }
    }

    describe("newline normalization and LineMap accuracy") {
      it("mixed line endings in multi-line interpolated strings") {
        val input = "s\"\"\"line 1\r\nline 2\rline 3\n\"\"\""
        // Raw characters:
        // 0-3: s"""
        // 4-9: line 1
        // 10: \r
        // 11: \n
        // 12-17: line 2
        // 18: \r
        // 19-24: line 3
        // 25: \n
        // 26-28: """

        TestReaderFactory.fromString(input) { (reader, lineMap) =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)

          val first = scanner.get()
          first.value shouldBe Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s"))

          val second = scanner.get()
          second.value shouldBe Token.InterpolatedPart("line 1\nline 2\nline 3\n")

          val third = scanner.get()
          third.value shouldBe Token.EndInterpolatedString

          // Verify LineMap
          val lineMap = reader.lineMap
          lineMap.indexToPosition(CharIndex(0)).line shouldBe LineIndex(1)
          lineMap.indexToPosition(CharIndex(12)).line.value shouldBe 2 // After \r\n (10, 11)
          lineMap.indexToPosition(CharIndex(19)).line.value shouldBe 3 // After \r (18)
          lineMap.indexToPosition(CharIndex(26)).line.value shouldBe 4 // After \n (25)
          lineMap.indexToPosition(CharIndex(29)).line.value shouldBe 4 // After """
        }
      }
    }

    it("raw interpolator") {
      check("raw\"\\n $foo\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("raw")), 0, 3),
        success(Token.InterpolatedPart("\\n "), 4, 6),
        success(Token.Identifier.Lower("foo"), 8, 10),
        success(Token.EndInterpolatedString, 11, 11)
      )
    }

    it("f interpolator") {
      check("f\"$name%s\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("f")), 0, 1),
        success(Token.Identifier.Lower("name"), 3, 6),
        success(Token.InterpolatedPart("%s"), 7, 8),
        success(Token.EndInterpolatedString, 9, 9)
      )
    }

    it("unclosed interpolator") {
      check("s\"hello $name",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        success(Token.InterpolatedPart("hello "), 2, 7),
        success(Token.Identifier.Lower("name"), 9, 12),
        failure(UnclosedStringLiteral, 13, 13)
      )
    }

    it("invalid escape character in part") {
      // Similar to Scala's behavior: Discard the prefix and return only the fatal error.
      check("s\"hello \\z\"",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        failure(InvalidEscapeCharacter, 8, 8)
      )
    }

    it("invalid escape character in multiline part") {
      check("s\"\"\"hello \\z\"\"\"",
        success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
        failure(InvalidEscapeCharacter, 10, 10)
      )
    }

    it("unclosed string with invalid escape character") {
      check("s\"hello \\z",
        success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
        failure(UnclosedStringLiteral, 2, 10)
      )
    }

    it("unclosed multiline string with invalid escape character") {
      check("s\"\"\"hello \\z",
        success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
        failure(UnclosedMultiLineString, 4, 12)
      )
    }

    it("invalid escape character in multiline part with single quotes before closing") {
      check("s\"\"\"hello \\z \" still in string \"\"\"",
        success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
        failure(InvalidEscapeCharacter, 10, 10)
      )
    }

    describe("saturation") {
      it("saturates on EndOfInput after an unclosed interpolated string literal error") {
        val input = "s\""
        TestReaderFactory.fromString(input) { (reader, lineMap) =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)

          // First call: BeginInterpolatedString
          val first = scanner.get()
          first.value shouldBe Token.BeginInterpolatedString(Interpolator.fromName("s"))

          // Second call: Error
          val second = scanner.get()
          second.value shouldBe Token.Error(ScanError.UnclosedStringLiteral)

          // Third call: Should be EndOfInput, NOT the same error again
          val third = scanner.get()
          if (third.value == Token.Error(ScanError.UnclosedStringLiteral)) {
            fail("Scanner saturated on the Error token instead of transitioning to EndOfInput")
          }
          third.value shouldBe Token.EndOfInput
        }
      }
    }
  }

  private def check(input: String,
                    expected: Pos[Token]*)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { (reader, lineMap) =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default)
      val actualTokens = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
      val expectedTokens = expected.toVector

      assertExpectedTokens(input, lineMap, expectedTokens, actualTokens)
    }
  }
}
