package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.scanner.ScannerError._
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory

class StringInterpolatorsTest extends AnyFunSpec with Matchers {
  describe("string interpolators") {
    it("simple variable interpolation") {
      check("s\"hello $name\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)),
        Some(success(Token.InterpolatedPart("hello "), 2, 7)),
        Some(success(Token.Identifier.Lower("name"), 9, 12)),
        Some(success(Token.EndInterpolatedString, 13, 13))
      )
    }

    it("expression interpolation with braces") {
      check("s\"value: ${foo.bar}\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)),
        Some(success(Token.InterpolatedPart("value: "), 2, 8)),
        Some(success(Token.BeginInterpolatedEscape, 9, 10)),
        Some(success(Token.Identifier.Lower("foo"), 11, 13)),
        Some(success(Token.Dot, 14, 14)),
        Some(success(Token.Identifier.Lower("bar"), 15, 17)),
        Some(success(Token.EndInterpolatedEscape, 18, 18)),
        Some(success(Token.EndInterpolatedString, 19, 19))
      )
    }

    it("escaped dollar signs") {
      check("s\"price: $$100\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)),
        Some(success(Token.InterpolatedPart("price: $100"), 2, 13)),
        Some(success(Token.EndInterpolatedString, 14, 14))
      )
    }

    it("multiple interpolations") {
      check("s\"$a $b\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)),
        Some(success(Token.Identifier.Lower("a"), 3, 3)),
        Some(success(Token.InterpolatedPart(" "), 4, 4)),
        Some(success(Token.Identifier.Lower("b"), 6, 6)),
        Some(success(Token.EndInterpolatedString, 7, 7))
      )
    }

    describe("multi-line interpolators") {
      it("containing multiple lines and an identifier") {
        check("s\"\"\"line 1\n$name\"\"\"",
          Some(success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3)),
          Some(success(Token.InterpolatedPart("line 1\n"), 4, 10)),
          Some(success(Token.Identifier.Lower("name"), 12, 15)),
          Some(success(Token.EndInterpolatedString, 16, 18))
        )
      }

      it("containing multiple lines and no identifier") {
        check("s\"\"\"line 1\nline 2\"\"\"",
          Some(success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3)),
          Some(success(Token.InterpolatedPart("line 1\nline 2"), 4, 16)),
          Some(success(Token.EndInterpolatedString, 17, 19))
        )
      }

      it("containing one line and an identifier") {
        check("s\"\"\"line 1 $name\"\"\"",
          Some(success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3)),
          Some(success(Token.InterpolatedPart("line 1 "), 4, 10)),
          Some(success(Token.Identifier.Lower("name"), 12, 15)),
          Some(success(Token.EndInterpolatedString, 16, 18))
        )
      }

      it("containing one line and no identifier") {
        check("s\"\"\"line 1\"\"\"",
          Some(success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3)),
          Some(success(Token.InterpolatedPart("line 1"), 4, 9)),
          Some(success(Token.EndInterpolatedString, 10, 12))
        )
      }
    }

    it("raw interpolator") {
      check("raw\"\\n $foo\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("raw")), 0, 3)),
        Some(success(Token.InterpolatedPart("\\n "), 4, 6)),
        Some(success(Token.Identifier.Lower("foo"), 8, 10)),
        Some(success(Token.EndInterpolatedString, 11, 11))
      )
    }

    it("f interpolator") {
      check("f\"$name%s\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("f")), 0, 1)),
        Some(success(Token.Identifier.Lower("name"), 3, 6)),
        Some(success(Token.InterpolatedPart("%s"), 7, 8)),
        Some(success(Token.EndInterpolatedString, 9, 9))
      )
    }

    it("unclosed interpolator") {
      check("s\"hello $name",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)),
        Some(success(Token.InterpolatedPart("hello "), 2, 7)),
        Some(success(Token.Identifier.Lower("name"), 9, 12)),
        Some(failure(UnclosedStringLiteral, 13, 13))
      )
    }

    it("invalid escape character in part") {
      // Assuming interpolator parts follow standard string escape rules unless 'raw'
      check("s\"hello \\z\"",
        Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)),
        Some(failure(InvalidEscapeCharacter, 9, 9))
      )
    }
  }

  // Note: This helper is simplified. In the actual implementation, we might need a 
  // more complex loop to handle the stateful scanning of interpolators.
  private def check(input: String,
                    expectedTokens: Option[Pos[Token]]*)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default)
      expectedTokens.foreach { expected =>
        val actual = scanner.get()
        expected match {
          case Some(expectedPos) =>
            actual.value match {
              case Token.EndOfInput =>
                fail("Expected more tokens, but got EndOfInput")
              case _ =>
                actual.value shouldBe expectedPos.value
                actual.begin shouldBe expectedPos.begin
                actual.end shouldBe expectedPos.end
            }
          case None =>
            actual.value shouldBe Token.EndOfInput
        }
      }
    }
  }
}
