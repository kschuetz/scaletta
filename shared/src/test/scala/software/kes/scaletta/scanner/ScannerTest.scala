package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory

class ScannerTest extends AnyFunSpec with Matchers {
  describe("Scanner") {
    describe("whitespace and comments") {
      it("should skip whitespace") {
        check("  \t\r\n  123",
          Some(success(Token.IntLiteral(123), 7, 9))
        )
      }

      it("should skip line comments") {
        check("// comment\n123",
          Some(success(Token.IntLiteral(123), 11, 13))
        )
      }

      it("should skip block comments") {
        check("/* comment */123",
          Some(success(Token.IntLiteral(123), 13, 15))
        )
      }

      it("should skip nested block comments") {
        check("/* outer /* inner */ outer */123",
          Some(success(Token.IntLiteral(123), 29, 31))
        )
      }

      it("should skip multiple comments and whitespace") {
        check("  // line\n  /* block */  123",
          Some(success(Token.IntLiteral(123), 25, 27))
        )
      }

      it("should handle unterminated block comments") {
        check("/* comment",
          Some(failure(ScannerError.UnclosedComment, 0, 10))
        )
      }
    }

    describe("numeric literals") {
      it("integers") {
        check("123", Some(success(Token.IntLiteral(123), 0, 2)))
        check("0", Some(success(Token.IntLiteral(0), 0, 0)))
        check("-123", Some(success(Token.IntLiteral(-123), 0, 3)))
      }

      it("longs") {
        check("123L", Some(success(Token.LongLiteral(123L), 0, 3)))
        check("-123l", Some(success(Token.LongLiteral(-123L), 0, 4)))
      }

      it("hex literals") {
        check("0x123", Some(success(Token.IntLiteral(0x123), 0, 4)))
        check("0XABC", Some(success(Token.IntLiteral(0xABC), 0, 4)))
        check("0x123L", Some(success(Token.LongLiteral(0x123L), 0, 5)))
      }

      it("binary literals") {
        check("0b101", Some(success(Token.IntLiteral(5), 0, 4)))
        check("0B110L", Some(success(Token.LongLiteral(6L), 0, 5)))
      }

      it("floating point literals") {
        check("1.23", Some(success(Token.DoubleLiteral(1.23), 0, 3)))
        check(".123", Some(success(Token.DoubleLiteral(0.123), 0, 3)))
        check("1.23f", Some(success(Token.FloatLiteral(1.23f), 0, 4)))
        check("1.23D", Some(success(Token.DoubleLiteral(1.23), 0, 4)))
        check("1e10", Some(success(Token.DoubleLiteral(1e10), 0, 3)))
        check("1.23e-4", Some(success(Token.DoubleLiteral(1.23e-4), 0, 6)))
        check("-1.23", Some(success(Token.DoubleLiteral(-1.23), 0, 4)))
        check("-.123", Some(success(Token.DoubleLiteral(-0.123), 0, 4)))
      }

      it("underscores in numeric literals") {
        check("1_000", Some(success(Token.IntLiteral(1000), 0, 4)))
        check("0x12_34", Some(success(Token.IntLiteral(0x1234), 0, 6)))
      }
    }

    describe("delimiters and operators") {
      it("should recognize basic delimiters") {
        check("().,;",
          Some(success(Token.LParen, 0, 0)),
          Some(success(Token.RParen, 1, 1)),
          Some(success(Token.Dot, 2, 2)),
          Some(success(Token.Comma, 3, 3)),
          Some(success(Token.Semicolon, 4, 4))
        )
      }

      it("should recognize operators") {
        check("+ - * /",
          Some(success(Token.Identifier.Operator("+"), 0, 0)),
          Some(success(Token.Identifier.Operator("-"), 2, 2)),
          Some(success(Token.Identifier.Operator("*"), 4, 4)),
          Some(success(Token.Identifier.Operator("/"), 6, 6))
        )
      }
    }

    describe("identifiers and reserved words") {
      it("should recognize identifiers") {
        check("foo barBaz",
          Some(success(Token.Identifier.Lower("foo"), 0, 2)),
          Some(success(Token.Identifier.Lower("barBaz"), 4, 9))
        )
      }

      it("should recognize reserved words") {
        check("if else val def then case",
          Some(success(Token.If, 0, 1)),
          Some(success(Token.Else, 3, 6)),
          Some(success(Token.Val, 8, 10)),
          Some(success(Token.Def, 12, 14)),
          Some(success(Token.Then, 16, 19)),
          Some(success(Token.Case, 21, 24))
        )
      }
    }

    describe("interpolated strings") {
      it("simple variable interpolation") {
        check("s\"hello $name\"",
          Some(success(Token.BeginInterpolatedString("s"), 0, 1)),
          Some(success(Token.InterpolatedPart("hello "), 2, 7)),
          Some(success(Token.Identifier.Lower("name"), 9, 12)),
          Some(success(Token.EndInterpolatedString, 13, 13))
        )
      }

      it("nested interpolations") {
        check("s\"outer ${s\"inner $name\"}\"",
          Some(success(Token.BeginInterpolatedString("s"), 0, 1)),
          Some(success(Token.InterpolatedPart("outer "), 2, 7)),
          Some(success(Token.BeginInterpolatedEscape, 8, 9)),
          Some(success(Token.BeginInterpolatedString("s"), 10, 11)),
          Some(success(Token.InterpolatedPart("inner "), 12, 17)),
          Some(success(Token.Identifier.Lower("name"), 19, 22)),
          Some(success(Token.EndInterpolatedString, 23, 23)),
          Some(success(Token.EndInterpolatedEscape, 24, 24)),
          Some(success(Token.EndInterpolatedString, 25, 25))
        )
      }

      it("expression with match and cases") {
        check("x match { case 1 => 2 }",
          Some(success(Token.Identifier.Lower("x"), 0, 0)),
          Some(success(Token.Match, 2, 6)),
          Some(success(Token.LBrace, 8, 8)),
          Some(success(Token.Case, 10, 13)),
          Some(success(Token.IntLiteral(1), 15, 15)),
          Some(success(Token.RDoubleArrow, 17, 18)),
          Some(success(Token.IntLiteral(2), 20, 20)),
          Some(success(Token.RBrace, 22, 22))
        )
      }

      it("interpolated expression with multiple tokens") {
        check("s\"${1 + 2}\"",
          Some(success(Token.BeginInterpolatedString("s"), 0, 1)),
          Some(success(Token.BeginInterpolatedEscape, 2, 3)),
          Some(success(Token.IntLiteral(1), 4, 4)),
          Some(success(Token.Identifier.Operator("+"), 6, 6)),
          Some(success(Token.IntLiteral(2), 8, 8)),
          Some(success(Token.EndInterpolatedEscape, 9, 9)),
          Some(success(Token.EndInterpolatedString, 10, 10))
        )
      }
    }
  }

  private def check(input: String,
                    expectedTokens: Option[Pos[Either[ScannerError, Token]]]*): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default)
      expectedTokens.foreach { expected =>
        val actual = scanner.get()
        expected match {
          case Some(expectedPos) =>
            actual match {
              case ScannerResult.Success(actualPos) =>
                expectedPos.value match {
                  case Right(expectedToken) =>
                    withClue(s"Token at ${actualPos.begin}") {
                      actualPos.value shouldBe expectedToken
                      actualPos.begin shouldBe expectedPos.begin
                      actualPos.end shouldBe expectedPos.end
                    }
                  case Left(expectedError) =>
                    fail(s"Expected error $expectedError, but got success with ${actualPos.value}")
                }
              case ScannerResult.Error(actualPos) =>
                expectedPos.value match {
                  case Left(expectedError) =>
                    withClue(s"Error at ${actualPos.begin}") {
                      actualPos.value shouldBe expectedError
                      actualPos.begin shouldBe expectedPos.begin
                      actualPos.end shouldBe expectedPos.end
                    }
                  case Right(expectedToken) =>
                    fail(s"Expected success with $expectedToken, but got error ${actualPos.value}")
                }
              case ScannerResult.EndOfInput =>
                fail(s"Expected more tokens, but got EndOfInput (expected $expectedPos)")
            }
          case None =>
            actual shouldBe ScannerResult.EndOfInput
        }
      }
      scanner.get() shouldBe ScannerResult.EndOfInput
    }
  }
}
