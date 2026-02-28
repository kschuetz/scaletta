package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.LineEndingInterpolators._
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}

class ScannerTest extends AnyFunSpec with Matchers with AssertExpectedTokens {
  describe("Scanner") {
    describe("peek(n)") {
      it("should peek ahead without consuming") {
        val input = "val x = 1"
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          scanner.peek(1).value shouldBe Token.Val
          scanner.peek(1).value shouldBe Token.Val
          scanner.peek(2).value shouldBe Token.Identifier.Lower("x")
          scanner.peek(3).value shouldBe Token.Eq
          scanner.peek(4).value shouldBe Token.IntLiteral(1)
          scanner.peek(5).value shouldBe Token.EndOfInput

          scanner.get().value shouldBe Token.Val
          scanner.peek(1).value shouldBe Token.Identifier.Lower("x")
          scanner.get().value shouldBe Token.Identifier.Lower("x")
          scanner.get().value shouldBe Token.Eq
          scanner.get().value shouldBe Token.IntLiteral(1)
          scanner.get().value shouldBe Token.EndOfInput
        }
      }

      it("should handle semicolon inference during peek") {
        val input = lf"1\n2"
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          scanner.peek(1).value shouldBe Token.IntLiteral(1)
          scanner.peek(2).value shouldBe Token.Semicolon
          scanner.peek(3).value shouldBe Token.IntLiteral(2)
          scanner.peek(4).value shouldBe Token.EndOfInput

          scanner.get().value shouldBe Token.IntLiteral(1)
          scanner.get().value shouldBe Token.Semicolon
          scanner.get().value shouldBe Token.IntLiteral(2)
          scanner.get().value shouldBe Token.EndOfInput
        }
      }

      it("should return EndOfInput indefinitely when peeking beyond end") {
        val input = "1"
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          scanner.peek(1).value shouldBe Token.IntLiteral(1)
          scanner.peek(2).value shouldBe Token.EndOfInput
          scanner.peek(3).value shouldBe Token.EndOfInput
          scanner.peek(100).value shouldBe Token.EndOfInput
          scanner.peek(1).value shouldBe Token.IntLiteral(1)
        }
      }

      it("should throw exception for n < 1") {
        val input = "1"
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          intercept[IllegalArgumentException] {
            scanner.peek(0)
          }
          intercept[IllegalArgumentException] {
            scanner.peek(-1)
          }
        }
      }
    }

    describe("whitespace and comments") {
      it("should skip whitespace") {
        check(lf"  \t\n  123",
          success(Token.IntLiteral(123), 6, 8)
        )
        check(crlf"  \t\r\n  123",
          success(Token.IntLiteral(123), 7, 9)
        )
      }

      it("should skip line comments") {
        check(lf"// comment\n123",
          success(Token.IntLiteral(123), 11, 13)
        )
      }

      it("should skip block comments") {
        check("/* comment */123",
          success(Token.IntLiteral(123), 13, 15)
        )
      }

      it("should skip nested block comments") {
        check("/* outer /* inner */ outer */123",
          success(Token.IntLiteral(123), 29, 31)
        )
      }

      it("should skip multiple comments and whitespace") {
        check(lf"  // line\n  /* block */  123",
          success(Token.IntLiteral(123), 25, 27)
        )
      }

      it("should handle unterminated block comments") {
        check("/* comment",
          failure(ScannerError.UnclosedComment, 0, 10)
        )
      }
    }

    describe("numeric literals") {
      it("integers") {
        check("123", success(Token.IntLiteral(123), 0, 2))
        check("0", success(Token.IntLiteral(0), 0, 0))
        check("-123", success(Token.IntLiteral(-123), 0, 3))
      }

      it("longs") {
        check("123L", success(Token.LongLiteral(123L), 0, 3))
        check("-123l", success(Token.LongLiteral(-123L), 0, 4))
      }

      it("hex literals") {
        check("0x123", success(Token.IntLiteral(0x123), 0, 4))
        check("0XABC", success(Token.IntLiteral(0xABC), 0, 4))
        check("0x123L", success(Token.LongLiteral(0x123L), 0, 5))
      }

      it("binary literals") {
        check("0b101", success(Token.IntLiteral(5), 0, 4))
        check("0B110L", success(Token.LongLiteral(6L), 0, 5))
      }

      it("floating point literals") {
        check("1.23", success(Token.DoubleLiteral(1.23), 0, 3))
        check(".123", success(Token.DoubleLiteral(0.123), 0, 3))
        check("1.23f", success(Token.FloatLiteral(1.23f), 0, 4))
        check("1.23D", success(Token.DoubleLiteral(1.23), 0, 4))
        check("1e10", success(Token.DoubleLiteral(1e10), 0, 3))
        check("1.23e-4", success(Token.DoubleLiteral(1.23e-4), 0, 6))
        check("-1.23", success(Token.DoubleLiteral(-1.23), 0, 4))
        check("-.123", success(Token.DoubleLiteral(-0.123), 0, 4))
      }

      it("underscores in numeric literals") {
        check("1_000", success(Token.IntLiteral(1000), 0, 4))
        check("0x12_34", success(Token.IntLiteral(0x1234), 0, 6))
      }
    }

    describe("delimiters and operators") {
      it("should recognize basic delimiters") {
        check("().,;",
          success(Token.LParen, 0, 0),
          success(Token.RParen, 1, 1),
          success(Token.Dot, 2, 2),
          success(Token.Comma, 3, 3),
          success(Token.Semicolon, 4, 4)
        )
      }

      it("should recognize operators") {
        check("+ - * /",
          success(Token.Identifier.Operator("+"), 0, 0),
          success(Token.Identifier.Operator("-"), 2, 2),
          success(Token.Identifier.Operator("*"), 4, 4),
          success(Token.Identifier.Operator("/"), 6, 6)
        )
      }
    }

    describe("identifiers and reserved words") {
      it("should recognize identifiers") {
        check("foo barBaz",
          success(Token.Identifier.Lower("foo"), 0, 2),
          success(Token.Identifier.Lower("barBaz"), 4, 9)
        )
      }

      it("should recognize reserved words") {
        check("if else val def then case",
          success(Token.If, 0, 1),
          success(Token.Else, 3, 6),
          success(Token.Val, 8, 10),
          success(Token.Def, 12, 14),
          success(Token.Then, 16, 19),
          success(Token.Case, 21, 24)
        )
      }
    }

    describe("interpolated strings") {
      it("simple variable interpolation") {
        check("s\"hello $name\"",
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
          success(Token.InterpolatedPart("hello "), 2, 7),
          success(Token.Identifier.Lower("name"), 9, 12),
          success(Token.EndInterpolatedString, 13, 13)
        )
      }

      it("nested interpolations") {
        check("s\"outer ${s\"inner $name\"}\"",
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
          success(Token.InterpolatedPart("outer "), 2, 7),
          success(Token.BeginInterpolatedEscape, 8, 9),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 10, 11),
          success(Token.InterpolatedPart("inner "), 12, 17),
          success(Token.Identifier.Lower("name"), 19, 22),
          success(Token.EndInterpolatedString, 23, 23),
          success(Token.EndInterpolatedEscape, 24, 24),
          success(Token.EndInterpolatedString, 25, 25)
        )
      }

      it("expression with match and cases") {
        check("x match { case 1 => 2 }",
          success(Token.Identifier.Lower("x"), 0, 0),
          success(Token.Match, 2, 6),
          success(Token.LBrace, 8, 8),
          success(Token.Case, 10, 13),
          success(Token.IntLiteral(1), 15, 15),
          success(Token.RDoubleArrow, 17, 18),
          success(Token.IntLiteral(2), 20, 20),
          success(Token.RBrace, 22, 22)
        )
      }

      it("interpolated expression with multiple tokens") {
        check("s\"${1 + 2}\"",
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
          success(Token.BeginInterpolatedEscape, 2, 3),
          success(Token.IntLiteral(1), 4, 4),
          success(Token.Identifier.Operator("+"), 6, 6),
          success(Token.IntLiteral(2), 8, 8),
          success(Token.EndInterpolatedEscape, 9, 9),
          success(Token.EndInterpolatedString, 10, 10)
        )
      }
    }

    describe("semicolon inference") {
      it("should NOT infer semicolon between tokens on the same line") {
        check("val x = 1 val y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Val, 10, 12),
          success(Token.Identifier.Lower("y"), 14, 14),
          success(Token.Eq, 16, 16),
          success(Token.IntLiteral(2), 18, 18)
        )
      }

      it("should infer semicolon between statements on different lines") {
        check(lf"val x = 1\nval y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Val, 10, 12),
          success(Token.Identifier.Lower("y"), 14, 14),
          success(Token.Eq, 16, 16),
          success(Token.IntLiteral(2), 18, 18)
        )
      }

      it("should NOT infer semicolon when the previous token cannot terminate a statement") {
        check(lf"val x =\n1",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8)
        )
      }

      it("should NOT infer semicolon when the next token cannot begin a statement") {
        check(lf"x match\n{ case _ => 0 }",
          success(Token.Identifier.Lower("x"), 0, 0),
          success(Token.Match, 2, 6),
          success(Token.LBrace, 8, 8),
          success(Token.Case, 10, 13),
          success(Token.Underscore, 15, 15),
          success(Token.RDoubleArrow, 17, 18),
          success(Token.IntLiteral(0), 20, 20),
          success(Token.RBrace, 22, 22)
        )
      }

      it("should NOT infer semicolon inside parentheses") {
        check(lf"(1 +\n2)",
          success(Token.LParen, 0, 0),
          success(Token.IntLiteral(1), 1, 1),
          success(Token.Identifier.Operator("+"), 3, 3),
          success(Token.IntLiteral(2), 5, 5),
          success(Token.RParen, 6, 6)
        )
      }

      it("should NOT infer semicolon inside brackets") {
        check(lf"[A,\nB]",
          success(Token.LBracket, 0, 0),
          success(Token.Identifier.Upper("A"), 1, 1),
          success(Token.Comma, 2, 2),
          success(Token.Identifier.Upper("B"), 4, 4),
          success(Token.RBracket, 5, 5)
        )
      }

      it("should infer semicolon inside braces") {
        check(lf"{ val x = 1\nval y = 2 }",
          success(Token.LBrace, 0, 0),
          success(Token.Val, 2, 4),
          success(Token.Identifier.Lower("x"), 6, 6),
          success(Token.Eq, 8, 8),
          success(Token.IntLiteral(1), 10, 10),
          success(Token.Semicolon, 11, 11),
          success(Token.Val, 12, 14),
          success(Token.Identifier.Lower("y"), 16, 16),
          success(Token.Eq, 18, 18),
          success(Token.IntLiteral(2), 20, 20),
          success(Token.RBrace, 22, 22)
        )
      }

      it("should NOT infer semicolon before 'else'") {
        check(lf"if (true) 1\nelse 2",
          success(Token.If, 0, 1),
          success(Token.LParen, 3, 3),
          success(Token.True, 4, 7),
          success(Token.RParen, 8, 8),
          success(Token.IntLiteral(1), 10, 10),
          success(Token.Else, 12, 15),
          success(Token.IntLiteral(2), 17, 17)
        )
      }

      it("should NOT infer semicolon before 'match'") {
        check(lf"x\nmatch { case _ => 0 }",
          success(Token.Identifier.Lower("x"), 0, 0),
          success(Token.Match, 2, 6),
          success(Token.LBrace, 8, 8),
          success(Token.Case, 10, 13),
          success(Token.Underscore, 15, 15),
          success(Token.RDoubleArrow, 17, 18),
          success(Token.IntLiteral(0), 20, 20),
          success(Token.RBrace, 22, 22)
        )
      }

      it("should infer semicolon after block comment with newline") {
        check(lf"1 /* comment */\n2",
          success(Token.IntLiteral(1), 0, 0),
          success(Token.Semicolon, 15, 15),
          success(Token.IntLiteral(2), 16, 16)
        )
      }

      it("should infer semicolon after line comment") {
        check(lf"1 // comment\n2",
          success(Token.IntLiteral(1), 0, 0),
          success(Token.Semicolon, 12, 12),
          success(Token.IntLiteral(2), 13, 13)
        )
      }

      describe("line ending invariance") {
        it("should infer semicolon for all line ending types (value-only)") {
          val code = "val x = 1\nval y = 2"
          val expected = Vector(
            Token.Val, Token.Identifier.Lower("x"), Token.Eq, Token.IntLiteral(1),
            Token.Semicolon,
            Token.Val, Token.Identifier.Lower("y"), Token.Eq, Token.IntLiteral(2)
          )

          checkValuesOnly(lf"$code",
            crlf"$code",
            cr"$code") { actual =>
            actual shouldBe expected
          }
        }

        it("should skip all types of whitespace and line endings (value-only)") {
          val expected = Vector(Token.IntLiteral(1), Token.IntLiteral(2))

          checkValuesOnly(lf"1\n  \t  \n2",
            crlf"1\n  \t  \n2",
            cr"1\n  \t  \n2") { actual =>
            actual.filter(_ != Token.Semicolon) shouldBe expected
          }
        }

        it("should handle complex sequences identically (value-only)") {
          val code =
            """/* Multi-line
              |   comment */
              |val x = 1 // end of line
              |s"interpolated $x string"
              |/* outer /* inner */ outer */
              |1 +
              |  2""".stripMargin

          val expected = Vector(
            Token.Val, Token.Identifier.Lower("x"), Token.Eq, Token.IntLiteral(1),
            Token.BeginInterpolatedString(Interpolator.fromName("s")),
            Token.InterpolatedPart("interpolated "),
            Token.Identifier.Lower("x"),
            Token.InterpolatedPart(" string"),
            Token.EndInterpolatedString,
            Token.Semicolon,
            Token.IntLiteral(1), Token.Identifier.Operator("+"), Token.IntLiteral(2)
          )

          checkValuesOnly(lf"$code", crlf"$code", cr"$code") { actual =>
            actual shouldBe expected
          }
        }
      }

      it("should NOT infer semicolon when an explicit semicolon is present on the same line") {
        check("val x = 1; val y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Val, 11, 13),
          success(Token.Identifier.Lower("y"), 15, 15),
          success(Token.Eq, 17, 17),
          success(Token.IntLiteral(2), 19, 19)
        )
      }

      it("should NOT infer an additional semicolon when an explicit one is followed by a newline") {
        check("val x = 1;\nval y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Val, 11, 13),
          success(Token.Identifier.Lower("y"), 15, 15),
          success(Token.Eq, 17, 17),
          success(Token.IntLiteral(2), 19, 19)
        )
      }

      it("should handle mixed explicit and inferred semicolons") {
        check("val x = 1; val y = 2\nval z = 3",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Val, 11, 13),
          success(Token.Identifier.Lower("y"), 15, 15),
          success(Token.Eq, 17, 17),
          success(Token.IntLiteral(2), 19, 19),
          success(Token.Semicolon, 20, 20),
          success(Token.Val, 21, 23),
          success(Token.Identifier.Lower("z"), 25, 25),
          success(Token.Eq, 27, 27),
          success(Token.IntLiteral(3), 29, 29)
        )
      }

      it("should handle explicit semicolon before a comment and newline") {
        check("val x = 1; // comment\nval y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Val, 22, 24),
          success(Token.Identifier.Lower("y"), 26, 26),
          success(Token.Eq, 28, 28),
          success(Token.IntLiteral(2), 30, 30)
        )
      }

      it("should handle multiple explicit semicolons on the same line") {
        check("val x = 1;; val y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Semicolon, 10, 10),
          success(Token.Val, 12, 14),
          success(Token.Identifier.Lower("y"), 16, 16),
          success(Token.Eq, 18, 18),
          success(Token.IntLiteral(2), 20, 20)
        )
      }

      it("should handle multiple explicit semicolons split by a newline") {
        check("val x = 1;\n;val y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Semicolon, 11, 11),
          success(Token.Val, 12, 14),
          success(Token.Identifier.Lower("y"), 16, 16),
          success(Token.Eq, 18, 18),
          success(Token.IntLiteral(2), 20, 20)
        )
      }

      it("should handle inferred semicolon followed by an explicit one") {
        check("val x = 1\n; val y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 10, 10),
          success(Token.Val, 12, 14),
          success(Token.Identifier.Lower("y"), 16, 16),
          success(Token.Eq, 18, 18),
          success(Token.IntLiteral(2), 20, 20)
        )
      }

      it("should handle multiple explicit semicolons separated by newlines") {
        check("val x = 1;\n;\nval y = 2",
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.IntLiteral(1), 8, 8),
          success(Token.Semicolon, 9, 9),
          success(Token.Semicolon, 11, 11),
          success(Token.Val, 13, 15),
          success(Token.Identifier.Lower("y"), 17, 17),
          success(Token.Eq, 19, 19),
          success(Token.IntLiteral(2), 21, 21)
        )
      }

      it("should NOT infer semicolon when line starts with an operator (infix notation)") {
        check("1\n+ 2",
          success(Token.IntLiteral(1), 0, 0),
          success(Token.Identifier.Operator("+"), 2, 2),
          success(Token.IntLiteral(2), 4, 4)
        )
      }

      it("should NOT infer semicolon when line starts with a dot") {
        check("foo\n.bar",
          success(Token.Identifier.Lower("foo"), 0, 2),
          success(Token.Dot, 4, 4),
          success(Token.Identifier.Lower("bar"), 5, 7)
        )
      }

      it("should emit the semicolon at the same position in the source code the newline was encountered") {
        check(
          lf"""val foo = 1
              |val bar = 2
              |""".stripMargin,
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("foo"), 4, 6),
          success(Token.Eq, 8, 8),
          success(Token.IntLiteral(1), 10, 10),
          success(Token.Semicolon, 11, 11),
          success(Token.Val, 12, 14),
          success(Token.Identifier.Lower("bar"), 16, 18),
          success(Token.Eq, 20, 20),
          success(Token.IntLiteral(2), 22, 22),
        )
      }
    }

    it("collects multiple errors in a single pass") {
      val input = "val \u0001 123 . . 5"
      check(input,
        success(Token.Val, 0, 2),
        failure(ScannerError.InvalidCharacter, 4, 4),
        success(Token.IntLiteral(123), 6, 8),
        success(Token.Dot, 10, 10),
        success(Token.Dot, 12, 12),
        success(Token.IntLiteral(5), 14, 14)
      )
    }

    it("handles unclosed string literal and resumes on next line") {
      val input =
        lf"""val x = "hello world
            |val y = 2""".stripMargin

      // Position breakdown:
      // "val x = " is 0-7
      // "\"hello world" starts at index 8. It hits newline at index 20.
      // Literals.stringLiteral ungets the newline and returns error.
      // Resume skipCommentsAndWhitespace will find the newline at index 20.
      // It will then see "val y = 2" starting at index 21.

      check(input,
        success(Token.Val, 0, 2),
        success(Token.Identifier.Lower("x"), 4, 4),
        success(Token.Eq, 6, 6),
        failure(ScannerError.UnclosedStringLiteral, 8, 19),
        success(Token.Semicolon, 20, 20),
        success(Token.Val, 21, 23),
        success(Token.Identifier.Lower("y"), 25, 25),
        success(Token.Eq, 27, 27),
        success(Token.IntLiteral(2), 29, 29)
      )
    }

    it("handles unclosed multi-line string literal") {
      val input =
        lf"""val x = """ + "\"\"\"" +
          lf"""hello
              |val y = 2""".stripMargin

      // Multi-line strings in Literals.scala currently scan until the end of input
      // if not closed. It should return a single error for the whole thing.

      check(input,
        success(Token.Val, 0, 2),
        success(Token.Identifier.Lower("x"), 4, 4),
        success(Token.Eq, 6, 6),
        failure(ScannerError.UnclosedMultiLineString, 8, 25)
      )
    }

    it("handles unclosed quoted identifier and resumes on next line") {
      val input =
        lf"""val `unclosed = 1
            |val y = 2""".stripMargin

      // In Scaletta, quoted identifiers currently stop at a newline.
      // Index 4: `
      // Index 5-12: unclosed
      // Index 13: space
      // Index 14: =
      // Index 15: space
      // Index 16: 1
      // Index 17: newline
      // IdentifierScanner.quoted returns Error(UnclosedQuotedIdentifier) at line 153
      // It ungets the \n, so reader.prevIndex is 16.

      check(input,
        success(Token.Val, 0, 2),
        // "val `unclosed = 1"
        // 01234567890123456
        failure(ScannerError.UnclosedQuotedIdentifier, 4, 16),
        // After Error, Scanner.get calls readNext -> skipCommentsAndWhitespace.
        // It skips the \n at 17.
        // readToken(Some(17)) sees "val y = 2" starting at 18.
        success(Token.Semicolon, 17, 17),
        success(Token.Val, 18, 20),
        success(Token.Identifier.Lower("y"), 22, 22),
        success(Token.Eq, 24, 24),
        success(Token.IntLiteral(2), 26, 26)
      )
    }

    it("unclosed string with trailing backslash does not leak to next line") {
      val input =
        lf"""val x = "unclosed \\
            |val y = 2""".stripMargin

      check(input,
        success(Token.Val, 0, 2),
        success(Token.Identifier.Lower("x"), 4, 4),
        success(Token.Eq, 6, 6),
        // "unclosed \
        // starts at 8. It hits \ at 18.
        // It identifies the newline as a boundary and returns UnclosedStringLiteral error.
        failure(ScannerError.UnclosedStringLiteral, 8, 18),
        success(Token.Semicolon, 19, 19),
        success(Token.Val, 20, 22),
        success(Token.Identifier.Lower("y"), 24, 24),
        success(Token.Eq, 26, 26),
        success(Token.IntLiteral(2), 28, 28)
      )
    }

    it("handles unclosed quoted identifier with trailing backslash does not leak to next line") {
      val input =
        lf"""val `unclosed \\
            |val y = 2""".stripMargin

      check(input,
        success(Token.Val, 0, 2),
        // `unclosed \
        // Similar to string above.
        failure(ScannerError.UnclosedQuotedIdentifier, 4, 14),
        success(Token.Semicolon, 15, 15),
        success(Token.Val, 16, 18),
        success(Token.Identifier.Lower("y"), 20, 20),
        success(Token.Eq, 22, 22),
        success(Token.IntLiteral(2), 24, 24)
      )
    }

    it("handles unclosed string literal with partial Unicode escape") {
      // Index 0-7: "hello \
      // Index 8-11: u00
      // Index 12: \n
      val input = "\"hello \\u00\nval y = 2"
      check(input,
        // The Unicode escape hits the newline and triggers a boundary exit.
        // It reports the string as unclosed up to the point where it hit the boundary.
        failure(ScannerError.UnclosedStringLiteral, 0, 10),
        success(Token.Semicolon, 11, 11),
        success(Token.Val, 12, 14),
        success(Token.Identifier.Lower("y"), 16, 16),
        success(Token.Eq, 18, 18),
        success(Token.IntLiteral(2), 20, 20)
      )
    }

    it("handles unclosed expression escape in multi-line interpolator") {
      val input = "val x = s\"\"\"hello ${ world\nval y = 2\n\"\"\"\nval z = 3"

      check(input,
        success(Token.Val, 0, 2),
        success(Token.Identifier.Lower("x"), 4, 4),
        success(Token.Eq, 6, 6),
        success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 8, 11),
        success(Token.InterpolatedPart("hello "), 12, 17),
        success(Token.BeginInterpolatedEscape, 18, 19),
        success(Token.Identifier.Lower("world"), 21, 25),
        success(Token.Semicolon, 26, 26),
        success(Token.Val, 27, 29),
        success(Token.Identifier.Lower("y"), 31, 31),
        success(Token.Eq, 33, 33),
        success(Token.IntLiteral(2), 35, 35),
        success(Token.Semicolon, 36, 36),
        // The closing """ is at index 37-39.
        // Robust behavior: forced exit and report error.
        failure(ScannerError.UnclosedMultiLineString, 37, 39),
        success(Token.Semicolon, 40, 40),
        success(Token.Val, 41, 43),
        success(Token.Identifier.Lower("z"), 45, 45),
        success(Token.Eq, 47, 47),
        success(Token.IntLiteral(3), 49, 49)
      )
    }

    it("handles malformed hex literal and observes resume point") {
      // 0xG should be one error, then resume
      val input = "0xG val"
      check(input,
        failure(ScannerError.InvalidLiteralNumber, 0, 2), // Consumes ''
        success(Token.Val, 4, 6)
      )
    }

    it("handles malformed decimal literal and observes resume point") {
      // 1.2.3 should be 1.2, then .3 (current behavior)
      val input = "1.2.3 val"
      check(input,
        success(Token.DoubleLiteral(1.2), 0, 2),
        success(Token.DoubleLiteral(0.3), 3, 4),
        success(Token.Val, 6, 8)
      )
    }

    it("handles illegal separator in numeric literal and observes resume point") {
      // 123_ followed by space and val.
      // Strict behavior: The whole 123_ is a failed number literal.
      val input = "123_ val"
      check(input,
        failure(ScannerError.IllegalSeparator, 3, 3),
        success(Token.Val, 5, 7)
      )
    }

    it("handles malformed exponent and observes resume point") {
      // 1e+ followed by space and val.
      // Strict behavior: 1e is a failed number literal. The + should be preserved.
      val input = "1e+ val"
      check(input,
        failure(ScannerError.InvalidLiteralNumber, 0, 1),
        success(Token.Identifier.Operator("+"), 2, 2),
        success(Token.Val, 4, 6)
      )
    }

    it("handles illegal separators in scientific notation and observes resume point") {
      // Case A: 1_e+10. Underscore before 'e' is illegal in Scala.
      // Scala reports illegal separator and doesn't yield the '1'.
      check("1_e+10",
        failure(ScannerError.IllegalSeparator, 1, 1),
        success(Token.Identifier.Lower("e"), 2, 2),
        success(Token.Identifier.Operator("+"), 3, 3),
        success(Token.IntLiteral(10), 4, 5)
      )

      // Case B: 1e+_10. Underscore after '+' is illegal.
      // 1e+ is an invalid literal number.
      check("1e+_10",
        failure(ScannerError.IllegalSeparator, 3, 3),
        success(Token.IntLiteral(10), 4, 5)
      )
    }

    describe("garbage clustering") {
      it("groups invalid character clusters into a single error") {
        val input = "\u0001\u0002\u0003 val"
        // Goal: One error for the cluster, then resume at 'val'
        check(input,
          failure(ScannerError.InvalidCharacter, 0, 2),
          success(Token.Val, 4, 6)
        )
      }

      it("groups invalid character clusters even without trailing whitespace") {
        val input = "\u0001\u0002\u0003val"
        // Goal: Group garbage, then resume immediately at identifier 'val'
        check(input,
          failure(ScannerError.InvalidCharacter, 0, 2),
          success(Token.Val, 3, 5)
        )
      }

      it("garbage cluster grouping stops at newlines") {
        val input = "\u0001\u0002\n\u0003\u0004"
        // Garbage should be grouped per-line to preserve newline synchronization
        check(input,
          failure(ScannerError.InvalidCharacter, 0, 1),
          failure(ScannerError.InvalidCharacter, 3, 4)
        )
      }

      it("garbage cluster grouping stops at delimiters") {
        val input = "\u0001\u0002(123)"
        // Garbage should not 'eat' the opening parenthesis
        check(input,
          failure(ScannerError.InvalidCharacter, 0, 1),
          success(Token.LParen, 2, 2),
          success(Token.IntLiteral(123), 3, 5),
          success(Token.RParen, 6, 6)
        )
      }

      it("groups invalid character clusters inside interpolation escapes") {
        val input = "s\"${\u0001\u0002}\""
        // Garbage inside ${ } should be grouped without breaking the interpolation state
        check(input,
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
          success(Token.BeginInterpolatedEscape, 2, 3),
          failure(ScannerError.InvalidCharacter, 4, 5),
          success(Token.EndInterpolatedEscape, 6, 6),
          success(Token.EndInterpolatedString, 7, 7)
        )
      }

      it("garbage cluster grouping interacts correctly with comments") {
        // Example A: Garbage followed by a comment
        // The grouping must stop at the '/' so the comment scanner can see it.
        val inputA = "\u0001\u0002// comment\nval x = 1"
        check(inputA,
          failure(ScannerError.InvalidCharacter, 0, 1),
          success(Token.Semicolon, 12, 12),
          success(Token.Val, 13, 15),
          success(Token.Identifier.Lower("x"), 17, 17),
          success(Token.Eq, 19, 19),
          success(Token.IntLiteral(1), 21, 21)
        )

        // Example B: Garbage following a block comment
        val inputB = "/* comment */\u0003\u0004 val"
        check(inputB,
          failure(ScannerError.InvalidCharacter, 13, 14),
          success(Token.Val, 16, 18)
        )
      }

      it("garbage cluster grouping stops at dots (number or field access)") {
        // Case A: Followed by number
        check("\u0001.123",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.DoubleLiteral(0.123), 1, 4)
        )

        // Case B: Followed by field access
        check("\u0001.foo",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.Dot, 1, 1),
          success(Token.Identifier.Lower("foo"), 2, 4)
        )
      }

      it("treats unrecognized Unicode symbols as operators (Scala-style)") {
        // Snowman (\u2603) and Comet (\u2604) are valid operators in Scala/Scaletta
        val input = "\u2603\u2604 val"
        check(input,
          success(Token.Identifier.Operator("\u2603\u2604"), 0, 1),
          success(Token.Val, 3, 5)
        )
      }

      it("groups truly invalid characters (e.g. control chars) as garbage clusters") {
        // Using non-whitespace control characters that aren't operators or identifiers
        val input = "\u0001\u0002\u0003 val"
        check(input,
          failure(ScannerError.InvalidCharacter, 0, 2),
          success(Token.Val, 4, 6)
        )
      }

      it("garbage cluster grouping stops at literal delimiters") {
        // Case A: Followed by string
        check("\u0001\"hello\"",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.StringLiteral("hello"), 1, 7)
        )

        // Case B: Followed by character
        check("\u0001'a'",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.CharLiteral('a'), 1, 3)
        )
      }

      it("garbage cluster grouping interacts correctly with interpolation starts: single line") {
        check("\u0001s\"hello\"",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 1, 2),
          success(Token.InterpolatedPart("hello"), 3, 7),
          success(Token.EndInterpolatedString, 8, 8)
        )
      }

      it("garbage cluster grouping interacts correctly with interpolation starts: multi-line") {
        check("\u0001raw\"\"\"hello\"\"\"",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("raw")), 1, 6),
          success(Token.InterpolatedPart("hello"), 7, 11),
          success(Token.EndInterpolatedString, 12, 14)
        )
      }

      it("garbage cluster grouping hand-off to complex identifiers") {
        // This test specifically targets the hand-off between garbage and multi-character identifiers
        // that are NOT interpolators.
        check("\u0001abc",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.Identifier.Lower("abc"), 1, 3)
        )
      }

      it("garbage followed by '+'") {
        // If we have redundant unget, this will fail or return shifted tokens
        check("\u0001+",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.Identifier.Operator("+"), 1, 1)
        )
      }

      it("handles garbage followed by '-' and then a digit") {
        // This tests the case where '-' is ambiguous until we see the digit
        check("\u0001-123",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.IntLiteral(-123), 1, 4)
        )
      }

      it("handles garbage followed by a failed interpolator lookahead") {
        // 'raw' followed by a space is NOT an interpolator.
        // It should be scanned as an identifier after the garbage error.
        check("\u0001raw ",
          failure(ScannerError.InvalidCharacter, 0, 0),
          success(Token.Identifier.Lower("raw"), 1, 3)
        )
      }

      it("handles garbage in the middle of a partial numeric lookahead") {
        // '1.' followed by garbage should not eat the garbage.
        // In Scaletta, '1.' currently scans as an IntLiteral(1) followed by a Dot.
        check("1.\u0001",
          success(Token.IntLiteral(1), 0, 0),
          success(Token.Dot, 1, 1),
          failure(ScannerError.InvalidCharacter, 2, 2)
        )
      }
    }

    describe("EndOfInput") {
      it("saturates EndOfInput for empty input") {
        val input = ""
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          val first = scanner.get()
          first.value shouldBe Token.EndOfInput
          first.begin.value shouldBe 0
          first.end.value shouldBe 0

          val second = scanner.get()
          second.value shouldBe Token.EndOfInput
          second.begin.value shouldBe 0
          second.end.value shouldBe 0

          val third = scanner.get()
          third.value shouldBe Token.EndOfInput
          third.begin.value shouldBe 0
          third.end.value shouldBe 0
        }
      }

      it("saturates EndOfInput for non-empty input") {
        val input = "abc"
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          val first = scanner.get()
          first.value shouldBe Token.Identifier.Lower("abc")

          val second = scanner.get()
          second.value shouldBe Token.EndOfInput
          second.begin.value shouldBe 3
          second.end.value shouldBe 3

          val third = scanner.get()
          third.value shouldBe Token.EndOfInput
          third.begin.value shouldBe 3
          third.end.value shouldBe 3
        }
      }
    }

    describe("complex expressions") {
      it("case 1") {
        val input =
          lf"""{
              |  val name = "world"
              |  val msg = s"Hello, $${name.toUpperCase}!"
              |  val result = (1 + 2) * 3
              |  msg + " " + result.toString
              |}""".stripMargin
        check(input,
          success(Token.LBrace, 0, 0),
          success(Token.Val, 4, 6),
          success(Token.Identifier.Lower("name"), 8, 11),
          success(Token.Eq, 13, 13),
          success(Token.StringLiteral("world"), 15, 21),
          success(Token.Semicolon, 22, 22),
          success(Token.Val, 25, 27),
          success(Token.Identifier.Lower("msg"), 29, 31),
          success(Token.Eq, 33, 33),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 35, 36),
          success(Token.InterpolatedPart("Hello, "), 37, 43),
          success(Token.BeginInterpolatedEscape, 44, 45),
          success(Token.Identifier.Lower("name"), 46, 49),
          success(Token.Dot, 50, 50),
          success(Token.Identifier.Lower("toUpperCase"), 51, 61),
          success(Token.EndInterpolatedEscape, 62, 62),
          success(Token.InterpolatedPart("!"), 63, 63),
          success(Token.EndInterpolatedString, 64, 64),
          success(Token.Semicolon, 65, 65),
          success(Token.Val, 68, 70),
          success(Token.Identifier.Lower("result"), 72, 77),
          success(Token.Eq, 79, 79),
          success(Token.LParen, 81, 81),
          success(Token.IntLiteral(1), 82, 82),
          success(Token.Identifier.Operator("+"), 84, 84),
          success(Token.IntLiteral(2), 86, 86),
          success(Token.RParen, 87, 87),
          success(Token.Identifier.Operator("*"), 89, 89),
          success(Token.IntLiteral(3), 91, 91),
          success(Token.Semicolon, 92, 92),
          success(Token.Identifier.Lower("msg"), 95, 97),
          success(Token.Identifier.Operator("+"), 99, 99),
          success(Token.StringLiteral(" "), 101, 103),
          success(Token.Identifier.Operator("+"), 105, 105),
          success(Token.Identifier.Lower("result"), 107, 112),
          success(Token.Dot, 113, 113),
          success(Token.Identifier.Lower("toString"), 114, 121),
          success(Token.RBrace, 123, 123)
        )
      }

      it("case 2") {
        val input =
          lf"""if (x > 0) {
              |  s"Positive: $$x"
              |} else if (x < 0) {
              |  s"Negative: $$x"
              |} else {
              |  "Zero"
              |}""".stripMargin
        check(input,
          success(Token.If, 0, 1),
          success(Token.LParen, 3, 3),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Identifier.Operator(">"), 6, 6),
          success(Token.IntLiteral(0), 8, 8),
          success(Token.RParen, 9, 9),
          success(Token.LBrace, 11, 11),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 15, 16),
          success(Token.InterpolatedPart("Positive: "), 17, 26),
          success(Token.Identifier.Lower("x"), 28, 28),
          success(Token.EndInterpolatedString, 29, 29),
          success(Token.RBrace, 31, 31),
          success(Token.Else, 33, 36),
          success(Token.If, 38, 39),
          success(Token.LParen, 41, 41),
          success(Token.Identifier.Lower("x"), 42, 42),
          success(Token.Identifier.Operator("<"), 44, 44),
          success(Token.IntLiteral(0), 46, 46),
          success(Token.RParen, 47, 47),
          success(Token.LBrace, 49, 49),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 53, 54),
          success(Token.InterpolatedPart("Negative: "), 55, 64),
          success(Token.Identifier.Lower("x"), 66, 66),
          success(Token.EndInterpolatedString, 67, 67),
          success(Token.RBrace, 69, 69),
          success(Token.Else, 71, 74),
          success(Token.LBrace, 76, 76),
          success(Token.StringLiteral("Zero"), 80, 85),
          success(Token.RBrace, 87, 87)
        )
      }

      it("case 3") {
        val input =
          lf"""res match {
              |  case s"foo$$x" => s"Matched foo with $$x"
              |  case _ =>
              |    val fallback = "none"
              |    fallback
              |}""".stripMargin
        check(input,
          success(Token.Identifier.Lower("res"), 0, 2),
          success(Token.Match, 4, 8),
          success(Token.LBrace, 10, 10),
          success(Token.Case, 14, 17),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 19, 20),
          success(Token.InterpolatedPart("foo"), 21, 23),
          success(Token.Identifier.Lower("x"), 25, 25),
          success(Token.EndInterpolatedString, 26, 26),
          success(Token.RDoubleArrow, 28, 29),
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 31, 32),
          success(Token.InterpolatedPart("Matched foo with "), 33, 49),
          success(Token.Identifier.Lower("x"), 51, 51),
          success(Token.EndInterpolatedString, 52, 52),
          success(Token.Case, 56, 59),
          success(Token.Underscore, 61, 61),
          success(Token.RDoubleArrow, 63, 64),
          success(Token.Val, 70, 72),
          success(Token.Identifier.Lower("fallback"), 74, 81),
          success(Token.Eq, 83, 83),
          success(Token.StringLiteral("none"), 85, 90),
          success(Token.Semicolon, 91, 91),
          success(Token.Identifier.Lower("fallback"), 96, 103),
          success(Token.RBrace, 105, 105)
        )
      }
    }

    describe("portal mode") {
      it("should exit silently at the closing brace that balances the entrance") {
        val input = "x + 1}"
        checkPortal(input,
          success(Token.Identifier.Lower("x"), 0, 0),
          success(Token.Identifier.Operator("+"), 2, 2),
          success(Token.IntLiteral(1), 4, 4)
          // No RBrace here
        )
      }

      it("should leave the reader positioned exactly after the closing brace") {
        val input = "x}tail"
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)
          scanner.get().value shouldBe Token.Identifier.Lower("x")
          scanner.get().value shouldBe Token.EndOfInput
          scanner.get().value shouldBe Token.EndOfInput
          reader.currentIndex.value shouldBe 2 // exactly at 't' in 'tail'
        }
      }

      it("should track nested structural braces and only exit at the top-level one") {
        val input = "{ x } }"
        checkPortal(input,
          success(Token.LBrace, 0, 0),
          success(Token.Identifier.Lower("x"), 2, 2),
          success(Token.RBrace, 4, 4)
          // Exits at the second '}'
        )
      }

      it("should ignore braces in comments and string literals") {
        val input =
          """ "}" // }
            |}""".stripMargin
        checkPortal(input,
          success(Token.StringLiteral("}"), 1, 3)
          // Exits at the final '}'
        )
      }

      it("should yield a fatal error if EndOfInput is reached while depth > 0") {
        val input = "x + { 1"
        checkPortal(input,
          success(Token.Identifier.Lower("x"), 0, 0),
          success(Token.Identifier.Operator("+"), 2, 2),
          success(Token.LBrace, 4, 4),
          success(Token.IntLiteral(1), 6, 6),
          failure(ScannerError.UnbalancedBraces, 7, 7)
        )
      }

      it("should yield a fatal error if EndOfInput is reached immediately") {
        val input = ""
        checkPortal(input,
          failure(ScannerError.UnbalancedBraces, 0, 0)
        )
      }

      it("should not exit if a brace is encountered while a non-portal region is on the stack") {
        val input = "( x }"
        checkPortal(input,
          success(Token.LParen, 0, 0),
          success(Token.Identifier.Lower("x"), 2, 2),
          success(Token.RBrace, 4, 4),
          failure(ScannerError.UnbalancedBraces, 5, 5)
        )
      }

      it("should only exit when the stack is balanced back to the Portal region") {
        val input = "val x = (1 } tail }"
        checkPortal(input,
          success(Token.Val, 0, 2),
          success(Token.Identifier.Lower("x"), 4, 4),
          success(Token.Eq, 6, 6),
          success(Token.LParen, 8, 8),
          success(Token.IntLiteral(1), 9, 9),
          success(Token.RBrace, 11, 11), // typo, doesn't exit
          success(Token.Identifier.Lower("tail"), 13, 16),
          success(Token.RBrace, 18, 18), // still doesn't exit because '(' is still opn
          failure(ScannerError.UnbalancedBraces, 19, 19) // reaches EOF while unbalancd
        )
      }
    }
  }

  private def check(input: String,
                    expectedTokens: Pos[Token]*)
                   (implicit pos: Position): Unit = {
    checkWithMode(input, portalMode = false, expectedTokens: _*)
  }

  private def checkPortal(input: String,
                          expectedTokens: Pos[Token]*)
                         (implicit pos: Position): Unit = {
    checkWithMode(input, portalMode = true, expectedTokens: _*)
  }

  private def checkWithMode(input: String,
                            portalMode: Boolean,
                            expectedTokens: Pos[Token]*)
                           (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = portalMode)
      val actualTokens = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
      val expectedPosList = expectedTokens.toVector

      assertExpectedTokens(input, expectedPosList, actualTokens)
    }
  }

  private def checkValuesOnly(inputs: String*)
                             (assertions: Vector[Token] => Unit)
                             (implicit pos: Position): Unit =
    inputs.foreach { input =>
      withClue(s"Input: $input") {
        TestReaderFactory.fromString(input) { reader =>
          val scanner = Scanner.create(reader, IdentifierPolicy.Default)
          val actual = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
          assertions(actual.map(_.value))
        }
      }
    }

}
