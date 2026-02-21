package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory

class ScannerTest extends AnyFunSpec with Matchers {
  describe("Scanner") {
    describe("whitespace and comments") {
      it("should skip whitespace") {
        check("  \t\n  123",
          Some(success(Token.IntLiteral(123), 6, 8))
        )
        // The \r preceding the \n will be ignored and will not have its own char index:
        check("  \t\r\n  123",
          Some(success(Token.IntLiteral(123), 6, 8))
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

    describe("semicolon inference") {
      it("should NOT infer semicolon between tokens on the same line") {
        check("val x = 1 val y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Val, 10, 12)),
          Some(success(Token.Identifier.Lower("y"), 14, 14)),
          Some(success(Token.Eq, 16, 16)),
          Some(success(Token.IntLiteral(2), 18, 18))
        )
      }

      it("should infer semicolon between statements on different lines") {
        check("val x = 1\nval y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Val, 10, 12)),
          Some(success(Token.Identifier.Lower("y"), 14, 14)),
          Some(success(Token.Eq, 16, 16)),
          Some(success(Token.IntLiteral(2), 18, 18))
        )
      }

      it("should NOT infer semicolon when the previous token cannot terminate a statement") {
        check("val x =\n1",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8))
        )
      }

      it("should NOT infer semicolon when the next token cannot begin a statement") {
        check("x match\n{ case _ => 0 }",
          Some(success(Token.Identifier.Lower("x"), 0, 0)),
          Some(success(Token.Match, 2, 6)),
          Some(success(Token.LBrace, 8, 8)),
          Some(success(Token.Case, 10, 13)),
          Some(success(Token.Underscore, 15, 15)),
          Some(success(Token.RDoubleArrow, 17, 18)),
          Some(success(Token.IntLiteral(0), 20, 20)),
          Some(success(Token.RBrace, 22, 22))
        )
      }

      it("should NOT infer semicolon inside parentheses") {
        check("(1 +\n2)",
          Some(success(Token.LParen, 0, 0)),
          Some(success(Token.IntLiteral(1), 1, 1)),
          Some(success(Token.Identifier.Operator("+"), 3, 3)),
          Some(success(Token.IntLiteral(2), 5, 5)),
          Some(success(Token.RParen, 6, 6))
        )
      }

      it("should NOT infer semicolon inside brackets") {
        check("[A,\nB]",
          Some(success(Token.LBracket, 0, 0)),
          Some(success(Token.Identifier.Upper("A"), 1, 1)),
          Some(success(Token.Comma, 2, 2)),
          Some(success(Token.Identifier.Upper("B"), 4, 4)),
          Some(success(Token.RBracket, 5, 5))
        )
      }

      it("should infer semicolon inside braces") {
        check("{ val x = 1\nval y = 2 }",
          Some(success(Token.LBrace, 0, 0)),
          Some(success(Token.Val, 2, 4)),
          Some(success(Token.Identifier.Lower("x"), 6, 6)),
          Some(success(Token.Eq, 8, 8)),
          Some(success(Token.IntLiteral(1), 10, 10)),
          Some(success(Token.Semicolon, 11, 11)),
          Some(success(Token.Val, 12, 14)),
          Some(success(Token.Identifier.Lower("y"), 16, 16)),
          Some(success(Token.Eq, 18, 18)),
          Some(success(Token.IntLiteral(2), 20, 20)),
          Some(success(Token.RBrace, 22, 22))
        )
      }

      it("should NOT infer semicolon before 'else'") {
        check("if (true) 1\nelse 2",
          Some(success(Token.If, 0, 1)),
          Some(success(Token.LParen, 3, 3)),
          Some(success(Token.True, 4, 7)),
          Some(success(Token.RParen, 8, 8)),
          Some(success(Token.IntLiteral(1), 10, 10)),
          Some(success(Token.Else, 12, 15)),
          Some(success(Token.IntLiteral(2), 17, 17))
        )
      }

      it("should NOT infer semicolon before 'match'") {
        check("x\nmatch { case _ => 0 }",
          Some(success(Token.Identifier.Lower("x"), 0, 0)),
          Some(success(Token.Match, 2, 6)),
          Some(success(Token.LBrace, 8, 8)),
          Some(success(Token.Case, 10, 13)),
          Some(success(Token.Underscore, 15, 15)),
          Some(success(Token.RDoubleArrow, 17, 18)),
          Some(success(Token.IntLiteral(0), 20, 20)),
          Some(success(Token.RBrace, 22, 22))
        )
      }

      it("should infer semicolon after block comment with newline") {
        check("1 /* comment */\n2",
          Some(success(Token.IntLiteral(1), 0, 0)),
          Some(success(Token.Semicolon, 15, 15)),
          Some(success(Token.IntLiteral(2), 16, 16))
        )
      }

      it("should infer semicolon after line comment") {
        check("1 // comment\n2",
          Some(success(Token.IntLiteral(1), 0, 0)),
          Some(success(Token.Semicolon, 12, 12)),
          Some(success(Token.IntLiteral(2), 13, 13))
        )
      }

      it("should NOT infer semicolon when an explicit semicolon is present on the same line") {
        check("val x = 1; val y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Val, 11, 13)),
          Some(success(Token.Identifier.Lower("y"), 15, 15)),
          Some(success(Token.Eq, 17, 17)),
          Some(success(Token.IntLiteral(2), 19, 19))
        )
      }

      it("should NOT infer an additional semicolon when an explicit one is followed by a newline") {
        check("val x = 1;\nval y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Val, 11, 13)),
          Some(success(Token.Identifier.Lower("y"), 15, 15)),
          Some(success(Token.Eq, 17, 17)),
          Some(success(Token.IntLiteral(2), 19, 19))
        )
      }

      it("should handle mixed explicit and inferred semicolons") {
        check("val x = 1; val y = 2\nval z = 3",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Val, 11, 13)),
          Some(success(Token.Identifier.Lower("y"), 15, 15)),
          Some(success(Token.Eq, 17, 17)),
          Some(success(Token.IntLiteral(2), 19, 19)),
          Some(success(Token.Semicolon, 20, 20)),
          Some(success(Token.Val, 21, 23)),
          Some(success(Token.Identifier.Lower("z"), 25, 25)),
          Some(success(Token.Eq, 27, 27)),
          Some(success(Token.IntLiteral(3), 29, 29))
        )
      }

      it("should handle explicit semicolon before a comment and newline") {
        check("val x = 1; // comment\nval y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Val, 22, 24)),
          Some(success(Token.Identifier.Lower("y"), 26, 26)),
          Some(success(Token.Eq, 28, 28)),
          Some(success(Token.IntLiteral(2), 30, 30))
        )
      }

      it("should handle multiple explicit semicolons on the same line") {
        check("val x = 1;; val y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Semicolon, 10, 10)),
          Some(success(Token.Val, 12, 14)),
          Some(success(Token.Identifier.Lower("y"), 16, 16)),
          Some(success(Token.Eq, 18, 18)),
          Some(success(Token.IntLiteral(2), 20, 20))
        )
      }

      it("should handle multiple explicit semicolons split by a newline") {
        check("val x = 1;\n;val y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Semicolon, 11, 11)),
          Some(success(Token.Val, 12, 14)),
          Some(success(Token.Identifier.Lower("y"), 16, 16)),
          Some(success(Token.Eq, 18, 18)),
          Some(success(Token.IntLiteral(2), 20, 20))
        )
      }

      it("should handle inferred semicolon followed by an explicit one") {
        check("val x = 1\n; val y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 10, 10)),
          Some(success(Token.Val, 12, 14)),
          Some(success(Token.Identifier.Lower("y"), 16, 16)),
          Some(success(Token.Eq, 18, 18)),
          Some(success(Token.IntLiteral(2), 20, 20))
        )
      }

      it("should handle multiple explicit semicolons separated by newlines") {
        check("val x = 1;\n;\nval y = 2",
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Eq, 6, 6)),
          Some(success(Token.IntLiteral(1), 8, 8)),
          Some(success(Token.Semicolon, 9, 9)),
          Some(success(Token.Semicolon, 11, 11)),
          Some(success(Token.Val, 13, 15)),
          Some(success(Token.Identifier.Lower("y"), 17, 17)),
          Some(success(Token.Eq, 19, 19)),
          Some(success(Token.IntLiteral(2), 21, 21))
        )
      }

      it("should NOT infer semicolon when line starts with an operator (infix notation)") {
        check("1\n+ 2",
          Some(success(Token.IntLiteral(1), 0, 0)),
          Some(success(Token.Identifier.Operator("+"), 2, 2)),
          Some(success(Token.IntLiteral(2), 4, 4))
        )
      }

      it("should NOT infer semicolon when line starts with a dot") {
        check("foo\n.bar",
          Some(success(Token.Identifier.Lower("foo"), 0, 2)),
          Some(success(Token.Dot, 4, 4)),
          Some(success(Token.Identifier.Lower("bar"), 5, 7))
        )
      }

      it("should emit the semicolon at the same position in the source code the newline was encountered") {
        check(
          """val foo = 1
            |val bar = 2
            |""".stripMargin,
          Some(success(Token.Val, 0, 2)),
          Some(success(Token.Identifier.Lower("foo"), 4, 6)),
          Some(success(Token.Eq, 8, 8)),
          Some(success(Token.IntLiteral(1), 10, 10)),
          Some(success(Token.Semicolon, 11, 11)),
          Some(success(Token.Val, 12, 14)),
          Some(success(Token.Identifier.Lower("bar"), 16, 18)),
          Some(success(Token.Eq, 20, 20)),
          Some(success(Token.IntLiteral(2), 22, 22)),
        )
      }
    }

    it("collects multiple errors in a single pass") {
      val input = "val \u0001 123 . . 5"
      check(input,
        Some(success(Token.Val, 0, 2)),
        Some(failure(ScannerError.InvalidCharacter, 4, 4)),
        Some(success(Token.IntLiteral(123), 6, 8)),
        Some(success(Token.Dot, 10, 10)),
        Some(success(Token.Dot, 12, 12)),
        Some(success(Token.IntLiteral(5), 14, 14))
      )
    }

    it("handles unclosed string literal and resumes on next line") {
      val input =
        """val x = "hello world
          |val y = 2""".stripMargin

      // Position breakdown:
      // "val x = " is 0-7
      // "\"hello world" starts at index 8. It hits newline at index 20.
      // Literals.stringLiteral ungets the newline and returns error.
      // Resume skipCommentsAndWhitespace will find the newline at index 20.
      // It will then see "val y = 2" starting at index 21.

      check(input,
        Some(success(Token.Val, 0, 2)),
        Some(success(Token.Identifier.Lower("x"), 4, 4)),
        Some(success(Token.Eq, 6, 6)),
        Some(failure(ScannerError.UnclosedStringLiteral, 8, 19)),
        Some(success(Token.Semicolon, 20, 20)),
        Some(success(Token.Val, 21, 23)),
        Some(success(Token.Identifier.Lower("y"), 25, 25)),
        Some(success(Token.Eq, 27, 27)),
        Some(success(Token.IntLiteral(2), 29, 29))
      )
    }

    it("handles unclosed multi-line string literal") {
      val input =
        """val x = """ + "\"\"\"" +
          """hello
            |val y = 2""".stripMargin

      // Multi-line strings in Literals.scala currently scan until the end of input
      // if not closed. It should return a single error for the whole thing.

      check(input,
        Some(success(Token.Val, 0, 2)),
        Some(success(Token.Identifier.Lower("x"), 4, 4)),
        Some(success(Token.Eq, 6, 6)),
        Some(failure(ScannerError.UnclosedMultiLineString, 8, 25))
      )
    }

    it("handles unclosed quoted identifier and resumes on next line") {
      val input =
        """val `unclosed = 1
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
        Some(success(Token.Val, 0, 2)),
        // "val `unclosed = 1"
        // 01234567890123456
        Some(failure(ScannerError.UnclosedQuotedIdentifier, 4, 16)),
        // After Error, Scanner.get calls readNext -> skipCommentsAndWhitespace.
        // It skips the \n at 17.
        // readToken(Some(17)) sees "val y = 2" starting at 18.
        Some(success(Token.Semicolon, 17, 17)),
        Some(success(Token.Val, 18, 20)),
        Some(success(Token.Identifier.Lower("y"), 22, 22)),
        Some(success(Token.Eq, 24, 24)),
        Some(success(Token.IntLiteral(2), 26, 26))
      )
    }

    it("unclosed string with trailing backslash does not leak to next line") {
      val input =
        """val x = "unclosed \
          |val y = 2""".stripMargin

      check(input,
        Some(success(Token.Val, 0, 2)),
        Some(success(Token.Identifier.Lower("x"), 4, 4)),
        Some(success(Token.Eq, 6, 6)),
        // "unclosed \
        // starts at 8. It hits \ at 18.
        // It identifies the newline as a boundary and returns UnclosedStringLiteral error.
        Some(failure(ScannerError.UnclosedStringLiteral, 8, 18)),
        Some(success(Token.Semicolon, 19, 19)),
        Some(success(Token.Val, 20, 22)),
        Some(success(Token.Identifier.Lower("y"), 24, 24)),
        Some(success(Token.Eq, 26, 26)),
        Some(success(Token.IntLiteral(2), 28, 28))
      )
    }

    it("handles unclosed quoted identifier with trailing backslash does not leak to next line") {
      val input =
        """val `unclosed \
          |val y = 2""".stripMargin

      check(input,
        Some(success(Token.Val, 0, 2)),
        // `unclosed \
        // Similar to string above.
        Some(failure(ScannerError.UnclosedQuotedIdentifier, 4, 14)),
        Some(success(Token.Semicolon, 15, 15)),
        Some(success(Token.Val, 16, 18)),
        Some(success(Token.Identifier.Lower("y"), 20, 20)),
        Some(success(Token.Eq, 22, 22)),
        Some(success(Token.IntLiteral(2), 24, 24))
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
        Some(failure(ScannerError.UnclosedStringLiteral, 0, 10)),
        Some(success(Token.Semicolon, 11, 11)),
        Some(success(Token.Val, 12, 14)),
        Some(success(Token.Identifier.Lower("y"), 16, 16)),
        Some(success(Token.Eq, 18, 18)),
        Some(success(Token.IntLiteral(2), 20, 20))
      )
    }

    it("handles unclosed expression escape in multi-line interpolator") {
      val input = "val x = s\"\"\"hello ${ world\nval y = 2\n\"\"\"\nval z = 3"

      check(input,
        Some(success(Token.Val, 0, 2)),
        Some(success(Token.Identifier.Lower("x"), 4, 4)),
        Some(success(Token.Eq, 6, 6)),
        Some(success(Token.BeginMultiLineInterpolatedString("s"), 8, 11)),
        Some(success(Token.InterpolatedPart("hello "), 12, 17)),
        Some(success(Token.BeginInterpolatedEscape, 18, 19)),
        Some(success(Token.Identifier.Lower("world"), 21, 25)),
        Some(success(Token.Semicolon, 26, 26)),
        Some(success(Token.Val, 27, 29)),
        Some(success(Token.Identifier.Lower("y"), 31, 31)),
        Some(success(Token.Eq, 33, 33)),
        Some(success(Token.IntLiteral(2), 35, 35)),
        Some(success(Token.Semicolon, 36, 36)),
        // The closing """ is at index 37-39.
        // Robust behavior: forced exit and report error.
        Some(failure(ScannerError.UnclosedMultiLineString, 37, 39)),
        Some(success(Token.Semicolon, 40, 40)),
        Some(success(Token.Val, 41, 43)),
        Some(success(Token.Identifier.Lower("z"), 45, 45)),
        Some(success(Token.Eq, 47, 47)),
        Some(success(Token.IntLiteral(3), 49, 49))
      )
    }

    it("handles malformed hex literal and observes resume point") {
      // 0xG should be one error, then resume
      val input = "0xG val"
      check(input,
        Some(failure(ScannerError.InvalidLiteralNumber, 0, 2)), // Consumes 'G'
        Some(success(Token.Val, 4, 6))
      )
    }

    it("handles malformed decimal literal and observes resume point") {
      // 1.2.3 should be 1.2, then .3 (current behavior)
      val input = "1.2.3 val"
      check(input,
        Some(success(Token.DoubleLiteral(1.2), 0, 2)),
        Some(success(Token.DoubleLiteral(0.3), 3, 4)),
        Some(success(Token.Val, 6, 8))
      )
    }

    it("handles illegal separator in numeric literal and observes resume point") {
      // 123_ followed by space and val.
      // Strict behavior: The whole 123_ is a failed number literal.
      val input = "123_ val"
      check(input,
        Some(failure(ScannerError.IllegalSeparator, 3, 3)),
        Some(success(Token.Val, 5, 7))
      )
    }

    it("handles malformed exponent and observes resume point") {
      // 1e+ followed by space and val.
      // Strict behavior: 1e is a failed number literal. The + should be preserved.
      val input = "1e+ val"
      check(input,
        Some(failure(ScannerError.InvalidLiteralNumber, 0, 1)),
        Some(success(Token.Identifier.Operator("+"), 2, 2)),
        Some(success(Token.Val, 4, 6))
      )
    }

    describe("garbage clustering") {
      it("groups invalid character clusters into a single error") {
        val input = "\u0001\u0002\u0003 val"
        // Goal: One error for the cluster, then resume at 'val'
        check(input,
          Some(failure(ScannerError.InvalidCharacter, 0, 2)),
          Some(success(Token.Val, 4, 6))
        )
      }

      it("groups invalid character clusters even without trailing whitespace") {
        val input = "\u0001\u0002\u0003val"
        // Goal: Group garbage, then resume immediately at identifier 'val'
        check(input,
          Some(failure(ScannerError.InvalidCharacter, 0, 2)),
          Some(success(Token.Val, 3, 5))
        )
      }

      it("garbage cluster grouping stops at newlines") {
        val input = "\u0001\u0002\n\u0003\u0004"
        // Garbage should be grouped per-line to preserve newline synchronization
        check(input,
          Some(failure(ScannerError.InvalidCharacter, 0, 1)),
          Some(failure(ScannerError.InvalidCharacter, 3, 4))
        )
      }

      it("garbage cluster grouping stops at delimiters") {
        val input = "\u0001\u0002(123)"
        // Garbage should not 'eat' the opening parenthesis
        check(input,
          Some(failure(ScannerError.InvalidCharacter, 0, 1)),
          Some(success(Token.LParen, 2, 2)),
          Some(success(Token.IntLiteral(123), 3, 5)),
          Some(success(Token.RParen, 6, 6))
        )
      }

      it("groups invalid character clusters inside interpolation escapes") {
        val input = "s\"${\u0001\u0002}\""
        // Garbage inside ${ } should be grouped without breaking the interpolation state
        check(input,
          Some(success(Token.BeginInterpolatedString("s"), 0, 1)),
          Some(success(Token.BeginInterpolatedEscape, 2, 3)),
          Some(failure(ScannerError.InvalidCharacter, 4, 5)),
          Some(success(Token.EndInterpolatedEscape, 6, 6)),
          Some(success(Token.EndInterpolatedString, 7, 7))
        )
      }

      it("garbage cluster grouping interacts correctly with comments") {
        // Example A: Garbage followed by a comment
        // The grouping must stop at the '/' so the comment scanner can see it.
        val inputA = "\u0001\u0002// comment\nval x = 1"
        check(inputA,
          Some(failure(ScannerError.InvalidCharacter, 0, 1)),
          Some(success(Token.Val, 13, 15)),
          Some(success(Token.Identifier.Lower("x"), 17, 17)),
          Some(success(Token.Eq, 19, 19)),
          Some(success(Token.IntLiteral(1), 21, 21))
        )

        // Example B: Garbage following a block comment
        val inputB = "/* comment */\u0003\u0004 val"
        check(inputB,
          Some(failure(ScannerError.InvalidCharacter, 13, 14)),
          Some(success(Token.Val, 16, 18))
        )
      }

      it("garbage cluster grouping stops at dots (number or field access)") {
        // Case A: Followed by number
        check("\u0001.123",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.DoubleLiteral(0.123), 1, 4))
        )

        // Case B: Followed by field access
        check("\u0001.foo",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.Dot, 1, 1)),
          Some(success(Token.Identifier.Lower("foo"), 2, 4))
        )
      }

      it("treats unrecognized Unicode symbols as operators (Scala-style)") {
        // Snowman (\u2603) and Comet (\u2604) are valid operators in Scala/Scaletta
        val input = "\u2603\u2604 val"
        check(input,
          Some(success(Token.Identifier.Operator("\u2603\u2604"), 0, 1)),
          Some(success(Token.Val, 3, 5))
        )
      }

      it("groups truly invalid characters (e.g. control chars) as garbage clusters") {
        // Using non-whitespace control characters that aren't operators or identifiers
        val input = "\u0001\u0002\u0003 val"
        check(input,
          Some(failure(ScannerError.InvalidCharacter, 0, 2)),
          Some(success(Token.Val, 4, 6))
        )
      }

      it("garbage cluster grouping stops at literal delimiters") {
        // Case A: Followed by string
        check("\u0001\"hello\"",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.StringLiteral("hello"), 1, 7))
        )

        // Case B: Followed by character
        check("\u0001'a'",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.CharLiteral('a'), 1, 3))
        )
      }

      it("garbage cluster grouping interacts correctly with interpolation starts: single line") {
        check("\u0001s\"hello\"",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.BeginInterpolatedString("s"), 1, 2)),
          Some(success(Token.InterpolatedPart("hello"), 3, 7)),
          Some(success(Token.EndInterpolatedString, 8, 8))
        )
      }

      it("garbage cluster grouping interacts correctly with interpolation starts: multi-line") {
        check("\u0001raw\"\"\"hello\"\"\"",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.BeginMultiLineInterpolatedString("raw"), 1, 6)),
          Some(success(Token.InterpolatedPart("hello"), 7, 11)),
          Some(success(Token.EndInterpolatedString, 12, 14))
        )
      }

      it("garbage cluster grouping hand-off to complex identifiers") {
        // This test specifically targets the hand-off between garbage and multi-character identifiers
        // that are NOT interpolators.
        check("\u0001abc",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.Identifier.Lower("abc"), 1, 3))
        )
      }

      it("garbage followed by '+'") {
        // If we have redundant unget, this will fail or return shifted tokens
        check("\u0001+",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.Identifier.Operator("+"), 1, 1))
        )
      }

      it("handles garbage followed by '-' and then a digit") {
        // This tests the case where '-' is ambiguous until we see the digit
        check("\u0001-123",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.IntLiteral(-123), 1, 4))
        )
      }

      it("handles garbage followed by a failed interpolator lookahead") {
        // 'raw' followed by a space is NOT an interpolator.
        // It should be scanned as an identifier after the garbage error.
        check("\u0001raw ",
          Some(failure(ScannerError.InvalidCharacter, 0, 0)),
          Some(success(Token.Identifier.Lower("raw"), 1, 3))
        )
      }

      it("handles garbage in the middle of a partial numeric lookahead") {
        // '1.' followed by garbage should not eat the garbage.
        // In Scaletta, '1.' currently scans as an IntLiteral(1) followed by a Dot.
        check("1.\u0001",
          Some(success(Token.IntLiteral(1), 0, 0)),
          Some(success(Token.Dot, 1, 1)),
          Some(failure(ScannerError.InvalidCharacter, 2, 2))
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
          """{
            |  val name = "world"
            |  val msg = s"Hello, ${name.toUpperCase}!"
            |  val result = (1 + 2) * 3
            |  msg + " " + result.toString
            |}""".stripMargin
        check(input,
          Some(success(Token.LBrace, 0, 0)),
          Some(success(Token.Val, 4, 6)),
          Some(success(Token.Identifier.Lower("name"), 8, 11)),
          Some(success(Token.Eq, 13, 13)),
          Some(success(Token.StringLiteral("world"), 15, 21)),
          Some(success(Token.Semicolon, 22, 22)),
          Some(success(Token.Val, 25, 27)),
          Some(success(Token.Identifier.Lower("msg"), 29, 31)),
          Some(success(Token.Eq, 33, 33)),
          Some(success(Token.BeginInterpolatedString("s"), 35, 36)),
          Some(success(Token.InterpolatedPart("Hello, "), 37, 43)),
          Some(success(Token.BeginInterpolatedEscape, 44, 45)),
          Some(success(Token.Identifier.Lower("name"), 46, 49)),
          Some(success(Token.Dot, 50, 50)),
          Some(success(Token.Identifier.Lower("toUpperCase"), 51, 61)),
          Some(success(Token.EndInterpolatedEscape, 62, 62)),
          Some(success(Token.InterpolatedPart("!"), 63, 63)),
          Some(success(Token.EndInterpolatedString, 64, 64)),
          Some(success(Token.Semicolon, 65, 65)),
          Some(success(Token.Val, 68, 70)),
          Some(success(Token.Identifier.Lower("result"), 72, 77)),
          Some(success(Token.Eq, 79, 79)),
          Some(success(Token.LParen, 81, 81)),
          Some(success(Token.IntLiteral(1), 82, 82)),
          Some(success(Token.Identifier.Operator("+"), 84, 84)),
          Some(success(Token.IntLiteral(2), 86, 86)),
          Some(success(Token.RParen, 87, 87)),
          Some(success(Token.Identifier.Operator("*"), 89, 89)),
          Some(success(Token.IntLiteral(3), 91, 91)),
          Some(success(Token.Semicolon, 92, 92)),
          Some(success(Token.Identifier.Lower("msg"), 95, 97)),
          Some(success(Token.Identifier.Operator("+"), 99, 99)),
          Some(success(Token.StringLiteral(" "), 101, 103)),
          Some(success(Token.Identifier.Operator("+"), 105, 105)),
          Some(success(Token.Identifier.Lower("result"), 107, 112)),
          Some(success(Token.Dot, 113, 113)),
          Some(success(Token.Identifier.Lower("toString"), 114, 121)),
          Some(success(Token.RBrace, 123, 123))
        )
      }

      it("case 2") {
        val input =
          """if (x > 0) {
            |  s"Positive: $x"
            |} else if (x < 0) {
            |  s"Negative: $x"
            |} else {
            |  "Zero"
            |}""".stripMargin
        check(input,
          Some(success(Token.If, 0, 1)),
          Some(success(Token.LParen, 3, 3)),
          Some(success(Token.Identifier.Lower("x"), 4, 4)),
          Some(success(Token.Identifier.Operator(">"), 6, 6)),
          Some(success(Token.IntLiteral(0), 8, 8)),
          Some(success(Token.RParen, 9, 9)),
          Some(success(Token.LBrace, 11, 11)),
          Some(success(Token.BeginInterpolatedString("s"), 15, 16)),
          Some(success(Token.InterpolatedPart("Positive: "), 17, 26)),
          Some(success(Token.Identifier.Lower("x"), 28, 28)),
          Some(success(Token.EndInterpolatedString, 29, 29)),
          Some(success(Token.RBrace, 31, 31)),
          Some(success(Token.Else, 33, 36)),
          Some(success(Token.If, 38, 39)),
          Some(success(Token.LParen, 41, 41)),
          Some(success(Token.Identifier.Lower("x"), 42, 42)),
          Some(success(Token.Identifier.Operator("<"), 44, 44)),
          Some(success(Token.IntLiteral(0), 46, 46)),
          Some(success(Token.RParen, 47, 47)),
          Some(success(Token.LBrace, 49, 49)),
          Some(success(Token.BeginInterpolatedString("s"), 53, 54)),
          Some(success(Token.InterpolatedPart("Negative: "), 55, 64)),
          Some(success(Token.Identifier.Lower("x"), 66, 66)),
          Some(success(Token.EndInterpolatedString, 67, 67)),
          Some(success(Token.RBrace, 69, 69)),
          Some(success(Token.Else, 71, 74)),
          Some(success(Token.LBrace, 76, 76)),
          Some(success(Token.StringLiteral("Zero"), 80, 85)),
          Some(success(Token.RBrace, 87, 87))
        )
      }

      it("case 3") {
        val input =
          """res match {
            |  case s"foo$x" => s"Matched foo with $x"
            |  case _ =>
            |    val fallback = "none"
            |    fallback
            |}""".stripMargin
        check(input,
          Some(success(Token.Identifier.Lower("res"), 0, 2)),
          Some(success(Token.Match, 4, 8)),
          Some(success(Token.LBrace, 10, 10)),
          Some(success(Token.Case, 14, 17)),
          Some(success(Token.BeginInterpolatedString("s"), 19, 20)),
          Some(success(Token.InterpolatedPart("foo"), 21, 23)),
          Some(success(Token.Identifier.Lower("x"), 25, 25)),
          Some(success(Token.EndInterpolatedString, 26, 26)),
          Some(success(Token.RDoubleArrow, 28, 29)),
          Some(success(Token.BeginInterpolatedString("s"), 31, 32)),
          Some(success(Token.InterpolatedPart("Matched foo with "), 33, 49)),
          Some(success(Token.Identifier.Lower("x"), 51, 51)),
          Some(success(Token.EndInterpolatedString, 52, 52)),
          Some(success(Token.Semicolon, 53, 53)),
          Some(success(Token.Case, 56, 59)),
          Some(success(Token.Underscore, 61, 61)),
          Some(success(Token.RDoubleArrow, 63, 64)),
          Some(success(Token.Val, 70, 72)),
          Some(success(Token.Identifier.Lower("fallback"), 74, 81)),
          Some(success(Token.Eq, 83, 83)),
          Some(success(Token.StringLiteral("none"), 85, 90)),
          Some(success(Token.Semicolon, 91, 91)),
          Some(success(Token.Identifier.Lower("fallback"), 96, 103)),
          Some(success(Token.RBrace, 105, 105))
        )
      }
    }
  }

  private def check(input: String,
                    expectedTokens: Option[Pos[Token]]*)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { reader =>
      val scanner = Scanner.create(reader, IdentifierPolicy.Default)
      val actualTokens = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
      val expectedPosList = expectedTokens.collect { case Some(p) => p }.toVector

      def formatToken(p: Pos[Token]): String = s"${p.value} at ${p.begin}:${p.end}"

      def renderUnderline(input: String, index: Int, label: String): String = {
        val lines = input.split("\n")
        var currentIdx = 0
        val result = new StringBuilder()
        var found = false
        lines.foreach { line =>
          if (!found && index >= currentIdx && index <= currentIdx + line.length) {
            result.append(line).append("\n")
            val padding = " " * (index - currentIdx)
            result.append(padding).append("^--- ").append(label).append("\n")
            found = true
          }
          currentIdx += line.length + 1 // +1 for newline
        }
        result.toString()
      }

      val maxLength = Math.max(actualTokens.length, expectedPosList.length)
      for (i <- 0 until maxLength) {
        if (i >= expectedPosList.length) {
          val actual = actualTokens(i)
          fail(s"Unexpected extra token at index $i: ${formatToken(actual)}\n${renderUnderline(input, actual.begin.value, "extra token")}")
        } else if (i >= actualTokens.length) {
          val expected = expectedPosList(i)
          fail(s"Expected more tokens, but stream ended. Missing: ${formatToken(expected)}\n${renderUnderline(input, expected.begin.value, "missing expected token")}")
        } else {
          val actual = actualTokens(i)
          val expected = expectedPosList(i)

          if (actual.value != expected.value) {
            fail(s"Token mismatch at index $i:\nExpected: ${formatToken(expected)}\nActual:   ${formatToken(actual)}\n" +
              s"Context:\n${renderUnderline(input, expected.begin.value, "expected " + expected.value)}\n" +
              s"${renderUnderline(input, actual.begin.value, "actual " + actual.value)}")
          }

          if (actual.positionTuple != expected.positionTuple) {
            fail(s"Position mismatch for token '${actual.value}' at index $i:\nExpected: ${expected.begin}:${expected.end}\nActual:   ${actual.begin}:${actual.end}\n" +
              s"Context:\n${renderUnderline(input, expected.begin.value, "expected start")}\n" +
              s"${renderUnderline(input, actual.begin.value, "actual start")}")
          }
        }
      }
    }
  }
}
