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

    describe("complex expressions") {
      ignore("should handle a complex block with val definitions, interpolation, and method calls") {
        val input =
          """{
            |  val name = "world"
            |  val msg = s"Hello, ${name.toUpperCase}!"
            |  val result = (1 + 2) * 3
            |  msg + " " + result.toString
            |}""".stripMargin
        check(input,
          Some(success(Token.LBrace, 0, 0)),
          Some(success(Token.Val, 5, 7)),
          Some(success(Token.Identifier.Lower("name"), 9, 12)),
          Some(success(Token.Eq, 14, 14)),
          Some(success(Token.StringLiteral("world"), 16, 22)),
          Some(success(Token.Semicolon, 23, 23)),
          Some(success(Token.Val, 27, 29)),
          Some(success(Token.Identifier.Lower("msg"), 31, 33)),
          Some(success(Token.Eq, 35, 35)),
          Some(success(Token.BeginInterpolatedString("s"), 37, 38)),
          Some(success(Token.InterpolatedPart("Hello, "), 39, 45)),
          Some(success(Token.BeginInterpolatedEscape, 46, 47)),
          Some(success(Token.Identifier.Lower("name"), 48, 51)),
          Some(success(Token.Dot, 52, 52)),
          Some(success(Token.Identifier.Lower("toUpperCase"), 53, 63)),
          Some(success(Token.EndInterpolatedEscape, 64, 64)),
          Some(success(Token.InterpolatedPart("!"), 65, 65)),
          Some(success(Token.EndInterpolatedString, 66, 66)),
          Some(success(Token.Semicolon, 67, 67)),
          Some(success(Token.Val, 71, 73)),
          Some(success(Token.Identifier.Lower("result"), 75, 80)),
          Some(success(Token.Eq, 82, 82)),
          Some(success(Token.LParen, 84, 84)),
          Some(success(Token.IntLiteral(1), 85, 85)),
          Some(success(Token.Identifier.Operator("+"), 87, 87)),
          Some(success(Token.IntLiteral(2), 89, 89)),
          Some(success(Token.RParen, 90, 90)),
          Some(success(Token.Identifier.Operator("*"), 92, 92)),
          Some(success(Token.IntLiteral(3), 94, 95)),
          Some(success(Token.Semicolon, 95, 95)),
          Some(success(Token.Identifier.Lower("msg"), 99, 101)),
          Some(success(Token.Identifier.Operator("+"), 103, 103)),
          Some(success(Token.StringLiteral(" "), 105, 107)),
          Some(success(Token.Identifier.Operator("+"), 109, 109)),
          Some(success(Token.Identifier.Lower("result"), 111, 116)),
          Some(success(Token.Dot, 117, 117)),
          Some(success(Token.Identifier.Lower("toString"), 118, 125)),
          Some(success(Token.Semicolon, 126, 126)),
          Some(success(Token.RBrace, 127, 127))
        )
      }

      ignore("should handle nested if-then-else with interpolation") {
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
          Some(success(Token.BeginInterpolatedString("s"), 17, 18)),
          Some(success(Token.InterpolatedPart("Positive: "), 19, 28)),
          Some(success(Token.Identifier.Lower("x"), 30, 30)),
          Some(success(Token.EndInterpolatedString, 31, 31)),
          Some(success(Token.Semicolon, 32, 32)),
          Some(success(Token.RBrace, 33, 33)),
          Some(success(Token.Else, 35, 38)),
          Some(success(Token.If, 40, 41)),
          Some(success(Token.LParen, 43, 43)),
          Some(success(Token.Identifier.Lower("x"), 44, 44)),
          Some(success(Token.Identifier.Operator("<"), 46, 46)),
          Some(success(Token.IntLiteral(0), 48, 48)),
          Some(success(Token.RParen, 49, 49)),
          Some(success(Token.LBrace, 51, 51)),
          Some(success(Token.BeginInterpolatedString("s"), 57, 58)),
          Some(success(Token.InterpolatedPart("Negative: "), 59, 68)),
          Some(success(Token.Identifier.Lower("x"), 70, 70)),
          Some(success(Token.EndInterpolatedString, 71, 71)),
          Some(success(Token.Semicolon, 72, 72)),
          Some(success(Token.RBrace, 73, 73)),
          Some(success(Token.Else, 75, 78)),
          Some(success(Token.LBrace, 80, 80)),
          Some(success(Token.StringLiteral("Zero"), 86, 91)),
          Some(success(Token.Semicolon, 92, 92)),
          Some(success(Token.RBrace, 93, 93))
        )
      }

      ignore("should handle pattern matching with interpolation and multiline strings") {
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
          Some(success(Token.Case, 16, 19)),
          Some(success(Token.BeginInterpolatedString("s"), 21, 22)),
          Some(success(Token.InterpolatedPart("foo"), 23, 25)),
          Some(success(Token.Identifier.Lower("x"), 27, 27)),
          Some(success(Token.EndInterpolatedString, 28, 28)),
          Some(success(Token.RDoubleArrow, 30, 31)),
          Some(success(Token.BeginInterpolatedString("s"), 33, 34)),
          Some(success(Token.InterpolatedPart("Matched foo with "), 35, 51)),
          Some(success(Token.Identifier.Lower("x"), 53, 53)),
          Some(success(Token.EndInterpolatedString, 54, 54)),
          Some(success(Token.Semicolon, 55, 55)),
          Some(success(Token.Case, 60, 63)),
          Some(success(Token.Underscore, 65, 65)),
          Some(success(Token.RDoubleArrow, 67, 68)),
          Some(success(Token.Val, 77, 79)),
          Some(success(Token.Identifier.Lower("fallback"), 81, 88)),
          Some(success(Token.Eq, 90, 90)),
          Some(success(Token.StringLiteral("none"), 92, 97)),
          Some(success(Token.Semicolon, 98, 98)),
          Some(success(Token.Identifier.Lower("fallback"), 105, 112)),
          Some(success(Token.Semicolon, 113, 113)),
          Some(success(Token.RBrace, 114, 114))
        )
      }
    }
  }

  private def check(input: String,
                    expectedTokens: Option[Pos[Either[ScannerError, Token]]]*)
                   (implicit pos: Position): Unit = {
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
                    withClue(s"Token $expectedToken at ${actualPos.begin}") {
                      actualPos.value shouldBe expectedToken
                      if (actualPos.positionTuple != expectedPos.positionTuple) {
                        fail(s"Expected begin:end ${expectedPos.positionTuple} but got ${actualPos.positionTuple}")
                      }
                    }
                  case Left(expectedError) =>
                    fail(s"Expected error $expectedError, but got success with ${actualPos.value} at ${actualPos.begin}")
                }
              case ScannerResult.Error(actualPos) =>
                expectedPos.value match {
                  case Left(expectedError) =>
                    withClue(s"Error $expectedError at ${actualPos.begin}") {
                      actualPos.value shouldBe expectedError
                      actualPos.begin shouldBe expectedPos.begin
                      actualPos.end shouldBe expectedPos.end
                    }
                  case Right(expectedToken) =>
                    fail(s"Expected success with $expectedToken, but got error ${actualPos.value} at ${actualPos.begin}")
                }
              case ScannerResult.EndOfInput =>
                fail(s"Expected more tokens, but got EndOfInput (expected ${expectedPos.value} at ${expectedPos.begin})")
            }
          case None =>
            actual shouldBe ScannerResult.EndOfInput
        }
      }
      val extra = scanner.get()
      if (extra != ScannerResult.EndOfInput) {
        fail(s"Expected EndOfInput, but got extra token: $extra")
      }
    }
  }
}
