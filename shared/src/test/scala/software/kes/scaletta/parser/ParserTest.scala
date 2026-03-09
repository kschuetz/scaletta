package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.ast.AstBuilders._
import software.kes.scaletta.scanner.Token
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ParserTest extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._
  import software.kes.scaletta.testsupport.ParseErrorMatchers.{atIndex => errorAtIndex, _}
  import software.kes.scaletta.testsupport.ParseWarningMatchers._

  describe("Parser") {
    describe("Simple Expressions") {
      it("should parse various literals and identifiers correctly") {
        val simpleExpressions = Table(
          ("input", "expectedAst"),
          ("123", lit(123)),
          ("\"hello\"", lit("hello")),
          ("true", lit(true)),
          ("false", lit(false)),
          ("null", litNull),
          ("foo", ref("foo"))
        )

        forAll(simpleExpressions) { (input, expected) =>
          input shouldParseTo expected
        }
      }
    }

    describe("Expressions") {
      describe("Arithmetic & Precedence") {
        it("should respect operator precedence and grouping") {
          val arithmeticTests = Table(
            ("input", "expectedAst"),
            ("1 + 2", infix(lit(1), "+", lit(2))),
            ("1 + 2 * 3", infix(lit(1), "+", infix(lit(2), "*", lit(3)))),
            ("1 * 2 + 3", infix(infix(lit(1), "*", lit(2)), "+", lit(3))),
            ("(1 + 2) * 3", infix(infix(lit(1), "+", lit(2)), "*", lit(3)))
          )

          forAll(arithmeticTests) { (input, expected) =>
            input shouldParseTo expected
          }
        }
      }

      describe("Function Calls") {
        it("should parse a standard function call (f(x, y))") {
          "f(x, y)" shouldParseTo call(ref("f"), ref("x"), ref("y"))
        }

        it("should parse a function call with no arguments (f())") {
          "f()" shouldParseTo call(ref("f"))
        }

        it("should parse a nested function call (f(g(x)))") {
          "f(g(x))" shouldParseTo call(ref("f"), call(ref("g"), ref("x")))
        }

        it("should parse a call on an expression target ((f + g)(x))") {
          "(f + g)(x)" shouldParseTo call(infix(ref("f"), "+", ref("g")), ref("x"))
        }

        it("should parse multiple argument groups (f(x)(y))") {
          "f(x)(y)" shouldParseTo multiCall(ref("f"), Vector(Vector(ref("x")), Vector(ref("y"))))
        }

        it("should parse an alphanumeric infix operator (a plus b)") {
          "a plus b" shouldParseWithWarnings (ParseWarning.SuspiciousInfixExpression("plus") at 2) producing {
            infix(ref("a"), "plus", ref("b"))
          }
        }
      }
    }

    describe("Scanner Exhaustion") {
      it("should fail if there is trailing garbage") {
        "123 garbage" shouldFailWith (ParseError.ExtraToken(Token.Identifier.Lower("garbage"), "end of input") at 4) producing {
          lit(123)
        }
      }

      it("should allow trailing garbage when using shouldParsePartiallyTo") {
        "123 garbage" shouldParsePartiallyTo lit(123)
      }
    }
  }

  describe("Parser Error Recovery") {
    it("should handle an unclosed parenthesis in an expression") {
      "(1 + 2" shouldRecoverWith (ParseError.UnclosedDelimiter(Token.LParen, Token.RParen) at 0) producing {
        infix(lit(1), "+", lit(2))
      }
    }

    it("should handle an unclosed parenthesis in a function call") {
      "f(1, 2" shouldRecoverWith (ParseError.UnclosedDelimiter(Token.LParen, Token.RParen) at 1) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should handle a missing expression in an argument list") {
      "f(1, , 2)" shouldRecoverWith (ParseError.MissingExpression("argument") at 5) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should recover from an unexpected token in a function call") {
      "f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should collect multiple errors in a function call") {
      "f(1, @, #, 2)" shouldRecoverWith(
        ParseError.UnexpectedToken(Token.At) at 5,
        ParseError.UnexpectedToken(Token.Hash) at 8
      ) producing {
        call(ref("f"), lit(1), lit(2))
      }
    }

    it("should handle an unexpected token at the start of an argument") {
      "f(@, 1)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 2) producing {
        call(ref("f"), lit(1))
      }
    }

    describe("Flexible Error Matchers") {
      it("should support containErrorOfType") {
        "f(1, @, 2)" shouldRecoverWith containErrorOfType[ParseError.UnexpectedToken]
      }

      it("should support atIndex with errorOfType") {
        "f(1, @, #, 2)" shouldRecoverWith {
          errorAtIndex(0)(errorOfType[ParseError.UnexpectedToken]) and
            errorAtIndex(1)(errorOfType[ParseError.UnexpectedToken])
        }
      }

      it("should support soft failure check (ignoringAst)") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .ignoringAst()
      }

      it("should support multiple assertions with chaining") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .producing(call(ref("f"), lit(1), lit(2)))
          .andNoFatalErrors()
      }

      it("should support withPartialAst for custom assertions") {
        ("f(1, @, 2)" shouldRecoverWith (ParseError.UnexpectedToken(Token.At) at 5))
          .withPartialAst { ast =>
            ast shouldBe call(ref("f"), lit(1), lit(2))
          }
      }
    }
  }

}
