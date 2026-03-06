package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.ast.AstBuilders._
import software.kes.scaletta.scanner.Token
import software.kes.scaletta.testsupport.ParserTestSupport

class ParserTest extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private val support = new ParserTestSupport() with Matchers

  import support._

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
          parseValue(input) shouldBe expected
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
            parseValue(input) shouldBe expected
          }
        }
      }

      describe("Function Calls") {
        it("should parse a standard function call (f(x, y))") {
          parseValue("f(x, y)") shouldBe call(ref("f"), ref("x"), ref("y"))
        }

        it("should parse a function call with no arguments (f())") {
          parseValue("f()") shouldBe call(ref("f"))
        }

        it("should parse a nested function call (f(g(x)))") {
          parseValue("f(g(x))") shouldBe call(ref("f"), call(ref("g"), ref("x")))
        }

        it("should parse a call on an expression target ((f + g)(x))") {
          parseValue("(f + g)(x)") shouldBe call(infix(ref("f"), "+", ref("g")), ref("x"))
        }

        it("should parse multiple argument groups (f(x)(y))") {
          parseValue("f(x)(y)") shouldBe multiCall(ref("f"), Vector(Vector(ref("x")), Vector(ref("y"))))
        }
      }
    }
  }

  describe("Parser Error Recovery") {
    it("should recover from an unexpected token in a function call") {
      val (ast, errors) = parseWithErrors("f(1, @, 2)")
      ast shouldBe Some(call(ref("f"), lit(1), lit(2)))
      errors should have size 1
      errors.head shouldBe ParseError.UnexpectedToken(Token.At)
    }

    it("should collect multiple errors in a function call") {
      val (ast, errors) = parseWithErrors("f(1, @, #, 2)")
      ast shouldBe Some(call(ref("f"), lit(1), lit(2)))
      errors should have size 2
      errors(0) shouldBe ParseError.UnexpectedToken(Token.At)
      errors(1) shouldBe ParseError.UnexpectedToken(Token.Hash)
    }

    it("should handle an unexpected token at the start of an argument") {
      val (ast, errors) = parseWithErrors("f(@, 1)")
      ast shouldBe Some(call(ref("f"), lit(1)))
      errors should have size 1
      errors.head shouldBe ParseError.UnexpectedToken(Token.At)
    }
  }

}
