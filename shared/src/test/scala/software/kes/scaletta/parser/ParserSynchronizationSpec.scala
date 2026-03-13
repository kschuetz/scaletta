package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.ast.AstBuilders._
import software.kes.scaletta.scanner.Token
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ParserSynchronizationSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._
  import software.kes.scaletta.testsupport.ParseErrorMatchers._

  describe("Parser Synchronization") {

    describe("Nested Calls") {
      it("should recover from an error in a nested function call") {
        "f(1, g(2, @, 3), 4)" shouldRecoverWith (
          ParseError.UnexpectedToken(Token.At) at 10
          ) producing {
          callSimple(ref("f"), lit(1), callSimple(ref("g"), lit(2), lit(3)), lit(4))
        }
      }
    }

    describe("Multiple Errors") {
      it("should recover from multiple independent errors in the same argument list") {
        "f(@, 1, #, 2)" shouldRecoverWith(
          ParseError.UnexpectedToken(Token.At) at 2,
          ParseError.UnexpectedToken(Token.Hash) at 8
        ) producing {
          callSimple(ref("f"), lit(1), lit(2))
        }
      }
    }

    describe("Boundary Proximity") {
      it("should recover at the closing parenthesis when error is at the end of the list") {
        "f(1, @)" shouldRecoverWith (
          ParseError.UnexpectedToken(Token.At) at 5
          ) producing {
          callSimple(ref("f"), lit(1))
        }
      }

      it("should recover at the closing parenthesis when error is the only argument") {
        "f(@)" shouldRecoverWith (
          ParseError.UnexpectedToken(Token.At) at 2
          ) producing {
          callSimple(ref("f"))
        }
      }
    }

    describe("Interleaved Valid/Invalid Code") {
      it("should extract all valid arguments when interleaved with errors") {
        "f(1, @, 2, #, 3)" shouldRecoverWith(
          ParseError.UnexpectedToken(Token.At) at 5,
          ParseError.UnexpectedToken(Token.Hash) at 11
        ) producing {
          callSimple(ref("f"), lit(1), lit(2), lit(3))
        }
      }
    }

    describe("Structural Boundaries") {
      it("should recover at structural boundaries (like val)") {
        "f(1, val x = 2, 3)" shouldRecoverWith (
          ParseError.UnexpectedToken(Token.Val) at 5,
          ParseError.UnexpectedToken(Token.Identifier.Lower("x")) at 9
          ) producing {
          callSimple(ref("f"), lit(1), lit(3))
        }
      }

      it("should recover at structural boundaries (like def)") {
        "f(1, def f(x), 3)" shouldFailWith(
          ParseError.UnexpectedToken(Token.Def) at 5,
          ParseError.UnexpectedToken(Token.Identifier.Lower("f")) at 9,
          ParseError.ExtraToken(Token.Comma, "end of input") at 13
        )
      }

      it("should recover at structural boundaries (like if)") {
        "f(1, if (x), 3)" shouldFailWith(
          ParseError.UnexpectedToken(Token.Comma) at 11,
          ParseError.UnexpectedToken(Token.IntLiteral(3)) at 13,
          ParseError.UnexpectedToken(Token.IntLiteral(3)) at 13
        )
      }
    }
  }
}
