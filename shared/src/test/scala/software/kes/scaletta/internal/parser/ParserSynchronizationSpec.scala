package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.internal.ast.AstBuilders._
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
          callSimple(ref("f"), lit(1), callSimple(ref("g"), lit(2), errUnexpected(Token.At), lit(3)), lit(4))
        }
      }
    }

    describe("Multiple Errors") {
      it("should recover from multiple independent errors in the same argument list") {
        "f(@, 1, #, 2)" shouldRecoverWith(
          ParseError.UnexpectedToken(Token.At) at 2,
          ParseError.UnexpectedToken(Token.Hash) at 8
        ) producing {
          callSimple(ref("f"), errUnexpected(Token.At), lit(1), errUnexpected(Token.Hash), lit(2))
        }
      }
    }

    describe("Boundary Proximity") {
      it("should recover at the closing parenthesis when error is at the end of the list") {
        "f(1, @)" shouldRecoverWith (
          ParseError.UnexpectedToken(Token.At) at 5
          ) producing {
          callSimple(ref("f"), lit(1), errUnexpected(Token.At))
        }
      }

      it("should recover at the closing parenthesis when error is the only argument") {
        "f(@)" shouldRecoverWith (
          ParseError.UnexpectedToken(Token.At) at 2
          ) producing {
          callSimple(ref("f"), errUnexpected(Token.At))
        }
      }
    }

    describe("Interleaved Valid/Invalid Code") {
      it("should extract all valid arguments when interleaved with errors") {
        "f(1, @, 2, #, 3)" shouldRecoverWith(
          ParseError.UnexpectedToken(Token.At) at 5,
          ParseError.UnexpectedToken(Token.Hash) at 11
        ) producing {
          callSimple(ref("f"), lit(1), errUnexpected(Token.At), lit(2), errUnexpected(Token.Hash), lit(3))
        }
      }
    }

    describe("Structural Boundaries") {
      import Token._
      import software.kes.scaletta.internal.parser.ParseError._

      it("should recover at structural boundaries (like val)") {
        "f(1, val x = 2, 3)" shouldFailWith(
          ExpectedIdentifier(Val, "expression") spanning(5, 7),
          UnexpectedToken(Eq) spanning(11, 11),
          ExpectedToken(Comma, IntLiteral(2), "argument list") spanning(13, 13)
        )
      }

      it("should recover at structural boundaries (like def)") {
        "f(1, def f(x), 3)" shouldFailWith(
          ExpectedIdentifier(Def, "expression") spanning(5, 7)
        )
      }

      it("should recover at structural boundaries (like if)") {
        "f(1, if (x), 3)" shouldFailWith(
          UnexpectedToken(Comma) spanning(11, 11),
          ExpectedToken(Else, IntLiteral(3), "conditional") spanning(13, 13),
          ExpectedToken(Comma, IntLiteral(3), "argument list") spanning(13, 13)
        )
      }
    }

    describe("Block Synchronization") {
      import Token._
      import software.kes.scaletta.internal.parser.ParseError._

      // TODO: fix this
      ignore("should recover from a malformed declaration in a block and find the next one") {
        "{ val x = ; val y = 2; x + y }" shouldRecoverWith (
          ExpectedIdentifier(Semicolon, "expression") at 10
          ) producing {
          block(
            Vector(
              valDecl(pId("x"), errExpr(ExpectedIdentifier(Semicolon, "expression"))),
              valDecl(pId("y"), lit(2))
            ),
            infix(ref("x"), "+", ref("y"))
          )
        }
      }

      it("should recover from a completely broken declaration and continue") {
        "{ val @ = 1; val y = 2; y }" shouldRecoverWith (
          ExpectedIdentifier(At, "pattern") at 6
          ) producing {
          block(
            Vector(
              valDecl(errPat(ExpectedIdentifier(At, "pattern")), lit(1)),
              valDecl(pId("y"), lit(2))
            ),
            ref("y")
          )
        }
      }

      it("should synchronize to the end of a block if an error occurs near the end") {
        "{ val x = 1; val y = @; y }" shouldRecoverWith (
          UnexpectedToken(At) at 21
          ) producing {
          block(
            Vector(
              valDecl(pId("x"), lit(1)),
              valDecl(pId("y"), errUnexpected(At))
            ),
            ref("y")
          )
        }
      }

      // TODO: fix this
      ignore("should handle multiple malformed declarations in a block") {
        "{ val x = ; val @ = 2; val z = 3; z }" shouldFailWith(
          ExpectedIdentifier(Semicolon, "expression") spanning(10, 10),
          ExpectedIdentifier(At, "pattern") spanning(16, 16)
        )
      }
    }

    describe("Formal Parameter Group Synchronization") {
      import Token._
      import software.kes.scaletta.internal.parser.ParseError._

      it("should recover from a malformed parameter and find the next one") {
        "f(x: Int, @, y: String)".shouldRecoverWith(
          UnexpectedToken(At) at 10
        ).ignoringAst()
      }
    }
  }
}
