package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.ast.AstBuilders._
import software.kes.scaletta.scanner.Token
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ReservedWordSpec extends AnyFunSpec with Matchers {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._
  import software.kes.scaletta.testsupport.ParseErrorMatchers._
  import software.kes.scaletta.testsupport.ParseWarningMatchers._

  describe("Reserved Word: '='") {
    it("should fail when '=' is used as a reference") {
      "=" shouldFailWith (ParseError.UnexpectedToken(Token.Eq) at 0)
    }

    it("should fail when '=' is used as an infix operator") {
      "1 = 2" shouldFailWith (ParseError.ExtraToken(Token.Eq, "end of input") at 2) producing {
        lit(1)
      }
    }

    it("should fail when '=' is used as a prefix operator") {
      "= 1" shouldFailWith(ParseError.UnexpectedToken(Token.Eq) at 0, ParseError.ExtraToken(Token.IntLiteral(1), "end of input") at 2)
    }

    it("should allow backticked '=' as an identifier") {
      "`=`" shouldParseTo ref("=")
    }

    it("should allow backticked '=' as an infix operator") {
      "1 `=` 2" shouldParseWithWarnings (ParseWarning.SuspiciousInfixExpression("=") at 2) producing {
        infix(lit(1), "=", lit(2))
      }
    }

    it("should allow '=' in its structural role for named arguments") {
      "f(x = 1)" shouldParseTo callSimple(ref("f"), namedArg("x", lit(1)))
    }

    it("should allow '=' in its structural role for declarations") {
      "{ val x = 1; x }" shouldParseTo block(ref("x"), valId("x", lit(1)))
    }
  }
}
