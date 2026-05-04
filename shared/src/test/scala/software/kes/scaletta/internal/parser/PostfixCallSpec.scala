package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.ParserTestSupport

class PostfixCallSpec extends AnyFunSpec with Matchers {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = this

  import software.kes.scaletta.testsupport.ParseErrorMatchers._
  import software.kes.scaletta.testsupport.ParserTestOps._

  describe("Postfix Calls") {
    it("should report an error for '1 increment' (if it were allowed to proceed)") {
      // In current logic, '1 increment' stops at '1' because 'increment' is not followed by an expression.
      // We can check that it fails with an ExtraToken error if we require exhaustion.
      "1 increment" shouldFailWith atIndex(0)(errorOfType[ParseError.ExtraToken])
    }

    it("should report an error for '(1 +)'") {
      // Inside parentheses, '+' will be taken as an infix because it has higher precedence than Minimum.
      // But it will fail because there is no RHS.
      // Since '+' is not followed by an expression, it stops at '1'.
      // The parenthesized expression then sees '+' where it expects ')'.
      "(1 +)" shouldFailWith atIndex(0)(errorOfType[ParseError.UnexpectedToken])
    }
  }
}
