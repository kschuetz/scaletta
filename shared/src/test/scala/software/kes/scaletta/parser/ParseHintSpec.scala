package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast.AstBuilders._
import software.kes.scaletta.testsupport.ParseHintMatchers._
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class ParseHintSpec extends AnyFunSpec with Matchers {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._

  describe("Parser Hints") {
    it("should emit a hint for unnecessary parentheses around a literal") {
      "(123)" shouldParseWithHints (ParseHint.UnnecessaryParentheses spanning(0, 4)) producing lit(123)
    }

    it("should emit a hint for unnecessary parentheses around a reference") {
      "(foo)" shouldParseWithHints (ParseHint.UnnecessaryParentheses spanning(0, 4)) producing ref("foo")
    }

    it("should NOT emit a hint for necessary parentheses in an arithmetic expression") {
      "(1 + 2) * 3" shouldParseTo infix(infix(lit(1), "+", lit(2)), "*", lit(3))
    }

    it("should emit a hint for unnecessary parentheses around a nested parenthesized literal") {
      "((123))" shouldParseWithHints(
        ParseHint.UnnecessaryParentheses spanning(1, 5),
        ParseHint.UnnecessaryParentheses spanning(0, 6)
      ) producing lit(123)
    }
  }
}
