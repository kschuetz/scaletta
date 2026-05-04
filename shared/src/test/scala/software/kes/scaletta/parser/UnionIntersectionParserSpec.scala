package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.ast.AstBuilders._
import software.kes.scaletta.testsupport.ParserTestSupport

class UnionIntersectionParserSpec extends AnyFunSpec with Matchers {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import software.kes.scaletta.testsupport.ParserTestOps._

  describe("Parser") {
    it("should parse '&' as an infix operator") {
      "a & b" shouldParseTo infix(ref("a"), "&", ref("b"))
    }

    it("should parse '|' as an infix operator") {
      "a | b" shouldParseTo infix(ref("a"), "|", ref("b"))
    }

    it("should respect precedence between '&' and '|'") {
      "a | b & c" shouldParseTo infix(ref("a"), "|", infix(ref("b"), "&", ref("c")))
      "a & b | c" shouldParseTo infix(infix(ref("a"), "&", ref("b")), "|", ref("c"))
    }

    it("should parse longer operators containing '|' and '&' as infix operators") {
      "a || b" shouldParseTo infix(ref("a"), "||", ref("b"))
      "a && b" shouldParseTo infix(ref("a"), "&&", ref("b"))
      "a |+ b" shouldParseTo infix(ref("a"), "|+", ref("b"))
    }
  }
}
