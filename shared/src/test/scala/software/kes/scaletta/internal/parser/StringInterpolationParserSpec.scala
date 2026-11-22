package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.internal.ast.AstBuilders._
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

final class StringInterpolationParserSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._

  describe("Parser - String Interpolation") {

    describe("Happy-Path Standard Syntax Permutations") {

      it("should parse an empty single-line interpolated string") {
        "s\"\"" shouldParseTo sLit("")
      }

      it("should parse an empty multi-line interpolated string") {
        "s\"\"\"\"\"\"" shouldParseTo sLit("")
      }

      it("should parse a pure literal single-line interpolated string") {
        "s\"hello world\"" shouldParseTo sLit("hello world")
      }

      it("should parse a pure literal multi-line interpolated string") {
        "s\"\"\"hello\nworld\"\"\"" shouldParseTo sLit("hello\nworld")
      }

      it("should parse simple identifier substitution ($name)") {
        "s\"hello $name\"" shouldParseTo sLit("hello ", (ref("name"), ""))
      }

      it("should parse simple identifier at start of string ($name is here)") {
        "s\"$name is here\"" shouldParseTo sLit("", (ref("name"), " is here"))
      }

      it("should parse block expression substitution (${a + b})") {
        "s\"sum: ${a + b}\"" shouldParseTo sLit("sum: ", (infix(ref("a"), "+", ref("b")), ""))
      }

      it("should parse adjacent identifier escapes ($a$b)") {
        "s\"$a$b\"" shouldParseTo sLit("", (ref("a"), ""), (ref("b"), ""))
      }

      it("should parse adjacent block escapes (${a}${b})") {
        "s\"${a}${b}\"" shouldParseTo sLit("", (ref("a"), ""), (ref("b"), ""))
      }

      it("should parse interleaved escapes with prefix, middle, and suffix literals") {
        "s\"prefix_${a}_middle_${b}_suffix\"" shouldParseTo sLit("prefix_", (ref("a"), "_middle_"), (ref("b"), "_suffix"))
      }

      it("should parse complex expressions inside escape blocks") {
        "s\"result: ${x + 41 * y} with prime: ${43}\"" shouldParseTo sLit(
          "result: ",
          (infix(ref("x"), "+", infix(lit(41), "*", ref("y"))), " with prime: "),
          (lit(43), "")
        )
      }

      it("should parse multi-line interpolated strings with embedded expressions") {
        "s\"\"\"start $x\nmiddle ${y + 47}\nend\"\"\"" shouldParseTo sLit(
          "start ",
          (ref("x"), "\nmiddle "),
          (infix(ref("y"), "+", lit(47)), "\nend")
        )
      }

      it("should parse raw interpolator") {
        "raw\"hello $x\"" shouldParseTo interpolatedString(Interpolator.Raw, "hello ", (ref("x"), ""))
      }

      it("should parse f interpolator") {
        "f\"value: $y\"" shouldParseTo interpolatedString(Interpolator.F, "value: ", (ref("y"), ""))
      }

      it("should parse custom interpolators") {
        "custom\"${z}\"" shouldParseTo interpolatedString(Interpolator.Custom("custom"), "", (ref("z"), ""))
        "query\"SELECT * FROM users WHERE id = $userId\"" shouldParseTo interpolatedString(
          Interpolator.Custom("query"),
          "SELECT * FROM users WHERE id = ",
          (ref("userId"), "")
        )
      }

      it("should parse nested block expressions inside escape blocks") {
        "s\"value: ${ { val a = 41; a + 43 } }\"" shouldParseTo sLit(
          "value: ",
          (block(infix(ref("a"), "+", lit(43)), valId("a", lit(41))), "")
        )
      }

      it("should parse conditional expressions inside escape blocks") {
        "s\"value: ${if (flag) 41 else 43}\"" shouldParseTo sLit(
          "value: ",
          (cond(ref("flag"), lit(41), lit(43)), "")
        )
      }
    }
  }
}
