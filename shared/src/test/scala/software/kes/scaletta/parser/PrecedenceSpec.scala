package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import software.kes.scaletta.internal.ast.AstBuilders._
import software.kes.scaletta.testsupport.{ParserTestOps, ParserTestSupport}

class PrecedenceSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private implicit val support: ParserTestSupport = new ParserTestSupport() with Matchers
  private implicit val matchers: Matchers = Matchers

  import ParserTestOps._

  describe("Operator Precedence & Associativity") {
    it("should respect the precedence matrix") {
      val matrix = Table(
        ("input", "expectedAst"),
        // Logical Or < Logical And
        ("a || b && c", infix(ref("a"), "||", infix(ref("b"), "&&", ref("c")))),
        ("a && b || c", infix(infix(ref("a"), "&&", ref("b")), "||", ref("c"))),

        // Comparison > Equality: a < b == c should be (a < b) == c
        ("a < b == c", infix(infix(ref("a"), "<", ref("b")), "==", ref("c"))),

        // Equality < Addition
        ("a == b + c", infix(ref("a"), "==", infix(ref("b"), "+", ref("c")))),

        // Addition < Multiplication
        ("a + b * c", infix(ref("a"), "+", infix(ref("b"), "*", ref("c")))),

        // Alphanumeric (1) < Arithmetic (8, 9)
        ("a + b plus c", infix(infix(ref("a"), "+", ref("b")), "plus", ref("c"))),
        ("a plus b * c", infix(ref("a"), "plus", infix(ref("b"), "*", ref("c")))),

        // Deeply nested
        ("a || b && c == d + e * f",
          infix(ref("a"), "||",
            infix(ref("b"), "&&",
              infix(ref("c"), "==",
                infix(ref("d"), "+",
                  infix(ref("e"), "*", ref("f"))
                )
              )
            )
          )
        )
      )
      forAll(matrix) { (input, expected) => input shouldParseIgnoringWarnings expected }
    }

    it("should handle left-associativity by default") {
      val matrix = Table(
        ("input", "expectedAst"),
        ("a - b - c", infix(infix(ref("a"), "-", ref("b")), "-", ref("c"))),
        ("a / b / c", infix(infix(ref("a"), "/", ref("b")), "/", ref("c"))),
        ("a plus b plus c", infix(infix(ref("a"), "plus", ref("b")), "plus", ref("c")))
      )
      forAll(matrix) { (input, expected) => input shouldParseIgnoringWarnings expected }
    }

    it("should handle right-associativity for colon-ending operators") {
      val matrix = Table(
        ("input", "expectedAst"),
        ("a :: b :: c", infix(ref("a"), "::", infix(ref("b"), "::", ref("c")))),
        ("a +: b +: c", infix(ref("a"), "+:", infix(ref("b"), "+:", ref("c"))))
        // Mixed with same precedence
        // ("a : b :: c", infix(ref("a"), ":", infix(ref("b"), "::", ref("c"))))
      )
      forAll(matrix) { (input, expected) => input shouldParseTo expected }
    }

    it("should handle postfix call priority") {
      val matrix = Table(
        ("input", "expectedAst"),
        ("f(x) + g(y)", infix(callSimple(ref("f"), ref("x")), "+", callSimple(ref("g"), ref("y")))),
        ("f(x) :: g(y)", infix(callSimple(ref("f"), ref("x")), "::", callSimple(ref("g"), ref("y")))),
        ("f(x)(y) * z", infix(call(ref("f")).group(ref("x")).group(ref("y")).build(), "*", ref("z")))
      )
      forAll(matrix) { (input, expected) => input shouldParseTo expected }
    }

    it("should correctly distinguish symbolic vs alphanumeric precedence") {
      // Alphanumeric is 1, Comparison is (6)
      "a < b lt c" shouldParseIgnoringWarnings infix(infix(ref("a"), "<", ref("b")), "lt", ref("c"))
    }
  }
}
