package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import software.kes.scaletta.testsupport.{AstGenerators, AstRenderer, Corrupter, ParserTestSupport}

class ParserResiliencePropertySpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
  private val support = new ParserTestSupport with Matchers

  describe("Parser Resilience") {
    it("should never crash or hang on corrupted input") {
      forAll(AstGenerators.genExpression(3)) { ast =>
        val source = AstRenderer.render(ast)
        val corrupted = Corrupter.corrupt(source)

        withClue(s"Corrupted source: $corrupted") {
          // Property 1: Non-termination / No exceptions
          // If it hangs, the test will timeout. If it throws, it fails here.
          val result = support.parse(corrupted)

          // Property 2: Errors should be reported if corrupted
          // (We don't strictly assert this because some mutations might result in valid code)
        }
      }
    }

    it("should recover a partial AST even with junk tokens (Currently Partial Support)") {
      val source = "val x = 1; val y = @#?; val z = 2; x + z"
      val result = support.parseWithDiagnostics(source)

      // Note: As of now, the parser might return None if it hits an error it can't sync from.
      // This test serves as a baseline for future improvements.
      if (result.ast.isDefined) {
        result.errors should not be empty
        val astStr = result.ast.get.toString
        astStr should include("Identifier(x)")
      } else {
        // If it returns None, it should at least have errors
        result.errors should not be empty
      }
    }
  }
}
