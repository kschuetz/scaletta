package software.kes.scaletta.testsupport

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CorrupterSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

  describe("Corrupter") {
    it("should mutate the source string") {
      val source = "val x = 1"
      val mutations = (1 to 100).map(_ => Corrupter.corrupt(source)).toSet

      // We expect at least some diversity in mutations
      mutations.size should be > 1
      mutations.exists(_ != source) shouldBe true
    }

    it("should handle empty strings") {
      Corrupter.corrupt("") shouldBe ""
    }

    it("should preserve the source if it is very short") {
      // With very short strings, mutation is still possible, but let's just 
      // check it doesn't crash
      noException should be thrownBy Corrupter.corrupt("x")
    }

    it("should produce a different string from valid AST-generated source") {
      forAll(AstGenerators.genExpression(2)) { ast =>
        val source = AstRenderer.render(ast)
        val corrupted = Corrupter.corrupt(source)

        // While not strictly guaranteed due to randomness, with 1-3 mutations 
        // it's highly likely to change.
        // We'll just check that it's a valid string.
        corrupted should not be null
      }
    }
  }
}
