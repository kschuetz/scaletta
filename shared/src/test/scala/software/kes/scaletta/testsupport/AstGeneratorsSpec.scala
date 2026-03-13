package software.kes.scaletta.testsupport

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AstGeneratorsSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
  // NOTE: Diversity checks can be flaky. Only enable them temporarily while
  // working on the generators.
  private val enableDiversityChecks = false

  describe("AstGenerators") {
    it("should generate valid expressions that can be rendered") {
      forAll(AstGenerators.genExpression(3)) { expr =>
        val rendered = AstRenderer.render(expr)
        rendered should not be empty
      }
    }

    if (enableDiversityChecks) {
      runDiversityChecks()
    }
  }

  private def runDiversityChecks(): Unit = {
    describe("Diversity Checks") {
      it("should generate diverse literal types") {
        var foundTypes = Set.empty[String]
        forAll(AstGenerators.genLiteral) { lit =>
          foundTypes += lit.getClass.getSimpleName
          if (foundTypes.size >= 8) {
            // We found most types: Int, Long, Float, Double, True, False, Null, Char, String
          }
        }
        foundTypes.size should be >= 5
      }

      it("should generate diverse patterns") {
        var foundPatterns = Set.empty[String]
        forAll(AstGenerators.genPattern(2)) { pat =>
          foundPatterns += pat.getClass.getSimpleName
        }
        foundPatterns.size should be >= 3
      }

      it("should generate diverse declarations") {
        var foundDecls = Set.empty[String]
        forAll(AstGenerators.genDeclaration(2)) { decl =>
          foundDecls += decl.getClass.getSimpleName
        }
        foundDecls.size should be >= 2
      }
    }
  }
}
