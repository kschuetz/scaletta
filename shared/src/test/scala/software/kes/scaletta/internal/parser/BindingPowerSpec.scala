package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.scanner.Token

class BindingPowerSpec extends AnyFunSpec with Matchers {

  describe("Operators.bindingPower") {
    def check(name: String, expected: BindingPower): Unit = {
      it(s"should return $expected for '$name'") {
        Operators.bindingPower(Token.Identifier.Operator(name)) shouldBe expected
      }
    }

    def checkId(name: String, expected: BindingPower): Unit = {
      it(s"should return $expected for '$name' (identifier)") {
        Operators.bindingPower(Token.Identifier.Lower(name)) shouldBe expected
        Operators.bindingPower(Token.Identifier.Upper(name)) shouldBe expected
        Operators.bindingPower(Token.Identifier.Quoted(name)) shouldBe expected
      }
    }

    check("|", BindingPower.LogicalOr)
    check("||", BindingPower.LogicalOr)
    check("^", BindingPower.LogicalXor)
    check("&", BindingPower.LogicalAnd)
    check("&&", BindingPower.LogicalAnd)
    check("<", BindingPower.Comparison)
    check(">", BindingPower.Comparison)
    check("<=", BindingPower.Comparison)
    check(">=", BindingPower.Comparison)
    check("==", BindingPower.Equality)
    check("!=", BindingPower.Equality)
    check("!", BindingPower.Equality)
    check("=", BindingPower.Minimum)
    check(":", BindingPower.ColonOperator)
    check("::", BindingPower.ColonOperator)
    check("+", BindingPower.Addition)
    check("-", BindingPower.Addition)
    check("*", BindingPower.Multiplication)
    check("/", BindingPower.Multiplication)
    check("%", BindingPower.Multiplication)
    check(">>=", BindingPower.Comparison)
    check("<<", BindingPower.Comparison)
    check("<~", BindingPower.Comparison)
    check("++:", BindingPower.Addition)
    check("+-+", BindingPower.Addition)
    check("--", BindingPower.Addition)
    check("&&&", BindingPower.LogicalAnd)
    check("&!", BindingPower.LogicalAnd)
    check("*:", BindingPower.Multiplication)
    check("//", BindingPower.Multiplication)
    check("%%%", BindingPower.Multiplication)
    check("!==", BindingPower.Equality)
    check("===", BindingPower.Equality)
    checkId("foo", BindingPower.Alphanumeric)
    checkId("Bar", BindingPower.Alphanumeric)
  }

  describe("BindingPower ordering") {
    it("should correctly order base levels") {
      BindingPower.allBaseLevels.sliding(2).foreach {
        case Vector(low, high) =>
          withClue(s"$low should be less than $high") {
            BindingPower.BindingPowerOrdering.compare(low, high) should be < 0
          }
        case _ => ()
      }
    }

    it("should correctly handle nudges") {
      import software.kes.scaletta.internal.parser.BindingPower._
      val base = Addition
      val before = base.nudge(-1)
      val after = base.nudge(1)

      BindingPower.BindingPowerOrdering.compare(before, base) should be < 0
      BindingPower.BindingPowerOrdering.compare(after, base) should be > 0
      BindingPower.BindingPowerOrdering.compare(before, after) should be < 0

      // Nudge vs adjacent base levels
      BindingPower.BindingPowerOrdering.compare(before, Comparison) should be > 0
      BindingPower.BindingPowerOrdering.compare(after, Multiplication) should be < 0
    }
  }
}
