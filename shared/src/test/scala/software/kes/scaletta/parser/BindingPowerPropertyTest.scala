package software.kes.scaletta.parser

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BindingPowerPropertyTest extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
  val genBaseLevel: Gen[BindingPower] = Gen.oneOf(BindingPower.allBaseLevels)

  val genNudgeAmount: Gen[Int] = Gen.choose(-1000, 1000)

  val genBindingPower: Gen[BindingPower] =
    Gen.oneOf(
      genBaseLevel,
      for {
        bp <- genBaseLevel
        amount <- genNudgeAmount
      } yield bp.nudge(amount)
    )

  implicit val arbBindingPower: Arbitrary[BindingPower] = Arbitrary(genBindingPower)

  describe("BindingPowerOrdering") {

    it("should be reflexive") {
      forAll(genBindingPower) { bp =>
        BindingPower.BindingPowerOrdering.compare(bp, bp) shouldBe 0
      }
    }

    it("should be antisymmetric") {
      forAll(genBindingPower, genBindingPower) { (a, b) =>
        val cmpAB = BindingPower.BindingPowerOrdering.compare(a, b)
        val cmpBA = BindingPower.BindingPowerOrdering.compare(b, a)
        if (cmpAB == 0) cmpBA shouldBe 0
        else if (cmpAB < 0) cmpBA shouldBe >(0)
        else cmpBA shouldBe <(0)
      }
    }

    it("should be transitive") {
      forAll(genBindingPower, genBindingPower, genBindingPower) { (a, b, c) =>
        val cmpAB = BindingPower.BindingPowerOrdering.compare(a, b)
        val cmpBC = BindingPower.BindingPowerOrdering.compare(b, c)

        if (cmpAB < 0 && cmpBC < 0) {
          BindingPower.BindingPowerOrdering.compare(a, c) shouldBe <(0)
        } else if (cmpAB > 0 && cmpBC > 0) {
          BindingPower.BindingPowerOrdering.compare(a, c) shouldBe >(0)
        } else if (cmpAB == 0 && cmpBC == 0) {
          BindingPower.BindingPowerOrdering.compare(a, c) shouldBe 0
        }
      }
    }

    it("should isolate nudges within major level boundaries") {
      forAll(genBaseLevel, genNudgeAmount) { (base, amount) =>
        val nudged = base.nudge(amount)
        val baseIdx = BindingPower.allBaseLevels.indexOf(base)

        // Check against previous base level
        if (baseIdx > 0) {
          val prevBase = BindingPower.allBaseLevels(baseIdx - 1)
          BindingPower.BindingPowerOrdering.compare(nudged, prevBase) shouldBe >(0)
        }

        // Check against next base level
        if (baseIdx < BindingPower.allBaseLevels.size - 1) {
          val nextBase = BindingPower.allBaseLevels(baseIdx + 1)
          BindingPower.BindingPowerOrdering.compare(nudged, nextBase) shouldBe <(0)
        }
      }
    }

    describe("Nudge Identity & Composition") {
      it("nudge(0) should be identity") {
        forAll(genBindingPower) { bp =>
          bp.nudge(0) shouldBe bp
        }
      }

      it("nudge(x).nudge(y) should be nudge(x + y)") {
        forAll(genBindingPower, genNudgeAmount, genNudgeAmount) { (bp, x, y) =>
          bp.nudge(x).nudge(y) shouldBe bp.nudge(x + y)
        }
      }

      it("nudge(x).nudge(-x) should return to original base if applied to base") {
        forAll(genBaseLevel, genNudgeAmount) { (base, x) =>
          base.nudge(x).nudge(-x) shouldBe base
        }
      }
    }
  }
}
