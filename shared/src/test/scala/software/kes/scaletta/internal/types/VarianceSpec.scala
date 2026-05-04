package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class VarianceSpec extends AnyFunSpec with Matchers {

  describe("Variance") {
    it("invariant factory should return Invariant") {
      Variance.invariant shouldBe Variance.Invariant
    }

    it("covariant factory should return Covariant") {
      Variance.covariant shouldBe Variance.Covariant
    }

    it("contravariant factory should return Contravariant") {
      Variance.contravariant shouldBe Variance.Contravariant
    }
  }
}
