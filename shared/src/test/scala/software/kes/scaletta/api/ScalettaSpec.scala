package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ScalettaSpec extends AnyFunSpec with Matchers {

  describe("Scaletta.compile") {
    it("should successfully parse a simple expression") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 + 2")

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should return errors for invalid syntax") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 +")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
    }

    it("should return errors for extra tokens") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 + 2 3")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
    }
  }

}
