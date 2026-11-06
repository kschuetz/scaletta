package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

final class ScalettaSpec extends AnyFunSpec with Matchers {

  describe("Scaletta.compile") {
    it("should successfully parse and link a simple expression") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 + 2")

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should successfully parse and link valid local bindings in blocks") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val a = 41; val b = 43; a + b }")

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

    it("should return errors for forward references in blocks") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val x = y; val y = 41; x }")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Forward reference to 'y'")) shouldBe true
    }

    it("should return errors for unknown identifiers") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val x = z; x }")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Unknown identifier 'z'")) shouldBe true
    }

    it("should return errors for duplicate definitions") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val x = 41; val x = 43; x }")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Duplicate definition of 'x'")) shouldBe true
    }

    it("should return errors for unknown type ascriptions") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("41: NonExistentType")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Unknown type 'NonExistentType'")) shouldBe true
    }
  }

}
