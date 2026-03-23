package software.kes.scaletta.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class IdentifierSpec extends AnyFunSpec with Matchers {
  describe("Identifier") {
    describe("apply") {
      it("should create an Identifier for a non-empty string") {
        val id = Identifier("abc")
        id.name shouldBe "abc"
      }

      it("should throw IllegalArgumentException for an empty string") {
        an[IllegalArgumentException] should be thrownBy Identifier("")
      }
    }

    describe("tryParse") {
      it("should return Right(Identifier) for a non-empty string") {
        val result = Identifier.tryParse("abc")
        result shouldBe Right(Identifier("abc"))
      }

      it("should return Left(error) for an empty string") {
        val result = Identifier.tryParse("")
        result.isLeft shouldBe true
        result.left.get shouldBe "Identifier cannot be empty"
      }
    }

    describe("unapply") {
      it("should work in pattern matching") {
        val id = Identifier("xyz")
        id match {
          case Identifier(name) => name shouldBe "xyz"
        }
      }
    }

    describe("toString") {
      it("should return the name") {
        Identifier("foo").toString shouldBe "foo"
      }
    }

    describe("equality") {
      it("should be equal to another Identifier with the same name") {
        Identifier("abc") shouldBe Identifier("abc")
      }

      it("should not be equal to an Identifier with a different name") {
        Identifier("abc") shouldNot be(Identifier("def"))
      }

      it("should have the same hash code for the same name") {
        Identifier("abc").hashCode shouldBe Identifier("abc").hashCode
      }
    }

    describe("reserved words") {
      it("should allow reserved words since they can be quoted in source") {
        val reserved = "val"
        val id = Identifier(reserved)
        id.name shouldBe "val"
      }
    }
  }
}
