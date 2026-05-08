package software.kes.scaletta.internal.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Name

class NameSpec extends AnyFunSpec with Matchers {
  describe("Name") {
    describe("apply") {
      it("should create an Name for a non-empty string") {
        val id = Name("abc")
        id.value shouldBe "abc"
      }

      it("should throw IllegalArgumentException for an empty string") {
        an[IllegalArgumentException] should be thrownBy Name("")
      }
    }

    describe("tryParse") {
      it("should return Right(Name) for a non-empty string") {
        val result = Name.tryParse("abc")
        result shouldBe Right(Name("abc"))
      }

      it("should return Left(error) for an empty string") {
        val result = Name.tryParse("")
        result.isLeft shouldBe true
        result.left.get shouldBe "Name cannot be empty"
      }
    }

    describe("unapply") {
      it("should work in pattern matching") {
        val id = Name("xyz")
        id match {
          case Name(name) => name shouldBe "xyz"
        }
      }
    }

    describe("toString") {
      it("should return the name") {
        Name("foo").toString shouldBe "foo"
      }
    }

    describe("equality") {
      it("should be equal to another Name with the same name") {
        Name("abc") shouldBe Name("abc")
      }

      it("should not be equal to an Name with a different name") {
        Name("abc") shouldNot be(Name("def"))
      }

      it("should have the same hash code for the same name") {
        Name("abc").hashCode shouldBe Name("abc").hashCode
      }
    }

    describe("reserved words") {
      it("should allow reserved words since they can be quoted in source") {
        val reserved = "val"
        val id = Name(reserved)
        id.value shouldBe "val"
      }
    }
  }
}
