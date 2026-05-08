package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PackageSegmentSpec extends AnyFunSpec with Matchers {

  describe("PackageSegment.parse") {
    it("should accept valid Java identifiers") {
      PackageSegment.parse("com") shouldBe Right(PackageSegment._unsafeCreate("com"))
      PackageSegment.parse("myPackage") shouldBe Right(PackageSegment._unsafeCreate("myPackage"))
      PackageSegment.parse("_internal") shouldBe Right(PackageSegment._unsafeCreate("_internal"))
      PackageSegment.parse("$impl") shouldBe Right(PackageSegment._unsafeCreate("$impl"))
      PackageSegment.parse("v2") shouldBe Right(PackageSegment._unsafeCreate("v2"))
    }

    it("should accept valid Unicode identifiers") {
      PackageSegment.parse("λ") shouldBe Right(PackageSegment("λ"))
    }

    it("should reject empty strings") {
      PackageSegment.parse("") shouldBe Left("Package name component cannot be empty")
    }

    it("should reject strings starting with a digit") {
      PackageSegment.parse("1abc") shouldBe Left("Invalid package name component: '1abc'")
    }

    it("should reject strings containing invalid characters") {
      PackageSegment.parse("my-package") shouldBe Left("Invalid package name component: 'my-package'")
      PackageSegment.parse("my package") shouldBe Left("Invalid package name component: 'my package'")
      PackageSegment.parse("my.package") shouldBe Left("Invalid package name component: 'my.package'")
    }

    it("should reject reserved keywords") {
      PackageSegment.parse("if") shouldBe Left("'if' is a reserved keyword and cannot be used as a package name component")
      PackageSegment.parse("val") shouldBe Left("'val' is a reserved keyword and cannot be used as a package name component")
      PackageSegment.parse("class") shouldBe Left("'class' is a reserved keyword and cannot be used as a package name component")
      PackageSegment.parse("true") shouldBe Left("'true' is a reserved keyword and cannot be used as a package name component")
      PackageSegment.parse("null") shouldBe Left("'null' is a reserved keyword and cannot be used as a package name component")
    }
  }

  describe("PackageSegment.apply") {
    it("should create a PackageSegment for valid input") {
      PackageSegment("com").name shouldBe Name("com")
    }

    it("should throw IllegalArgumentException for invalid input") {
      an[IllegalArgumentException] should be thrownBy PackageSegment("if")
      an[IllegalArgumentException] should be thrownBy PackageSegment("")
      an[IllegalArgumentException] should be thrownBy PackageSegment("1abc")
    }
  }
}
