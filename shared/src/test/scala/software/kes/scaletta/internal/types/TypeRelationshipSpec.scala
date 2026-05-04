package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Type

class TypeRelationshipSpec extends AnyFunSpec with Matchers {

  describe("TypeRelationship") {

    describe("Same") {
      val rel: TypeRelationship[String] = TypeRelationship.Same
      it("should have correct property values for Same") {
        rel.isSame shouldBe true
        rel.isSubtype shouldBe true
        rel.isSupertype shouldBe true
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe None
      }
    }

    describe("StrictSubtype") {
      val rel: TypeRelationship[String] = TypeRelationship.StrictSubtype
      it("should have correct property values for StrictSubtype") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe true
        rel.isSupertype shouldBe false
        rel.isStrictSubtype shouldBe true
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe None
      }
    }

    describe("StrictSupertype") {
      val rel: TypeRelationship[String] = TypeRelationship.StrictSupertype
      it("should have correct property values for StrictSupertype") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe false
        rel.isSupertype shouldBe true
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe true
        rel.commonSupertype shouldBe None
      }
    }

    describe("HaveCommonSupertype") {
      val common = Type.Nominal("Root")
      val rel = TypeRelationship.HaveCommonSupertype(common)
      it("should have correct property values for HaveCommonSupertype") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe false
        rel.isSupertype shouldBe false
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe Some(common)
      }

      it("should correctly store the common supertype") {
        rel.value shouldBe common
      }
    }

    describe("Unrelated") {
      val rel: TypeRelationship[String] = TypeRelationship.Unrelated
      it("should have correct property values for Unrelated") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe false
        rel.isSupertype shouldBe false
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe None
      }
    }
  }
}
