package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NonEmptySetSpec extends AnyFunSpec with Matchers {

  describe("NonEmptySet") {
    describe("creation") {
      it("should be created with apply (single element)") {
        val set = NonEmptySet(41)
        set.size shouldBe 1
        set.contains(41) shouldBe true
        set shouldBe a[NonEmptySet[_]]
      }

      it("should be created with apply (multiple elements)") {
        val set = NonEmptySet(41, 43, 47)
        set.size shouldBe 3
        set shouldBe a[SetTwoPlus[_]]
      }

      it("should be created from a non-empty Iterable via tryFrom") {
        val result = NonEmptySet.tryFrom(List(41, 43))
        result shouldBe defined
        result.get.size shouldBe 2
        result.get shouldBe a[SetTwoPlus[_]]
      }

      it("should return None when creating from an empty Iterable via tryFrom") {
        NonEmptySet.tryFrom(Nil) shouldBe None
      }

      it("should be created from a non-empty Iterable via from") {
        val set = NonEmptySet.from(List(41))
        set.size shouldBe 1
        set.contains(41) shouldBe true
      }

      it("should throw IllegalArgumentException when creating from an empty Iterable via from") {
        assertThrows[IllegalArgumentException] {
          NonEmptySet.from(Nil)
        }
      }
    }

    describe("operations") {
      it("should support incl") {
        val set = NonEmptySet(41)
        val updated = set.incl(43)
        updated.size shouldBe 2
        updated.contains(43) shouldBe true
      }

      it("should support excl") {
        val set = NonEmptySet(41, 43)
        val updated = set.excl(43)
        updated.size shouldBe 1
        updated.contains(41) shouldBe true
        updated.contains(43) shouldBe false
      }

      it("should return the underlying Set if excl results in an empty set") {
        val set = NonEmptySet(41)
        val result = set.excl(41)
        result shouldBe empty
        result should not be a[NonEmptySet[_]]
      }

      it("should support contains") {
        val set = NonEmptySet(41, 43)
        set.contains(41) shouldBe true
        set.contains(47) shouldBe false
      }

      it("should support ++") {
        val set1 = NonEmptySet(41)
        val set2 = Set(43, 47)
        val combined = set1 ++ set2
        combined.size shouldBe 3
        combined.contains(41) shouldBe true
        combined.contains(43) shouldBe true
        combined.contains(47) shouldBe true
      }

      it("should support equality with other NonEmptySet") {
        NonEmptySet(41, 43) shouldBe NonEmptySet(41, 43)
        NonEmptySet(41, 43) shouldBe NonEmptySet(43, 41)
        NonEmptySet(41, 43) shouldNot be(NonEmptySet(41))
      }

      it("should support equality with standard Set") {
        NonEmptySet(41, 43) shouldBe Set(41, 43)
        Set(41, 43) shouldBe NonEmptySet(41, 43)
      }

      it("should have correct toString") {
        NonEmptySet(41).toString shouldBe "NonEmptySet(41)"
      }
    }
  }

  describe("SetTwoPlus") {
    describe("creation") {
      it("should be created with apply") {
        val set = SetTwoPlus(41, 43, 47)
        set.size shouldBe 3
        set.contains(41) shouldBe true
      }

      it("should be created from an Iterable with 2+ elements via tryFrom") {
        val result = SetTwoPlus.tryFrom(List(41, 43))
        result shouldBe defined
        result.get.size shouldBe 2
      }

      it("should return None when creating from an Iterable with < 2 elements via tryFrom") {
        SetTwoPlus.tryFrom(List(41)) shouldBe None
        SetTwoPlus.tryFrom(Nil) shouldBe None
      }

      it("should be created from an Iterable with 2+ elements via from") {
        val set = SetTwoPlus.from(List(41, 43, 47))
        set.size shouldBe 3
      }

      it("should throw IllegalArgumentException when creating from an Iterable with < 2 elements via from") {
        assertThrows[IllegalArgumentException] {
          SetTwoPlus.from(List(41))
        }
      }
    }

    describe("operations") {
      it("should support incl and remain a SetTwoPlus") {
        val set = SetTwoPlus(41, 43)
        val updated = set.incl(47)
        updated shouldBe a[SetTwoPlus[_]]
        updated.size shouldBe 3
      }

      it("should support excl and remain a SetTwoPlus if size >= 2") {
        val set = SetTwoPlus(41, 43, 47)
        val updated = set.excl(47)
        updated shouldBe a[SetTwoPlus[_]]
        updated.size shouldBe 2
      }

      it("should downgrade to NonEmptySet on excl if only 1 element remains") {
        val set = SetTwoPlus(41, 43)
        val updated = set.excl(43)
        updated should not be a[SetTwoPlus[_]]
        updated shouldBe a[NonEmptySet[_]]
        updated.size shouldBe 1
        updated.contains(41) shouldBe true
      }

      it("should support ++ and remain a SetTwoPlus") {
        val set = SetTwoPlus(41, 43)
        val combined = set ++ Set(47)
        combined shouldBe a[SetTwoPlus[_]]
        combined.size shouldBe 3
      }

      it("should have correct toString") {
        SetTwoPlus(41, 43).toString shouldBe "SetTwoPlus(41, 43)"
      }
    }
  }
}
