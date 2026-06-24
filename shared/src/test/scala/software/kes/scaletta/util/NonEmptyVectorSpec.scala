package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NonEmptyVectorSpec extends AnyFunSpec with Matchers {

  describe("NonEmptyVector") {
    describe("creation") {
      it("should be created with apply (single element)") {
        val vec = NonEmptyVector(41)
        vec.size shouldBe 1
        vec.head shouldBe 41
        vec shouldBe a[NonEmptyVector[_]]
        vec should not be a[VectorTwoPlus[_]]
      }

      it("should be created with apply (multiple elements)") {
        val vec = NonEmptyVector(41, 43, 47)
        vec.size shouldBe 3
        vec shouldBe a[VectorTwoPlus[_]]
      }

      it("should be created from a non-empty Iterable via tryFrom") {
        val result = NonEmptyVector.tryFrom(List(41, 43))
        result shouldBe defined
        result.get.size shouldBe 2
        result.get shouldBe a[VectorTwoPlus[_]]
      }

      it("should return None when creating from an empty Iterable via tryFrom") {
        NonEmptyVector.tryFrom(Nil) shouldBe None
      }

      it("should be created from a non-empty Iterable via from") {
        val vec = NonEmptyVector.from(List(41))
        vec.size shouldBe 1
        vec.head shouldBe 41
      }

      it("should throw IllegalArgumentException when creating from an empty Iterable via from") {
        assertThrows[IllegalArgumentException] {
          NonEmptyVector.from(Nil)
        }
      }
    }

    describe("operations") {
      it("should support prepend") {
        val vec = NonEmptyVector(41)
        val updated = vec.prepend(43)
        updated.size shouldBe 2
        updated.head shouldBe 43
      }

      it("should support append") {
        val vec = NonEmptyVector(41)
        val updated = vec.append(43)
        updated.size shouldBe 2
        updated.last shouldBe 43
      }

      it("should support equality with other NonEmptyVector") {
        NonEmptyVector(41, 43) shouldBe NonEmptyVector(41, 43)
        NonEmptyVector(41, 43) shouldNot be(NonEmptyVector(43, 41))
      }

      it("should support equality with standard Vector") {
        NonEmptyVector(41, 43) shouldBe Vector(41, 43)
        Vector(41, 43) shouldBe NonEmptyVector(41, 43)
      }

      it("should have correct toString") {
        NonEmptyVector(41).toString shouldBe "NonEmptyVector(41)"
      }
    }
  }

  describe("VectorTwoPlus") {
    describe("creation") {
      it("should be created with apply") {
        val vec = VectorTwoPlus(41, 43, 47)
        vec.size shouldBe 3
        vec(0) shouldBe 41
        vec(1) shouldBe 43
      }

      it("should be created from an Iterable with 2+ elements via tryFrom") {
        val result = VectorTwoPlus.tryFrom(List(41, 43))
        result shouldBe defined
        result.get.size shouldBe 2
      }

      it("should return None when creating from an Iterable with < 2 elements via tryFrom") {
        VectorTwoPlus.tryFrom(List(41)) shouldBe None
        VectorTwoPlus.tryFrom(Nil) shouldBe None
      }

      it("should be created from an Iterable with 2+ elements via from") {
        val vec = VectorTwoPlus.from(List(41, 43, 47))
        vec.size shouldBe 3
      }

      it("should throw IllegalArgumentException when creating from an Iterable with < 2 elements via from") {
        assertThrows[IllegalArgumentException] {
          VectorTwoPlus.from(List(41))
        }
      }
    }

    describe("operations") {
      it("should preserve VectorTwoPlus on updated") {
        val vec = VectorTwoPlus(41, 43)
        val updated = vec.updated(0, 47)
        updated shouldBe a[VectorTwoPlus[_]]
        updated(0) shouldBe 47
        updated.size shouldBe 2
      }

      it("should preserve VectorTwoPlus on map") {
        val vec = VectorTwoPlus(41, 43)
        val mapped = vec.map(_ + 1)
        mapped shouldBe a[VectorTwoPlus[_]]
        mapped(0) shouldBe 42
        mapped(1) shouldBe 44
      }

      it("should support prependV2 and return VectorTwoPlus") {
        val vec = VectorTwoPlus(41, 43)
        val updated = vec.prependV2(47)
        updated shouldBe a[VectorTwoPlus[_]]
        updated.size shouldBe 3
        updated.head shouldBe 47
      }

      it("should support appendV2 and return VectorTwoPlus") {
        val vec = VectorTwoPlus(41, 43)
        val updated = vec.appendV2(47)
        updated shouldBe a[VectorTwoPlus[_]]
        updated.size shouldBe 3
        updated.last shouldBe 47
      }

      it("should have correct toString") {
        VectorTwoPlus(41, 43).toString shouldBe "VectorTwoPlus(41, 43)"
      }
    }
  }
}
