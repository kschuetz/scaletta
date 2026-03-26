package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NonEmptyVectorSpec extends AnyFunSpec with Matchers {

  describe("NonEmptyVector") {
    it("should be created with apply") {
      val nev = NonEmptyVector(41, 43)
      nev.length shouldBe 2
      nev.head shouldBe 41
      nev.last shouldBe 43
    }

    it("should be created from a non-empty Vector") {
      val v = Vector(41, 43)
      val nev = NonEmptyVector.tryFrom(v)
      nev shouldBe defined
      nev.get.underlying shouldBe v
    }

    it("should return None when created from an empty Vector") {
      NonEmptyVector.tryFrom(Vector.empty[Int]) shouldBe None
    }

    it("should be created via fromVectorUnsafe for non-empty Vector") {
      val v = Vector(41, 43)
      val nev = NonEmptyVector.from(v)
      nev.underlying shouldBe v
    }

    it("should throw IllegalArgumentException via fromVectorUnsafe for empty Vector") {
      assertThrows[IllegalArgumentException] {
        NonEmptyVector.from(Vector.empty[Int])
      }
    }

    it("should have length equal to underlying") {
      NonEmptyVector(41).length shouldBe 1
    }

    it("should not be empty") {
      val nev = NonEmptyVector(41)
      nev.isEmpty shouldBe false
      nev.nonEmpty shouldBe true
    }

    it("should support element access") {
      val nev = NonEmptyVector(41, 43, 45)
      nev(0) shouldBe 41
      nev(1) shouldBe 43
      nev(2) shouldBe 45
    }

    it("should support prepend") {
      val nev = NonEmptyVector(43)
      val updated = nev.prepend(41)
      updated shouldBe NonEmptyVector(41, 43)
    }

    it("should support append") {
      val nev = NonEmptyVector(41)
      val updated = nev.append(43)
      updated shouldBe NonEmptyVector(41, 43)
    }

    it("should support concatNE") {
      val nev1 = NonEmptyVector(41)
      val nev2 = NonEmptyVector(43, 45)
      val combined = nev1.concatNE(nev2)
      combined shouldBe NonEmptyVector(41, 43, 45)
    }

    it("should support updated") {
      val nev = NonEmptyVector(41, 44)
      val updated = nev.updated(1, 43)
      updated shouldBe NonEmptyVector(41, 43)
    }

    it("should support map and return a NonEmptyVector") {
      val nev = NonEmptyVector(1, 2)
      val mapped = nev.map(_ * 2)
      mapped shouldBe NonEmptyVector(2, 4)
    }

    it("should support filter and return a Vector") {
      val nev = NonEmptyVector(1, 2, 3)
      val filtered = nev.filter(_ > 1)
      filtered shouldBe Vector(2, 3)

      val filteredEmpty = nev.filter(_ > 10)
      filteredEmpty shouldBe Vector.empty
    }

    it("should support tail and return a Vector") {
      NonEmptyVector(41, 43).tail shouldBe Vector(43)
      NonEmptyVector(41).tail shouldBe Vector.empty
    }

    it("should support init and return a Vector") {
      NonEmptyVector(41, 43).init shouldBe Vector(41)
      NonEmptyVector(41).init shouldBe Vector.empty
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
      NonEmptyVector(41, 43).toString shouldBe "NonEmptyVector(41, 43)"
    }
  }
}
