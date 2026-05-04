package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Type

class VarianceMismatchSpec extends AnyFunSpec with Matchers {

  describe("VarianceMismatch.check") {

    describe("Invariant") {
      it("should return None for TypeRelationship.Same") {
        VarianceMismatch.check(Variance.Invariant, TypeRelationship.Same) shouldBe None
      }

      it("should return InvariantSubtype for TypeRelationship.StrictSubtype") {
        VarianceMismatch.check(Variance.Invariant, TypeRelationship.StrictSubtype) shouldBe Some(VarianceMismatch.InvariantSubtype)
      }

      it("should return InvariantSupertype for TypeRelationship.StrictSupertype") {
        VarianceMismatch.check(Variance.Invariant, TypeRelationship.StrictSupertype) shouldBe Some(VarianceMismatch.InvariantSupertype)
      }

      it("should return InvariantUnrelated for TypeRelationship.HaveCommonSupertype") {
        val rel = TypeRelationship.HaveCommonSupertype(Type.Nominal("Root"))
        VarianceMismatch.check(Variance.Invariant, rel) shouldBe Some(VarianceMismatch.InvariantUnrelated)
      }

      it("should return InvariantUnrelated for TypeRelationship.Unrelated") {
        VarianceMismatch.check(Variance.Invariant, TypeRelationship.Unrelated) shouldBe Some(VarianceMismatch.InvariantUnrelated)
      }
    }

    describe("Covariant") {
      it("should return None for TypeRelationship.Same") {
        VarianceMismatch.check(Variance.Covariant, TypeRelationship.Same) shouldBe None
      }

      it("should return None for TypeRelationship.StrictSubtype") {
        VarianceMismatch.check(Variance.Covariant, TypeRelationship.StrictSubtype) shouldBe None
      }

      it("should return CovariantSupertype for TypeRelationship.StrictSupertype") {
        VarianceMismatch.check(Variance.Covariant, TypeRelationship.StrictSupertype) shouldBe Some(VarianceMismatch.CovariantSupertype)
      }

      it("should return CovariantUnrelated for TypeRelationship.HaveCommonSupertype") {
        val rel = TypeRelationship.HaveCommonSupertype(Type.Nominal("Root"))
        VarianceMismatch.check(Variance.Covariant, rel) shouldBe Some(VarianceMismatch.CovariantUnrelated)
      }

      it("should return CovariantUnrelated for TypeRelationship.Unrelated") {
        VarianceMismatch.check(Variance.Covariant, TypeRelationship.Unrelated) shouldBe Some(VarianceMismatch.CovariantUnrelated)
      }
    }

    describe("Contravariant") {
      it("should return None for TypeRelationship.Same") {
        VarianceMismatch.check(Variance.Contravariant, TypeRelationship.Same) shouldBe None
      }

      it("should return ContravariantSubtype for TypeRelationship.StrictSubtype") {
        VarianceMismatch.check(Variance.Contravariant, TypeRelationship.StrictSubtype) shouldBe Some(VarianceMismatch.ContravariantSubtype)
      }

      it("should return None for TypeRelationship.StrictSupertype") {
        VarianceMismatch.check(Variance.Contravariant, TypeRelationship.StrictSupertype) shouldBe None
      }

      it("should return ContravariantUnrelated for TypeRelationship.HaveCommonSupertype") {
        val rel = TypeRelationship.HaveCommonSupertype(Type.Nominal("Root"))
        VarianceMismatch.check(Variance.Contravariant, rel) shouldBe Some(VarianceMismatch.ContravariantUnrelated)
      }

      it("should return ContravariantUnrelated for TypeRelationship.Unrelated") {
        VarianceMismatch.check(Variance.Contravariant, TypeRelationship.Unrelated) shouldBe Some(VarianceMismatch.ContravariantUnrelated)
      }
    }
  }

  describe("VarianceMismatch property verification") {

    it("InvariantSubtype should have correct properties") {
      val m = VarianceMismatch.InvariantSubtype
      m.variance shouldBe Variance.Invariant
      m.actual shouldBe VarianceRelationship.Subtype
      m.expected shouldBe VarianceRelationship.Same
    }

    it("InvariantSupertype should have correct properties") {
      val m = VarianceMismatch.InvariantSupertype
      m.variance shouldBe Variance.Invariant
      m.actual shouldBe VarianceRelationship.Supertype
      m.expected shouldBe VarianceRelationship.Same
    }

    it("InvariantUnrelated should have correct properties") {
      val m = VarianceMismatch.InvariantUnrelated
      m.variance shouldBe Variance.Invariant
      m.actual shouldBe VarianceRelationship.Unrelated
      m.expected shouldBe VarianceRelationship.Same
    }

    it("CovariantSupertype should have correct properties") {
      val m = VarianceMismatch.CovariantSupertype
      m.variance shouldBe Variance.Covariant
      m.actual shouldBe VarianceRelationship.Supertype
      m.expected shouldBe VarianceRelationship.Subtype
    }

    it("CovariantUnrelated should have correct properties") {
      val m = VarianceMismatch.CovariantUnrelated
      m.variance shouldBe Variance.Covariant
      m.actual shouldBe VarianceRelationship.Unrelated
      m.expected shouldBe VarianceRelationship.Subtype
    }

    it("ContravariantSubtype should have correct properties") {
      val m = VarianceMismatch.ContravariantSubtype
      m.variance shouldBe Variance.Contravariant
      m.actual shouldBe VarianceRelationship.Subtype
      m.expected shouldBe VarianceRelationship.Supertype
    }

    it("ContravariantUnrelated should have correct properties") {
      val m = VarianceMismatch.ContravariantUnrelated
      m.variance shouldBe Variance.Contravariant
      m.actual shouldBe VarianceRelationship.Unrelated
      m.expected shouldBe VarianceRelationship.Supertype
    }
  }
}
