package software.kes.scaletta.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TypeHierarchySpec extends AnyFunSpec with Matchers {

  // A simple, localized type system for testing
  sealed trait TestType

  case object Root extends TestType

  case object ParentA extends TestType

  case object ParentB extends TestType

  case object ChildA1 extends TestType

  case object ChildB1 extends TestType

  case object DiamondChild extends TestType

  case object Unrelated extends TestType

  /*
   * A concrete implementation of TypeHierarchy for the TestType system.
   * Tree structure:
   *        Root
   *       /    \
   *   ParentA  ParentB
   *      |        |
   *   ChildA1  ChildB1
   *      \       /
   *     DiamondChild
   */
  private val testHierarchy = new TypeHierarchy[TestType] {
    def relationshipFor(lhs: TestType, rhs: TestType): TypeRelationship[TestType] = {
      if (lhs == rhs) TypeRelationship.Same
      else (lhs, rhs) match {
        // Strict Subtypes
        case (ParentA, Root) => TypeRelationship.StrictSubtype
        case (ParentB, Root) => TypeRelationship.StrictSubtype
        case (ChildA1, Root) => TypeRelationship.StrictSubtype
        case (ChildB1, Root) => TypeRelationship.StrictSubtype
        case (ChildA1, ParentA) => TypeRelationship.StrictSubtype
        case (ChildB1, ParentB) => TypeRelationship.StrictSubtype

        // DiamondChild Subtypes
        case (DiamondChild, Root) => TypeRelationship.StrictSubtype
        case (DiamondChild, ParentA) => TypeRelationship.StrictSubtype
        case (DiamondChild, ParentB) => TypeRelationship.StrictSubtype
        case (DiamondChild, ChildA1) => TypeRelationship.StrictSubtype
        case (DiamondChild, ChildB1) => TypeRelationship.StrictSubtype

        // Strict Supertypes
        case (Root, ParentA) => TypeRelationship.StrictSupertype
        case (Root, ParentB) => TypeRelationship.StrictSupertype
        case (Root, ChildA1) => TypeRelationship.StrictSupertype
        case (Root, ChildB1) => TypeRelationship.StrictSupertype
        case (ParentA, ChildA1) => TypeRelationship.StrictSupertype
        case (ParentB, ChildB1) => TypeRelationship.StrictSupertype

        // DiamondChild Supertypes
        case (Root, DiamondChild) => TypeRelationship.StrictSupertype
        case (ParentA, DiamondChild) => TypeRelationship.StrictSupertype
        case (ParentB, DiamondChild) => TypeRelationship.StrictSupertype
        case (ChildA1, DiamondChild) => TypeRelationship.StrictSupertype
        case (ChildB1, DiamondChild) => TypeRelationship.StrictSupertype

        // Common Supertypes (LUB)
        case (ParentA, ParentB) => TypeRelationship.HaveCommonSupertype(Root)
        case (ParentB, ParentA) => TypeRelationship.HaveCommonSupertype(Root)
        case (ChildA1, ParentB) => TypeRelationship.HaveCommonSupertype(Root)
        case (ParentB, ChildA1) => TypeRelationship.HaveCommonSupertype(Root)
        case (ChildB1, ParentA) => TypeRelationship.HaveCommonSupertype(Root)
        case (ParentA, ChildB1) => TypeRelationship.HaveCommonSupertype(Root)
        case (ChildA1, ChildB1) => TypeRelationship.HaveCommonSupertype(Root)
        case (ChildB1, ChildA1) => TypeRelationship.HaveCommonSupertype(Root)

        // DiamondChild Common Supertypes
        case (DiamondChild, Unrelated) => TypeRelationship.None
        case (Unrelated, DiamondChild) => TypeRelationship.None

        // ParentA is a common supertype for its branch, but we only return HaveCommonSupertype
        // when there is no direct subtyping relationship.
        // Actually, for ChildA1 and some other child of ParentA (if it existed), ParentA would be LUB.

        case _ => TypeRelationship.None
      }
    }

    def immediateSupertypes(t: TestType): Iterable[TestType] =
      t match {
        case DiamondChild => List(ChildA1, ChildB1)
        case ChildA1 => List(ParentA)
        case ChildB1 => List(ParentB)
        case ParentA => List(Root)
        case ParentB => List(Root)
        case Root => Nil
        case Unrelated => Nil
      }
  }

  describe("TypeRelationship") {
    describe("Same") {
      val rel: TypeRelationship[TestType] = TypeRelationship.Same
      it("should have correct property values") {
        rel.isSame shouldBe true
        rel.isSubtype shouldBe true
        rel.isSupertype shouldBe true
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe None
      }
    }

    describe("StrictSubtype") {
      val rel: TypeRelationship[TestType] = TypeRelationship.StrictSubtype
      it("should have correct property values") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe true
        rel.isSupertype shouldBe false
        rel.isStrictSubtype shouldBe true
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe None
      }
    }

    describe("StrictSupertype") {
      val rel: TypeRelationship[TestType] = TypeRelationship.StrictSupertype
      it("should have correct property values") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe false
        rel.isSupertype shouldBe true
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe true
        rel.commonSupertype shouldBe None
      }
    }

    describe("HaveCommonSupertype") {
      val rel = TypeRelationship.HaveCommonSupertype(Root)
      it("should have correct property values") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe false
        rel.isSupertype shouldBe false
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe Some(Root)
      }
    }

    describe("None") {
      val rel: TypeRelationship[TestType] = TypeRelationship.None
      it("should have correct property values") {
        rel.isSame shouldBe false
        rel.isSubtype shouldBe false
        rel.isSupertype shouldBe false
        rel.isStrictSubtype shouldBe false
        rel.isStrictSupertype shouldBe false
        rel.commonSupertype shouldBe None
      }
    }
  }

  describe("TypeHierarchy Operations") {
    it("should identify identical types as Same") {
      testHierarchy.relationshipFor(ChildA1, ChildA1) shouldBe TypeRelationship.Same
      testHierarchy.relationshipFor(Root, Root) shouldBe TypeRelationship.Same
    }

    it("should identify strict subtypes") {
      testHierarchy.relationshipFor(ChildA1, ParentA) shouldBe TypeRelationship.StrictSubtype
      testHierarchy.relationshipFor(ChildA1, Root) shouldBe TypeRelationship.StrictSubtype
      testHierarchy.relationshipFor(ParentA, Root) shouldBe TypeRelationship.StrictSubtype
    }

    it("should identify strict supertypes") {
      testHierarchy.relationshipFor(ParentA, ChildA1) shouldBe TypeRelationship.StrictSupertype
      testHierarchy.relationshipFor(Root, ChildA1) shouldBe TypeRelationship.StrictSupertype
      testHierarchy.relationshipFor(Root, ParentA) shouldBe TypeRelationship.StrictSupertype
    }

    it("should identify types with a common supertype (LUB)") {
      testHierarchy.relationshipFor(ChildA1, ParentB) shouldBe TypeRelationship.HaveCommonSupertype(Root)
      testHierarchy.relationshipFor(ParentA, ParentB) shouldBe TypeRelationship.HaveCommonSupertype(Root)
      testHierarchy.relationshipFor(ChildA1, ChildB1) shouldBe TypeRelationship.HaveCommonSupertype(Root)
    }

    it("should return None for unrelated types") {
      testHierarchy.relationshipFor(Root, Unrelated) shouldBe TypeRelationship.None
      testHierarchy.relationshipFor(ChildA1, Unrelated) shouldBe TypeRelationship.None
    }

    it("should correctly handle covariance in TypeRelationship") {
      // This is more of a compile-time check, but we can assert it works
      val rel: TypeRelationship[TestType] = TypeRelationship.Same
      rel shouldBe TypeRelationship.Same
    }

    describe("immediateSupertypes") {
      it("should return ParentA for ChildA1") {
        testHierarchy.immediateSupertypes(ChildA1) should contain theSameElementsAs List(ParentA)
      }

      it("should return Root for ParentA") {
        testHierarchy.immediateSupertypes(ParentA) should contain theSameElementsAs List(Root)
      }

      it("should return Nil for Root") {
        testHierarchy.immediateSupertypes(Root) shouldBe empty
      }

      it("should return Nil for Unrelated") {
        testHierarchy.immediateSupertypes(Unrelated) shouldBe empty
      }

      it("should return both ChildA1 and ChildB1 for DiamondChild") {
        testHierarchy.immediateSupertypes(DiamondChild) should contain theSameElementsAs List(ChildA1, ChildB1)
      }
    }
  }
}
