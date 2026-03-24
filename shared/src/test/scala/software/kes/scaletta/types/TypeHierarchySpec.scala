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
    private def toNominal(t: TestType): Type[TestType] = Type.Nominal(t)

    def relationshipFor(lhs: Type[TestType], rhs: Type[TestType]): TypeRelationship[TestType] = {
      if (lhs == rhs) TypeRelationship.Same
      else (lhs, rhs) match {
        case (Type.Nominal(l), Type.Nominal(r)) =>
          (l, r) match {
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
            case (ParentA, ParentB) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ParentB, ParentA) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ChildA1, ParentB) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ParentB, ChildA1) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ChildB1, ParentA) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ParentA, ChildB1) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ChildA1, ChildB1) => TypeRelationship.HaveCommonSupertype(toNominal(Root))
            case (ChildB1, ChildA1) => TypeRelationship.HaveCommonSupertype(toNominal(Root))

            // DiamondChild Common Supertypes
            case (DiamondChild, Unrelated) => TypeRelationship.None
            case (Unrelated, DiamondChild) => TypeRelationship.None

            case _ => TypeRelationship.None
          }
        case _ => TypeRelationship.None
      }
    }

    def immediateSupertypes(t: Type[TestType]): Iterable[Type[TestType]] =
      t match {
        case Type.Nominal(name) =>
          name match {
            case DiamondChild => List(toNominal(ChildA1), toNominal(ChildB1))
            case ChildA1 => List(toNominal(ParentA))
            case ChildB1 => List(toNominal(ParentB))
            case ParentA => List(toNominal(Root))
            case ParentB => List(toNominal(Root))
            case Root => Nil
            case Unrelated => Nil
          }
        case _ => Nil
      }
  }

  private def toNominal(t: TestType): Type[TestType] = Type.Nominal(t)

  describe("TypeHierarchy Operations") {
    it("should identify identical types as Same") {
      testHierarchy.relationshipFor(toNominal(ChildA1), toNominal(ChildA1)) shouldBe TypeRelationship.Same
      testHierarchy.relationshipFor(toNominal(Root), toNominal(Root)) shouldBe TypeRelationship.Same
    }

    it("should identify strict subtypes") {
      testHierarchy.relationshipFor(toNominal(ChildA1), toNominal(ParentA)) shouldBe TypeRelationship.StrictSubtype
      testHierarchy.relationshipFor(toNominal(ChildA1), toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
      testHierarchy.relationshipFor(toNominal(ParentA), toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
    }

    it("should identify strict supertypes") {
      testHierarchy.relationshipFor(toNominal(ParentA), toNominal(ChildA1)) shouldBe TypeRelationship.StrictSupertype
      testHierarchy.relationshipFor(toNominal(Root), toNominal(ChildA1)) shouldBe TypeRelationship.StrictSupertype
      testHierarchy.relationshipFor(toNominal(Root), toNominal(ParentA)) shouldBe TypeRelationship.StrictSupertype
    }

    it("should identify types with a common supertype (LUB)") {
      testHierarchy.relationshipFor(toNominal(ChildA1), toNominal(ParentB)) shouldBe TypeRelationship.HaveCommonSupertype(toNominal(Root))
      testHierarchy.relationshipFor(toNominal(ParentA), toNominal(ParentB)) shouldBe TypeRelationship.HaveCommonSupertype(toNominal(Root))
      testHierarchy.relationshipFor(toNominal(ChildA1), toNominal(ChildB1)) shouldBe TypeRelationship.HaveCommonSupertype(toNominal(Root))
    }

    it("should return None for unrelated types") {
      testHierarchy.relationshipFor(toNominal(Root), toNominal(Unrelated)) shouldBe TypeRelationship.None
      testHierarchy.relationshipFor(toNominal(ChildA1), toNominal(Unrelated)) shouldBe TypeRelationship.None
    }

    it("should correctly handle covariance in TypeRelationship") {
      // This is more of a compile-time check, but we can assert it works
      val rel: TypeRelationship[TestType] = TypeRelationship.Same
      rel shouldBe TypeRelationship.Same
    }

    describe("immediateSupertypes") {
      it("should return ParentA for ChildA1") {
        testHierarchy.immediateSupertypes(toNominal(ChildA1)) should contain theSameElementsAs List(toNominal(ParentA))
      }

      it("should return Root for ParentA") {
        testHierarchy.immediateSupertypes(toNominal(ParentA)) should contain theSameElementsAs List(toNominal(Root))
      }

      it("should return Nil for Root") {
        testHierarchy.immediateSupertypes(toNominal(Root)) shouldBe empty
      }

      it("should return Nil for Unrelated") {
        testHierarchy.immediateSupertypes(toNominal(Unrelated)) shouldBe empty
      }

      it("should return both ChildA1 and ChildB1 for DiamondChild") {
        testHierarchy.immediateSupertypes(toNominal(DiamondChild)) should contain theSameElementsAs List(toNominal(ChildA1), toNominal(ChildB1))
      }
    }
  }
}
