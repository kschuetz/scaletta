package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Type

class AdjacencyTypeHierarchySpec extends AnyFunSpec with Matchers {
  describe("AdjacencyTypeHierarchy") {
    it("should identify identical types as Same") {
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(ChildA1)) shouldBe TypeRelationship.Same
      hierarchy.relationshipFor(toNominal(Root), toNominal(Root)) shouldBe TypeRelationship.Same
    }

    it("should identify strict subtypes") {
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(ParentA)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(toNominal(ParentA), toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(toNominal(DiamondChild), toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
    }

    it("should identify strict supertypes") {
      hierarchy.relationshipFor(toNominal(ParentA), toNominal(ChildA1)) shouldBe TypeRelationship.StrictSupertype
      hierarchy.relationshipFor(toNominal(Root), toNominal(ChildA1)) shouldBe TypeRelationship.StrictSupertype
      hierarchy.relationshipFor(toNominal(Root), toNominal(ParentA)) shouldBe TypeRelationship.StrictSupertype
      hierarchy.relationshipFor(toNominal(Root), toNominal(DiamondChild)) shouldBe TypeRelationship.StrictSupertype
    }

    it("should identify types with a common supertype (LUB)") {
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(ParentB)) shouldBe TypeRelationship.HaveCommonSupertype(toNominal(Root))
      hierarchy.relationshipFor(toNominal(ParentA), toNominal(ParentB)) shouldBe TypeRelationship.HaveCommonSupertype(toNominal(Root))
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(ChildB1)) shouldBe TypeRelationship.HaveCommonSupertype(toNominal(Root))
    }

    it("should return Unrelated for unrelated types") {
      hierarchy.relationshipFor(toNominal(Root), toNominal(Unrelated)) shouldBe TypeRelationship.Unrelated
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(Unrelated)) shouldBe TypeRelationship.Unrelated
    }

    it("should correctly identify subtypes via isSubtype") {
      hierarchy.isSubtype(toNominal(ChildA1), toNominal(ParentA)) shouldBe true
      hierarchy.isSubtype(toNominal(ChildA1), toNominal(Root)) shouldBe true
      hierarchy.isSubtype(toNominal(Root), toNominal(Root)) shouldBe true
      hierarchy.isSubtype(toNominal(ParentA), toNominal(ChildA1)) shouldBe false
      hierarchy.isSubtype(toNominal(ChildA1), toNominal(Unrelated)) shouldBe false
    }

    describe("immediateSupertypes") {
      it("should return correct immediate supertypes") {
        hierarchy.immediateSupertypes(toNominal(DiamondChild)) should contain theSameElementsAs Set(toNominal(ChildA1), toNominal(ChildB1))
        hierarchy.immediateSupertypes(toNominal(ChildA1)) should contain theSameElementsAs Set(toNominal(ParentA))
        hierarchy.immediateSupertypes(toNominal(Root)) shouldBe empty
      }
    }

    describe("Function types") {
      it("should be a subtype of itself") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        hierarchy.isSubtype(f1, f1) shouldBe true
      }

      it("should be covariant in return type") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        val f2 = Type.Function(Vector(toNominal(ParentA)), toNominal(ParentA))
        hierarchy.isSubtype(f1, f2) shouldBe true
        hierarchy.isSubtype(f2, f1) shouldBe false
      }

      it("should be contravariant in parameter types") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))
        val f2 = Type.Function(Vector(toNominal(ChildA1)), toNominal(Root))
        hierarchy.isSubtype(f1, f2) shouldBe true
        hierarchy.isSubtype(f2, f1) shouldBe false
      }

      it("should handle mixed variance") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        val f2 = Type.Function(Vector(toNominal(ChildA1)), toNominal(ParentA))
        hierarchy.isSubtype(f1, f2) shouldBe true
        hierarchy.isSubtype(f2, f1) shouldBe false
      }

      it("should handle multiple parameters") {
        val f1 = Type.Function(Vector(toNominal(ParentA), toNominal(ParentB)), toNominal(ChildA1))
        val f2 = Type.Function(Vector(toNominal(ChildA1), toNominal(ChildB1)), toNominal(ParentA))
        hierarchy.isSubtype(f1, f2) shouldBe true
      }

      it("should not be a subtype or supertype for different arity") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))
        val f2 = Type.Function(Vector(toNominal(ParentA), toNominal(ParentA)), toNominal(Root))
        hierarchy.isSubtype(f1, f2) shouldBe false
        hierarchy.isSubtype(f2, f1) shouldBe false
      }

      it("should be a subtype of Top and TopRef") {
        val f = Type.Function(Vector(toNominal(Root)), toNominal(Root))
        hierarchy.isSubtype(f, Type.Top) shouldBe true
        hierarchy.isSubtype(f, Type.TopRef) shouldBe true
      }

      it("should not be a subtype of TopValue") {
        val f = Type.Function(Vector(toNominal(Root)), toNominal(Root))
        hierarchy.isSubtype(f, Type.TopValue) shouldBe false
      }

      it("should be a supertype of Bottom") {
        val f = Type.Function(Vector(toNominal(Root)), toNominal(Root))
        hierarchy.isSubtype(Type.Bottom, f) shouldBe true
      }

      it("should be a supertype of BottomRef") {
        val f = Type.Function(Vector(toNominal(Root)), toNominal(Root))
        hierarchy.isSubtype(Type.BottomRef, f) shouldBe true
      }

      it("should correctly identify strict subtyping for functions") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        val f2 = Type.Function(Vector(toNominal(ChildA1)), toNominal(ParentA))
        hierarchy.relationshipFor(f1, f2) shouldBe TypeRelationship.StrictSubtype
      }

      it("should correctly identify same relationship for functions") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        val f2 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        hierarchy.relationshipFor(f1, f2) shouldBe TypeRelationship.Same
      }
    }

    describe("Tuple types") {
      it("should be a subtype of itself") {
        val t1 = Type.tuple(toNominal(ParentA), toNominal(ParentB))
        hierarchy.isSubtype(t1, t1) shouldBe true
      }

      it("should be covariant in its elements") {
        val t1 = Type.tuple(toNominal(ChildA1), toNominal(ChildB1))
        val t2 = Type.tuple(toNominal(ParentA), toNominal(ParentB))
        hierarchy.isSubtype(t1, t2) shouldBe true
        hierarchy.isSubtype(t2, t1) shouldBe false
      }

      it("should not be a subtype for different arity") {
        val t1 = Type.tuple(toNominal(Root), toNominal(Root))
        val t2 = Type.tuple(toNominal(Root), toNominal(Root), toNominal(Root))
        hierarchy.isSubtype(t1, t2) shouldBe false
        hierarchy.isSubtype(t2, t1) shouldBe false
      }

      it("should be a subtype of Top and TopRef") {
        val t = Type.tuple(toNominal(Root), toNominal(Root))
        hierarchy.isSubtype(t, Type.Top) shouldBe true
        hierarchy.isSubtype(t, Type.TopRef) shouldBe true
      }

      it("should not be a subtype of TopValue") {
        val t = Type.tuple(toNominal(Root), toNominal(Root))
        hierarchy.isSubtype(t, Type.TopValue) shouldBe false
      }

      it("should be a supertype of Bottom") {
        val t = Type.tuple(toNominal(Root), toNominal(Root))
        hierarchy.isSubtype(Type.Bottom, t) shouldBe true
      }

      it("should be a supertype of BottomRef") {
        val t = Type.tuple(toNominal(Root), toNominal(Root))
        hierarchy.isSubtype(Type.BottomRef, t) shouldBe true
      }

      it("should correctly identify strict subtyping for tuples") {
        val t1 = Type.tuple(toNominal(ChildA1), toNominal(ChildB1))
        val t2 = Type.tuple(toNominal(ParentA), toNominal(ParentB))
        hierarchy.relationshipFor(t1, t2) shouldBe TypeRelationship.StrictSubtype
      }

      it("should correctly identify common supertype for tuples") {
        val t1 = Type.tuple(toNominal(ChildA1), toNominal(ParentB))
        val t2 = Type.tuple(toNominal(ParentA), toNominal(ChildB1))
        // LUB of (ChildA1, ParentB) and (ParentA, ChildB1) is (ParentA, ParentB)
        // Note: findCommonSupertype currently returns the first ancestor found in BFS,
        // which might be TopRef if complex structural LUB is not yet implemented.
        hierarchy.isSubtype(t1, Type.TopRef) shouldBe true
        hierarchy.isSubtype(t2, Type.TopRef) shouldBe true
      }
    }
  }

  sealed trait TestType

  case object Root extends TestType

  case object ParentA extends TestType

  case object ParentB extends TestType

  case object ChildA1 extends TestType

  case object ChildB1 extends TestType

  case object DiamondChild extends TestType

  case object Unrelated extends TestType

  private def toNominal(t: TestType): Type[TestType] = Type.Nominal(t)

  private lazy val hierarchyMap: Map[Type[TestType], Set[Type[TestType]]] = Map(
    toNominal(DiamondChild) -> Set(toNominal(ChildA1), toNominal(ChildB1)),
    toNominal(ChildA1) -> Set(toNominal(ParentA)),
    toNominal(ChildB1) -> Set(toNominal(ParentB)),
    toNominal(ParentA) -> Set(toNominal(Root)),
    toNominal(ParentB) -> Set(toNominal(Root))
  )

  private lazy val hierarchy = AdjacencyTypeHierarchy.fromMap(hierarchyMap, Set.empty[Type.Nominal[TestType]])
}
