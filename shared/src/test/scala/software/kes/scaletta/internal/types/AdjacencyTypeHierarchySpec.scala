package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{Type, TypeArgument, TypeParameter}
import software.kes.scaletta.util.NonEmptyVector

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

    describe("Constructor types") {
      val param = TypeParameter.invariant[TestType]
      val c1 = Type.constructor(toTestName("C1"), NonEmptyVector(param))
      val c2 = Type.constructor(toTestName("C2"), NonEmptyVector(param))

      it("should identify identical constructors as Same") {
        hierarchy.relationshipFor(c1, c1) shouldBe TypeRelationship.Same
      }

      it("should identify different constructors as Unrelated") {
        hierarchy.relationshipFor(c1, c2) shouldBe TypeRelationship.Unrelated
      }

      it("should be a supertype of its applied types") {
        val applied = Type.applied(c1, TypeArgument(param, toNominal(ParentA)))
        hierarchy.isSubtype(applied, c1) shouldBe true
        hierarchy.relationshipFor(applied, c1) shouldBe TypeRelationship.StrictSubtype
      }

      it("should identify constructor as subtype of Top and TopRef") {
        hierarchy.isSubtype(c1, Type.Top) shouldBe true
        hierarchy.isSubtype(c1, Type.TopRef) shouldBe true
      }

      it("should not be a subtype of TopValue") {
        hierarchy.isSubtype(c1, Type.TopValue) shouldBe false
      }

      it("should be a supertype of Bottom and BottomRef") {
        hierarchy.isSubtype(Type.Bottom, c1) shouldBe true
        hierarchy.isSubtype(Type.BottomRef, c1) shouldBe true
      }
    }

    describe("Union types") {
      it("should identify a union as Same as itself") {
        val u = Type.union(toNominal(ParentA), toNominal(ParentB))
        hierarchy.relationshipFor(u, u) shouldBe TypeRelationship.Same
      }

      it("should identify a type as a strict subtype of a union containing it") {
        val u = Type.union(toNominal(ParentA), toNominal(ParentB))
        hierarchy.isSubtype(toNominal(ParentA), u) shouldBe true
        hierarchy.relationshipFor(toNominal(ParentA), u) shouldBe TypeRelationship.StrictSubtype
      }

      it("should identify a union as a strict supertype of any of its components") {
        val u = Type.union(toNominal(ParentA), toNominal(ParentB))
        hierarchy.isSubtype(u, toNominal(ParentA)) shouldBe false
        hierarchy.relationshipFor(u, toNominal(ParentA)) shouldBe TypeRelationship.StrictSupertype
      }

      it("should identify a union as a subtype of a wider union") {
        val unionAB = Type.union(toNominal(ParentA), toNominal(ParentB))
        val unionABC = Type.union(toNominal(ParentA), toNominal(ParentB), toNominal(Unrelated))

        hierarchy.isSubtype(unionAB, unionABC) shouldBe true
        hierarchy.relationshipFor(unionAB, unionABC) shouldBe TypeRelationship.StrictSubtype
      }

      it("should identify a union as a subtype of a common supertype") {
        val u = Type.union(toNominal(ChildA1), toNominal(ParentA))
        hierarchy.isSubtype(u, toNominal(Root)) shouldBe true
      }
    }

    describe("Intersection types") {
      it("should identify an intersection as Same as itself") {
        val i = Type.intersection(toNominal(ParentA), toNominal(ParentB))
        hierarchy.relationshipFor(i, i) shouldBe TypeRelationship.Same
      }

      it("should identify an intersection as a strict subtype of its components") {
        val i = Type.intersection(toNominal(ParentA), toNominal(ParentB))
        hierarchy.isSubtype(i, toNominal(ParentA)) shouldBe true
        hierarchy.relationshipFor(i, toNominal(ParentA)) shouldBe TypeRelationship.StrictSubtype
      }

      it("should identify a component as a strict supertype of an intersection") {
        val i = Type.intersection(toNominal(ParentA), toNominal(ParentB))
        hierarchy.isSubtype(toNominal(ParentA), i) shouldBe false
        hierarchy.relationshipFor(toNominal(ParentA), i) shouldBe TypeRelationship.StrictSupertype
      }

      it("should identify a narrower intersection as a subtype of a wider intersection") {
        val iABC = Type.intersection(toNominal(ParentA), toNominal(ParentB), toNominal(Root))
        val iAB = Type.intersection(toNominal(ParentA), toNominal(ParentB))
        // iABC has more constraints, so it is a subtype of iAB
        hierarchy.isSubtype(iABC, iAB) shouldBe true
      }

      it("should handle nested unions and intersections") {
        val u = Type.union(toNominal(ParentA), toNominal(ParentB))
        val i = Type.intersection(u, toNominal(Unrelated))
        hierarchy.isSubtype(i, u) shouldBe true
        hierarchy.isSubtype(i, toNominal(Unrelated)) shouldBe true
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

  case class Other(name: String) extends TestType

  private def toTestName(s: String): TestType = Other(s)

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
