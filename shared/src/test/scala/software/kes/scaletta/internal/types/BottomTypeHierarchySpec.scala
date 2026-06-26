package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Type

class BottomTypeHierarchySpec extends AnyFunSpec with Matchers {
  describe("AdjacencyTypeHierarchy with Bottom types") {
    it("should treat Bottom as a subtype of any nominal type") {
      hierarchy.relationshipFor(Type.bottom, toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(Type.bottom, toNominal(ChildA1)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(Type.bottom, toNominal(Unrelated)) shouldBe TypeRelationship.StrictSubtype
    }

    it("should treat Bottom as a subtype of any other built-in type") {
      hierarchy.relationshipFor(Type.bottom, Type.unit) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(Type.bottom, Type.bottomRef) shouldBe TypeRelationship.StrictSubtype
    }

    it("should treat Bottom as Same as Bottom") {
      hierarchy.relationshipFor(Type.bottom, Type.bottom) shouldBe TypeRelationship.Same
    }

    it("should treat any type as a supertype of Bottom") {
      hierarchy.relationshipFor(toNominal(Root), Type.bottom) shouldBe TypeRelationship.StrictSupertype
    }

    it("should treat BottomRef as a subtype of BottomRef") {
      hierarchy.relationshipFor(Type.bottomRef, Type.bottomRef) shouldBe TypeRelationship.Same
    }

    it("should treat Bottom as a subtype of BottomRef") {
      hierarchy.relationshipFor(Type.bottom, Type.bottomRef) shouldBe TypeRelationship.StrictSubtype
    }

    it("should treat BottomRef as a supertype of Bottom") {
      hierarchy.relationshipFor(Type.bottomRef, Type.bottom) shouldBe TypeRelationship.StrictSupertype
    }

    it("should correctly identify subtypes via isSubtype") {
      hierarchy.isSubtype(Type.bottom, toNominal(Root)) shouldBe true
      hierarchy.isSubtype(Type.bottom, Type.bottomRef) shouldBe true
      hierarchy.isSubtype(Type.bottom, Type.bottom) shouldBe true
      hierarchy.isSubtype(Type.bottomRef, Type.bottom) shouldBe false
      hierarchy.isSubtype(toNominal(Root), Type.bottom) shouldBe false
    }
  }

  sealed trait TestType

  case object Root extends TestType

  case object ParentA extends TestType

  case object ChildA1 extends TestType

  case object Unrelated extends TestType

  private def toNominal(t: TestType): Type[TestType] = Type.Nominal(t)

  private lazy val hierarchyMap: Map[Type[TestType], Set[Type[TestType]]] = Map(
    toNominal(ChildA1) -> Set(toNominal(ParentA)),
    toNominal(ParentA) -> Set(toNominal(Root))
  )

  private lazy val hierarchy = AdjacencyTypeHierarchy.fromMap(hierarchyMap, Set.empty[Type.Nominal[TestType]])
}
