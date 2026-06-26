package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{ConcreteType, Type}

class BottomTypeHierarchySpec extends AnyFunSpec with Matchers {
  private def bottom: ConcreteType[TestType] = Type.bottom[TestType]

  private def bottomRef: ConcreteType[TestType] = Type.bottomRef[TestType]

  private def unit: ConcreteType[TestType] = Type.unit[TestType]

  describe("AdjacencyTypeHierarchy with Bottom types") {
    it("should treat Bottom as a subtype of any nominal type") {
      hierarchy.relationshipFor(bottom, toNominal(Root)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(bottom, toNominal(ChildA1)) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(bottom, toNominal(Unrelated)) shouldBe TypeRelationship.StrictSubtype
    }

    it("should treat Bottom as a subtype of any other built-in type") {
      hierarchy.relationshipFor(bottom, unit) shouldBe TypeRelationship.StrictSubtype
      hierarchy.relationshipFor(bottom, bottomRef) shouldBe TypeRelationship.StrictSubtype
    }

    it("should treat Bottom as Same as Bottom") {
      hierarchy.relationshipFor(bottom, bottom) shouldBe TypeRelationship.Same
    }

    it("should treat any type as a supertype of Bottom") {
      hierarchy.relationshipFor(toNominal(Root), bottom) shouldBe TypeRelationship.StrictSupertype
    }

    it("should treat BottomRef as a subtype of BottomRef") {
      hierarchy.relationshipFor(bottomRef, bottomRef) shouldBe TypeRelationship.Same
    }

    it("should treat Bottom as a subtype of BottomRef") {
      hierarchy.relationshipFor(bottom, bottomRef) shouldBe TypeRelationship.StrictSubtype
    }

    it("should treat BottomRef as a supertype of Bottom") {
      hierarchy.relationshipFor(bottomRef, bottom) shouldBe TypeRelationship.StrictSupertype
    }

    it("should treat BottomRef as a subtype of non-value nominal types") {
      hierarchy.isSubtype(bottomRef, toNominal(Root)) shouldBe true
      hierarchy.isSubtype(bottomRef, toNominal(ParentA)) shouldBe true
      hierarchy.isSubtype(bottomRef, toNominal(ChildA1)) shouldBe true
    }

    it("should NOT treat BottomRef as a subtype of value nominal types") {
      hierarchy.isSubtype(bottomRef, toNominal(ValueType)) shouldBe false
    }

    it("should treat BottomRef as a subtype of non-nominal types") {
      hierarchy.isSubtype(bottomRef, unit) shouldBe true
      hierarchy.isSubtype(bottomRef, Type.function(toNominal(Root))(toNominal(Root))) shouldBe true
      hierarchy.isSubtype(bottomRef, Type.tuple(toNominal(Root), toNominal(Root))) shouldBe true
    }

    it("should correctly identify subtypes via isSubtype") {
      hierarchy.isSubtype(bottom, toNominal(Root)) shouldBe true
      hierarchy.isSubtype(bottom, bottomRef) shouldBe true
      hierarchy.isSubtype(bottom, bottom) shouldBe true
      hierarchy.isSubtype(bottomRef, bottom) shouldBe false
      hierarchy.isSubtype(toNominal(Root), bottom) shouldBe false
    }
  }

  sealed trait TestType

  case object Root extends TestType

  case object ParentA extends TestType

  case object ChildA1 extends TestType

  case object ValueType extends TestType

  case object Unrelated extends TestType

  private def toNominal(t: TestType): Type[TestType] = Type.Nominal(t)

  private lazy val hierarchyMap: Map[Type[TestType], Set[Type[TestType]]] = Map(
    toNominal(ChildA1) -> Set(toNominal(ParentA)),
    toNominal(ParentA) -> Set(toNominal(Root))
  )

  private lazy val hierarchy: TypeHierarchy[TestType] = AdjacencyTypeHierarchy.fromMap(hierarchyMap, Set(Type.Nominal(ValueType)))
}
