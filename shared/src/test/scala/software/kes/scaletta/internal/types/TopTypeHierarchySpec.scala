package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{ProperType, Type}

class TopTypeHierarchySpec extends AnyFunSpec with Matchers {
  private def top: ProperType[TestType] = Type.top[TestType]

  private def topValue: ProperType[TestType] = Type.topValue[TestType]

  private def topRef: ProperType[TestType] = Type.topRef[TestType]

  private def bottom: ProperType[TestType] = Type.bottom[TestType]

  private def bottomRef: ProperType[TestType] = Type.bottomRef[TestType]

  private def unit: ProperType[TestType] = Type.unit[TestType]

  describe("AdjacencyTypeHierarchy with Top types") {
    it("should treat all types as subtypes of Top") {
      hierarchy.isSubtype(toNominal(Root), top) shouldBe true
      hierarchy.isSubtype(toNominal(ValueType), top) shouldBe true
      hierarchy.isSubtype(unit, top) shouldBe true
      hierarchy.isSubtype(bottom, top) shouldBe true
      hierarchy.isSubtype(bottomRef, top) shouldBe true
      hierarchy.isSubtype(topValue, top) shouldBe true
      hierarchy.isSubtype(topRef, top) shouldBe true
      hierarchy.isSubtype(Type.function(toNominal(Root))(toNominal(Root)), top) shouldBe true
    }

    it("should treat value types as subtypes of TopValue") {
      hierarchy.isSubtype(toNominal(ValueType), topValue) shouldBe true
      hierarchy.isSubtype(unit, topValue) shouldBe true
      hierarchy.isSubtype(bottom, topValue) shouldBe true
      hierarchy.isSubtype(topValue, topValue) shouldBe true
    }

    it("should NOT treat reference types as subtypes of TopValue") {
      hierarchy.isSubtype(toNominal(Root), topValue) shouldBe false
      hierarchy.isSubtype(bottomRef, topValue) shouldBe false
      hierarchy.isSubtype(topRef, topValue) shouldBe false
      hierarchy.isSubtype(Type.function(toNominal(Root))(toNominal(Root)), topValue) shouldBe false
    }

    it("should treat reference types as subtypes of TopRef") {
      hierarchy.isSubtype(toNominal(Root), topRef) shouldBe true
      hierarchy.isSubtype(toNominal(ParentA), topRef) shouldBe true
      hierarchy.isSubtype(bottom, topRef) shouldBe true
      hierarchy.isSubtype(bottomRef, topRef) shouldBe true
      hierarchy.isSubtype(topRef, topRef) shouldBe true
      hierarchy.isSubtype(Type.function(toNominal(Root))(toNominal(Root)), topRef) shouldBe true
      hierarchy.isSubtype(Type.tuple(toNominal(Root), toNominal(Root)), topRef) shouldBe true
    }

    it("should NOT treat value types as subtypes of TopRef") {
      hierarchy.isSubtype(toNominal(ValueType), topRef) shouldBe false
      hierarchy.isSubtype(unit, topRef) shouldBe false
      hierarchy.isSubtype(topValue, topRef) shouldBe false
    }

    it("should treat Top as a supertype of everything") {
      hierarchy.relationshipFor(top, toNominal(Root)) shouldBe TypeRelationship.StrictSupertype
      hierarchy.relationshipFor(top, topValue) shouldBe TypeRelationship.StrictSupertype
      hierarchy.relationshipFor(top, topRef) shouldBe TypeRelationship.StrictSupertype
      hierarchy.relationshipFor(top, top) shouldBe TypeRelationship.Same
    }

    it("should treat TopValue and TopRef as disjoint (except for Bottom)") {
      hierarchy.isSubtype(topValue, topRef) shouldBe false
      hierarchy.isSubtype(topRef, topValue) shouldBe false
      hierarchy.relationshipFor(topValue, topRef) shouldBe TypeRelationship.HaveCommonSupertype(top)
    }
  }

  sealed trait TestType

  case object Root extends TestType

  case object ParentA extends TestType

  case object ValueType extends TestType

  private def toNominal(t: TestType): ProperType[TestType] = Type.Nominal(t)

  private lazy val hierarchyMap: Map[Type[TestType], Set[Type[TestType]]] = Map(
    toNominal(ParentA) -> Set(toNominal(Root))
  )

  private lazy val hierarchy: TypeHierarchy[TestType] = AdjacencyTypeHierarchy.fromMap(hierarchyMap, Set(Type.Nominal(ValueType)))
}
