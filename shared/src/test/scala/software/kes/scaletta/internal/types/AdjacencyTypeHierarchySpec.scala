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

    it("should return HaveCommonSupertype(TopRef) for unrelated reference types") {
      hierarchy.relationshipFor(toNominal(Root), toNominal(Unrelated)) shouldBe TypeRelationship.HaveCommonSupertype(Type.TopRef)
      hierarchy.relationshipFor(toNominal(ChildA1), toNominal(Unrelated)) shouldBe TypeRelationship.HaveCommonSupertype(Type.TopRef)
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
        hierarchy.immediateSupertypes(toNominal(Root)) should contain theSameElementsAs Set(Type.TopRef)
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

      it("should identify different constructors as having TopRef as common supertype") {
        hierarchy.relationshipFor(c1, c2) shouldBe TypeRelationship.HaveCommonSupertype(Type.TopRef)
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

    describe("Applied types") {
      val invariantParam = TypeParameter.invariant[TestType]
      val covariantParam = TypeParameter.covariant[TestType]
      val contravariantParam = TypeParameter.contravariant[TestType]

      val boxC = Type.constructor(toTestName("Box"), NonEmptyVector(invariantParam))
      val listC = Type.constructor(toTestName("List"), NonEmptyVector(covariantParam))
      val writerC = Type.constructor(toTestName("Writer"), NonEmptyVector(contravariantParam))

      it("should identify identical applied types as Same") {
        val listInt = Type.applied(listC, TypeArgument(covariantParam, toNominal(ChildA1)))
        hierarchy.relationshipFor(listInt, listInt) shouldBe TypeRelationship.Same
      }

      it("should respect covariance") {
        val listChild = Type.applied(listC, TypeArgument(covariantParam, toNominal(ChildA1)))
        val listParent = Type.applied(listC, TypeArgument(covariantParam, toNominal(ParentA)))
        hierarchy.isSubtype(listChild, listParent) shouldBe true
        hierarchy.isSubtype(listParent, listChild) shouldBe false
        hierarchy.relationshipFor(listChild, listParent) shouldBe TypeRelationship.StrictSubtype
      }

      it("should respect contravariance") {
        val writerChild = Type.applied(writerC, TypeArgument(contravariantParam, toNominal(ChildA1)))
        val writerParent = Type.applied(writerC, TypeArgument(contravariantParam, toNominal(ParentA)))
        hierarchy.isSubtype(writerParent, writerChild) shouldBe true
        hierarchy.isSubtype(writerChild, writerParent) shouldBe false
        hierarchy.relationshipFor(writerParent, writerChild) shouldBe TypeRelationship.StrictSubtype
      }

      it("should respect invariance") {
        val boxChild = Type.applied(boxC, TypeArgument(invariantParam, toNominal(ChildA1)))
        val boxParent = Type.applied(boxC, TypeArgument(invariantParam, toNominal(ParentA)))
        hierarchy.isSubtype(boxChild, boxParent) shouldBe false
        hierarchy.isSubtype(boxParent, boxChild) shouldBe false
        hierarchy.relationshipFor(boxChild, boxParent) shouldBe TypeRelationship.HaveCommonSupertype(boxC)
      }

      it("should be a subtype of its constructor") {
        val applied = Type.applied(listC, TypeArgument(covariantParam, toNominal(ParentA)))
        hierarchy.isSubtype(applied, listC) shouldBe true
        hierarchy.relationshipFor(applied, listC) shouldBe TypeRelationship.StrictSubtype
      }

      it("should handle mixed variance and multiple parameters") {
        val invParam = TypeParameter.invariant[TestType]
        val coParam = TypeParameter.covariant[TestType]
        val mapC = Type.constructor(toTestName("Map"), NonEmptyVector[TypeParameter[TestType]](invParam, coParam))

        val map1 = Type.applied(mapC, TypeArgument(invParam, toNominal(Root)), TypeArgument(coParam, toNominal(ChildA1)))
        val map2 = Type.applied(mapC, TypeArgument(invParam, toNominal(Root)), TypeArgument(coParam, toNominal(ParentA)))

        hierarchy.isSubtype(map1, map2) shouldBe true

        val map3 = Type.applied(mapC, TypeArgument(invParam, toNominal(ParentA)), TypeArgument(coParam, toNominal(ParentA)))
        // map2 and map3 are unrelated because keys are invariant and Root != ParentA
        hierarchy.isSubtype(map2, map3) shouldBe false
        hierarchy.isSubtype(map3, map2) shouldBe false
      }

      it("should climb the hierarchy via the constructor") {
        // Define a hierarchy of constructors
        // Suppose List[T] <: Collection[T]
        val collectionC = Type.constructor(toTestName("Collection"), NonEmptyVector(covariantParam))
        val listCWithParent = Type.constructor(toTestName("List"), NonEmptyVector(covariantParam))

        val hierarchyWithConstructors = AdjacencyTypeHierarchy.fromMap[TestType](
          hierarchyMap + (listCWithParent -> Set(collectionC)),
          Set.empty
        )

        val listInt = Type.applied(listCWithParent, TypeArgument(covariantParam, toNominal(ChildA1)))
        val collectionInt = Type.applied(collectionC, TypeArgument(covariantParam, toNominal(ChildA1)))

        hierarchyWithConstructors.isSubtype(listInt, collectionC) shouldBe true
        hierarchyWithConstructors.isSubtype(listInt, collectionInt) shouldBe true

        val collectionParent = Type.applied(collectionC, TypeArgument(covariantParam, toNominal(ParentA)))
        hierarchyWithConstructors.isSubtype(listInt, collectionParent) shouldBe true
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

      it("should find A as LUB of (A & B) and (A & C)") {
        val A = toNominal(ParentA)
        val B = toNominal(ParentB)
        val C = toNominal(Unrelated)
        val iAB = Type.intersection(A, B)
        val iAC = Type.intersection(A, C)

        val rel = hierarchy.relationshipFor(iAB, iAC)
        rel shouldBe a[TypeRelationship.HaveCommonSupertype[_]]
        rel.commonSupertype shouldBe Some(A)
      }

      it("should find A as LUB of (A & B) and A") {
        val A = toNominal(ParentA)
        val B = toNominal(ParentB)
        val iAB = Type.intersection(A, B)

        hierarchy.relationshipFor(iAB, A) shouldBe TypeRelationship.StrictSubtype
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
        val expected = Type.tuple(toNominal(ParentA), toNominal(ParentB))
        hierarchy.relationshipFor(t1, t2) shouldBe TypeRelationship.HaveCommonSupertype(expected)
      }
    }

    describe("Structural LUB") {
      it("should compute structural LUB for functions") {
        val f1 = Type.Function(Vector(toNominal(ParentA)), toNominal(ChildA1))
        val f2 = Type.Function(Vector(toNominal(ParentA)), toNominal(ParentB))
        // LUB((ParentA) => ChildA1, (ParentA) => ParentB) = (ParentA) => Root
        val expected = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))

        hierarchy.relationshipFor(f1, f2) shouldBe TypeRelationship.HaveCommonSupertype(expected)
      }

      it("should compute structural LUB for functions with contravariance") {
        val f1 = Type.Function(Vector(toNominal(ChildA1)), toNominal(Root))
        val f2 = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))
        // LUB((ChildA1) => Root, (ParentA) => Root) = (ChildA1) => Root
        // Because (ParentA) => Root is a subtype of (ChildA1) => Root
        hierarchy.relationshipFor(f1, f2) shouldBe TypeRelationship.StrictSupertype
      }

      it("should compute structural LUB for applied types (covariant)") {
        val covariantParam = TypeParameter.covariant[TestType]
        val listC = Type.constructor(toTestName("List"), NonEmptyVector(covariantParam))
        val listChild = Type.applied(listC, TypeArgument(covariantParam, toNominal(ChildA1)))
        val listParentA = Type.applied(listC, TypeArgument(covariantParam, toNominal(ParentA)))
        val listParentB = Type.applied(listC, TypeArgument(covariantParam, toNominal(ParentB)))

        // LUB(List[ChildA1], List[ParentA]) = List[ParentA]
        hierarchy.relationshipFor(listChild, listParentA) shouldBe TypeRelationship.StrictSubtype

        // LUB(List[ParentA], List[ParentB]) = List[Root]
        val expected = Type.applied(listC, TypeArgument(covariantParam, toNominal(Root)))
        hierarchy.relationshipFor(listParentA, listParentB) shouldBe TypeRelationship.HaveCommonSupertype(expected)
      }

      it("should compute LUB for applied types with different constructors but common parent") {
        val covariantParam = TypeParameter.covariant[TestType]
        val collectionC = Type.constructor(toTestName("Collection"), NonEmptyVector(covariantParam))
        val listC = Type.constructor(toTestName("List"), NonEmptyVector(covariantParam))
        val vectorC = Type.constructor(toTestName("Vector"), NonEmptyVector(covariantParam))

        val h = AdjacencyTypeHierarchy.fromMap[TestType](
          hierarchyMap ++ Map(
            listC -> Set(collectionC),
            vectorC -> Set(collectionC)
          ),
          Set.empty
        )

        val listChild = Type.applied(listC, TypeArgument(covariantParam, toNominal(ChildA1)))
        val vectorChild = Type.applied(vectorC, TypeArgument(covariantParam, toNominal(ChildA1)))

        val expected = Type.applied(collectionC, TypeArgument(covariantParam, toNominal(ChildA1)))
        h.relationshipFor(listChild, vectorChild) shouldBe TypeRelationship.HaveCommonSupertype(expected)
      }

      it("should compute LUB for applied types with different constructors and different arguments (covariant)") {
        val covariantParam = TypeParameter.covariant[TestType]
        val collectionC = Type.constructor(toTestName("Collection"), NonEmptyVector(covariantParam))
        val listC = Type.constructor(toTestName("List"), NonEmptyVector(covariantParam))
        val vectorC = Type.constructor(toTestName("Vector"), NonEmptyVector(covariantParam))

        val h = AdjacencyTypeHierarchy.fromMap[TestType](
          hierarchyMap ++ Map(
            listC -> Set(collectionC),
            vectorC -> Set(collectionC)
          ),
          Set.empty
        )

        val listParentA = Type.applied(listC, TypeArgument(covariantParam, toNominal(ParentA)))
        val vectorParentB = Type.applied(vectorC, TypeArgument(covariantParam, toNominal(ParentB)))

        // LUB(List[ParentA], Vector[ParentB]) -> Collection[LUB(ParentA, ParentB)] -> Collection[Root]
        val expected = Type.applied(collectionC, TypeArgument(covariantParam, toNominal(Root)))
        h.relationshipFor(listParentA, vectorParentB) shouldBe TypeRelationship.HaveCommonSupertype(expected)
      }
    }

    describe("empty hierarchy") {
      val h = AdjacencyTypeHierarchy.empty[TestType]

      it("should not define nominal inheritance relationships") {
        h.isSubtype(toNominal(ParentA), toNominal(Root)) shouldBe false
        h.isSubtype(toNominal(Root), toNominal(ParentA)) shouldBe false
        h.isSubtype(toNominal(ParentA), toNominal(Unrelated)) shouldBe false
      }

      it("should treat nominal types as reference types by default") {
        h.isSubtype(toNominal(ParentA), Type.TopRef) shouldBe true
        h.isSubtype(toNominal(ParentA), Type.Top) shouldBe true
        h.isSubtype(toNominal(ParentA), Type.TopValue) shouldBe false

        h.isSubtype(toNominal(Unrelated), Type.TopRef) shouldBe true
        h.isSubtype(toNominal(Unrelated), Type.TopValue) shouldBe false
      }

      it("should report TopRef as the common supertype for unrelated nominal types") {
        h.relationshipFor(toNominal(ParentA), toNominal(Unrelated)) shouldBe
          TypeRelationship.HaveCommonSupertype(Type.TopRef)
      }

      it("should preserve Same relationships") {
        h.relationshipFor(toNominal(ParentA), toNominal(ParentA)) shouldBe TypeRelationship.Same
      }

      it("should treat structural reference-like types as subtypes of TopRef") {
        val f = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))
        h.isSubtype(f, Type.TopRef) shouldBe true
        h.isSubtype(f, Type.Top) shouldBe true
        h.isSubtype(f, Type.TopValue) shouldBe false

        val tuple = Type.tuple(toNominal(ParentA), toNominal(Root))
        h.isSubtype(tuple, Type.TopRef) shouldBe true
        h.isSubtype(tuple, Type.Top) shouldBe true
        h.isSubtype(tuple, Type.TopValue) shouldBe false

        val param = TypeParameter.invariant[TestType]
        val c = Type.constructor(toTestName("C"), NonEmptyVector(param))
        h.isSubtype(c, Type.TopRef) shouldBe true

        val applied = Type.applied(c, TypeArgument(param, toNominal(ParentA)))
        h.isSubtype(applied, Type.TopRef) shouldBe true
      }

      it("should return default immediate supertypes") {
        h.immediateSupertypes(toNominal(ParentA)) should contain theSameElementsAs Set(Type.TopRef)
        h.immediateSupertypes(Type.TopRef) should contain theSameElementsAs Set(Type.Top)
        h.immediateSupertypes(Type.TopValue) should contain theSameElementsAs Set(Type.Top)
        h.immediateSupertypes(Type.Top) shouldBe empty

        val f = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))
        h.immediateSupertypes(f) should contain theSameElementsAs Set(Type.TopRef)
      }
    }

    describe("immediateSupertypes for non-nominal types") {
      it("should return correct supertypes for top types") {
        hierarchy.immediateSupertypes(Type.Top) shouldBe empty
        hierarchy.immediateSupertypes(Type.TopValue) should contain theSameElementsAs Set(Type.Top)
        hierarchy.immediateSupertypes(Type.TopRef) should contain theSameElementsAs Set(Type.Top)
      }

      it("should return TopRef for constructors") {
        val param = TypeParameter.invariant[TestType]
        val constructor = Type.constructor(toTestName("Box"), NonEmptyVector(param))

        hierarchy.immediateSupertypes(constructor) should contain theSameElementsAs Set(Type.TopRef)
      }

      it("should return TopRef for function types") {
        val f = Type.Function(Vector(toNominal(ParentA)), toNominal(Root))

        hierarchy.immediateSupertypes(f) should contain theSameElementsAs Set(Type.TopRef)
      }

      it("should return TopRef for tuple types") {
        val tuple = Type.tuple(toNominal(ParentA), toNominal(ParentB))

        hierarchy.immediateSupertypes(tuple) should contain theSameElementsAs Set(Type.TopRef)
      }

      it("should return constructor and TopRef for applied types without constructor supertypes") {
        val param = TypeParameter.covariant[TestType]
        val listC = Type.constructor(toTestName("List"), NonEmptyVector(param))
        val listA = Type.applied(listC, TypeArgument(param, toNominal(ParentA)))

        hierarchy.immediateSupertypes(listA) should contain theSameElementsAs Set(
          listC,
          Type.TopRef
        )
      }

      it("should lift constructor supertypes for applied types") {
        val param = TypeParameter.covariant[TestType]
        val collectionC = Type.constructor(toTestName("Collection"), NonEmptyVector(param))
        val listC = Type.constructor(toTestName("List"), NonEmptyVector(param))

        val h = AdjacencyTypeHierarchy.fromMap[TestType](
          hierarchyMap + (listC -> Set(collectionC)),
          Set.empty
        )

        val listA = Type.applied(listC, TypeArgument(param, toNominal(ParentA)))
        val collectionA = Type.applied(collectionC, TypeArgument(param, toNominal(ParentA)))

        h.immediateSupertypes(listA) should contain theSameElementsAs Set(
          collectionA,
          listC,
          Type.TopRef
        )
      }

      it("should include direct applied-type supertypes") {
        val param = TypeParameter.invariant[TestType]
        val boxC = Type.constructor(toTestName("Box"), NonEmptyVector(param))
        val boxA = Type.applied(boxC, TypeArgument(param, toNominal(ParentA)))
        val specialParent = toNominal(Other("SpecialParent"))

        val h = AdjacencyTypeHierarchy.fromMap[TestType](
          hierarchyMap + (boxA -> Set(specialParent)),
          Set.empty
        )

        h.immediateSupertypes(boxA) should contain theSameElementsAs Set(
          boxC,
          Type.TopRef,
          specialParent
        )
      }

      it("should derive union supertypes from component supertypes") {
        val union = Type.union(toNominal(ChildA1), toNominal(ChildB1))

        hierarchy.immediateSupertypes(union) should contain theSameElementsAs Set(
          toNominal(ParentA),
          toNominal(ParentB)
        )
      }

      it("should include components and component supertypes for intersections") {
        val intersection = Type.intersection(toNominal(ChildA1), toNominal(ChildB1))

        hierarchy.immediateSupertypes(intersection) should contain theSameElementsAs Set(
          toNominal(ChildA1),
          toNominal(ChildB1),
          toNominal(ParentA),
          toNominal(ParentB)
        )
      }
    }

    describe("Value and reference lattice") {
      val valueA = toNominal(ValueA)
      val valueB = toNominal(ValueB)
      val refA = toNominal(RefA)
      val refB = toNominal(RefB)

      val valueHierarchy = AdjacencyTypeHierarchy.fromMap[TestType](
        Map(
          valueB -> Set(valueA),
          refB -> Set(refA)
        ),
        Set(valueA, valueB)
      )

      it("should treat nominal value types correctly") {
        valueHierarchy.isSubtype(valueA, Type.TopValue) shouldBe true
        valueHierarchy.isSubtype(valueA, Type.Top) shouldBe true
        valueHierarchy.isSubtype(valueA, Type.TopRef) shouldBe false
      }

      it("should treat nominal reference types correctly") {
        valueHierarchy.isSubtype(refA, Type.TopRef) shouldBe true
        valueHierarchy.isSubtype(refA, Type.Top) shouldBe true
        valueHierarchy.isSubtype(refA, Type.TopValue) shouldBe false
      }

      it("should treat Bottom as the universal bottom type") {
        valueHierarchy.isSubtype(Type.Bottom, valueA) shouldBe true
        valueHierarchy.isSubtype(Type.Bottom, refA) shouldBe true
        valueHierarchy.isSubtype(Type.Bottom, Type.TopValue) shouldBe true
        valueHierarchy.isSubtype(Type.Bottom, Type.TopRef) shouldBe true
        valueHierarchy.isSubtype(Type.Bottom, Type.Top) shouldBe true
      }

      it("should treat BottomRef as only bottom for the reference side") {
        valueHierarchy.isSubtype(Type.BottomRef, refA) shouldBe true
        valueHierarchy.isSubtype(Type.BottomRef, Type.TopRef) shouldBe true
        valueHierarchy.isSubtype(Type.BottomRef, Type.Top) shouldBe true
        valueHierarchy.isSubtype(Type.BottomRef, valueA) shouldBe false
        valueHierarchy.isSubtype(Type.BottomRef, Type.TopValue) shouldBe false
        valueHierarchy.isSubtype(Type.BottomRef, Type.Bottom) shouldBe false
      }

      it("should treat Unit as a value type") {
        valueHierarchy.isSubtype(Type.Unit, Type.TopValue) shouldBe true
        valueHierarchy.isSubtype(Type.Unit, Type.Top) shouldBe true
        valueHierarchy.isSubtype(Type.Unit, Type.TopRef) shouldBe false
      }

      it("should verify branch tops relationships") {
        valueHierarchy.isSubtype(Type.TopValue, Type.Top) shouldBe true
        valueHierarchy.isSubtype(Type.TopRef, Type.Top) shouldBe true
        valueHierarchy.isSubtype(Type.TopValue, Type.TopRef) shouldBe false
        valueHierarchy.isSubtype(Type.TopRef, Type.TopValue) shouldBe false
      }

      it("should return correct immediateSupertypes for value and reference types") {
        // valueA has no explicit mapping, so it should extend TopValue
        valueHierarchy.immediateSupertypes(valueA) should contain theSameElementsAs Set(Type.TopValue)
        // refA has no explicit mapping, so it should extend TopRef
        valueHierarchy.immediateSupertypes(refA) should contain theSameElementsAs Set(Type.TopRef)
        // valueB extends valueA
        valueHierarchy.immediateSupertypes(valueB) should contain theSameElementsAs Set(valueA)
        // refB extends refA
        valueHierarchy.immediateSupertypes(refB) should contain theSameElementsAs Set(refA)

        valueHierarchy.immediateSupertypes(Type.TopValue) should contain theSameElementsAs Set(Type.Top)
        valueHierarchy.immediateSupertypes(Type.TopRef) should contain theSameElementsAs Set(Type.Top)
      }

      it("should report correct relationships via relationshipFor") {
        valueHierarchy.relationshipFor(valueA, Type.TopValue) shouldBe TypeRelationship.StrictSubtype
        valueHierarchy.relationshipFor(refA, Type.TopRef) shouldBe TypeRelationship.StrictSubtype
        valueHierarchy.relationshipFor(Type.TopValue, Type.Top) shouldBe TypeRelationship.StrictSubtype
        valueHierarchy.relationshipFor(Type.TopRef, Type.Top) shouldBe TypeRelationship.StrictSubtype

        // Relationship between value and reference type
        val relValRef = valueHierarchy.relationshipFor(valueA, refA)
        relValRef shouldBe a[TypeRelationship.HaveCommonSupertype[_]]
        relValRef.commonSupertype shouldBe Some(Type.Top)

        // Relationship between branch tops
        val relTops = valueHierarchy.relationshipFor(Type.TopValue, Type.TopRef)
        relTops shouldBe a[TypeRelationship.HaveCommonSupertype[_]]
        relTops.commonSupertype shouldBe Some(Type.Top)
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

  case object ValueA extends TestType

  case object ValueB extends TestType

  case object RefA extends TestType

  case object RefB extends TestType

  case object Unrelated extends TestType

  case class Other(name: String) extends TestType

  private def toTestName(s: String): TestType = Other(s)

  private def toNominal(t: TestType): Type.Nominal[TestType] = Type.Nominal(t)

  private lazy val hierarchyMap: Map[Type[TestType], Set[Type[TestType]]] = Map(
    toNominal(DiamondChild) -> Set(toNominal(ChildA1), toNominal(ChildB1)),
    toNominal(ChildA1) -> Set(toNominal(ParentA)),
    toNominal(ChildB1) -> Set(toNominal(ParentB)),
    toNominal(ParentA) -> Set(toNominal(Root)),
    toNominal(ParentB) -> Set(toNominal(Root))
  )

  private lazy val hierarchy = AdjacencyTypeHierarchy.fromMap(hierarchyMap, Set.empty[Type.Nominal[TestType]])
}
