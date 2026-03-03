package software.kes.scaletta.ast

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.types.ConjunctionType

class TypeIdentifierTest extends AnyFunSpec with Matchers {
  private val typeA = TypeIdentifier.name(Identifier("A"))
  private val typeB = TypeIdentifier.name(Identifier("B"))
  private val typeC = TypeIdentifier.name(Identifier("C"))

  describe("TypeIdentifier") {
    describe("name") {
      it("should create a Name node") {
        val ti = TypeIdentifier.name(Identifier("Int"))
        ti shouldBe TypeIdentifier.Name(Identifier("Int"))
      }
    }

    describe("applied") {
      it("should create an Applied node with arguments") {
        val ti = TypeIdentifier.applied(Identifier("List"), typeA)
        ti shouldBe TypeIdentifier.Applied(Identifier("List"), ::(typeA, Nil))
      }

      it("should fall back to Name for zero arguments") {
        val ti = TypeIdentifier.applied(Identifier("Int"))
        ti shouldBe TypeIdentifier.Name(Identifier("Int"))
      }
    }

    describe("union") {
      it("should simplify A | A to A") {
        val ti = TypeIdentifier.union(typeA, typeA)
        ti shouldBe typeA
      }

      it("should be order-insensitive (A | B == B | A)") {
        val ti1 = TypeIdentifier.union(typeA, typeB)
        val ti2 = TypeIdentifier.union(typeB, typeA)
        ti1 shouldBe ti2
      }

      it("should flatten nested unions (A | (B | C) == A | B | C)") {
        val nested = TypeIdentifier.union(typeB, typeC)
        val ti = TypeIdentifier.union(typeA, nested)
        ti should matchPattern {
          case TypeIdentifier.Conjunction(ConjunctionType.Union, components) if components == Set(typeA, typeB, typeC) =>
        }
      }

      it("should NOT flatten different conjunction types (A | (B & C))") {
        val nested = TypeIdentifier.intersection(typeB, typeC)
        val ti = TypeIdentifier.union(typeA, nested)
        ti should matchPattern {
          case TypeIdentifier.Conjunction(ConjunctionType.Union, components) if components == Set(typeA, nested) =>
        }
      }
    }

    describe("intersection") {
      it("should simplify A & A to A") {
        val ti = TypeIdentifier.intersection(typeA, typeA)
        ti shouldBe typeA
      }

      it("should be order-insensitive (A & B == B & A)") {
        val ti1 = TypeIdentifier.intersection(typeA, typeB)
        val ti2 = TypeIdentifier.intersection(typeB, typeA)
        ti1 shouldBe ti2
      }

      it("should flatten nested intersections (A & (B & C) == A & B & C)") {
        val nested = TypeIdentifier.intersection(typeB, typeC)
        val ti = TypeIdentifier.intersection(typeA, nested)
        ti should matchPattern {
          case TypeIdentifier.Conjunction(ConjunctionType.Intersection, components) if components == Set(typeA, typeB, typeC) =>
        }
      }
    }

    describe("function") {
      it("should support zero parameters (() => A)") {
        val ti = TypeIdentifier.function(Vector.empty, typeA)
        ti should matchPattern {
          case TypeIdentifier.Function(params, result) if params.isEmpty && result == typeA =>
        }
      }

      it("should support multiple parameters ((A, B) => C)") {
        val ti = TypeIdentifier.function(Vector(typeA, typeB), typeC)
        ti should matchPattern {
          case TypeIdentifier.Function(params, result) if params == Vector(typeA, typeB) && result == typeC =>
        }
      }

      it("should have consistent equality") {
        val ti1 = TypeIdentifier.function(Vector(typeA), typeB)
        val ti2 = TypeIdentifier.function(Vector(typeA), typeB)
        ti1 shouldBe ti2
      }
    }

    describe("Conjunction equality and hashCode") {
      it("should have consistent equals and hashCode for Unions") {
        val ti1 = TypeIdentifier.union(typeA, typeB, typeC)
        val ti2 = TypeIdentifier.union(typeC, typeB, typeA)
        ti1 shouldBe ti2
        ti1.hashCode() shouldBe ti2.hashCode()
      }

      it("should distinguish between Union and Intersection with same components") {
        val u = TypeIdentifier.union(typeA, typeB)
        val i = TypeIdentifier.intersection(typeA, typeB)
        u shouldNot be(i)
        u.hashCode() shouldNot be(i.hashCode())
      }
    }

    describe("Conjunction extractor") {
      it("should correctly extract components") {
        val ti = TypeIdentifier.union(typeA, typeB)
        ti match {
          case TypeIdentifier.Conjunction(ct, components) =>
            ct shouldBe ConjunctionType.Union
            components shouldBe Set(typeA, typeB)
          case _ => fail("Extractor failed")
        }
      }
    }
  }
}
