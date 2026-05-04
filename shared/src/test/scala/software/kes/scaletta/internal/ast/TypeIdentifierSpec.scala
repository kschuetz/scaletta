package software.kes.scaletta.internal.ast

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.types.ConjunctionType
import software.kes.scaletta.util.functional.Id.Id
import software.kes.scaletta.util.functional.{Id, ~>}

class TypeIdentifierSpec extends AnyFunSpec with Matchers {

  import Id.idFunctor

  private def id[A](a: A): Id[A] = a

  private val typeA = TypeIdentifier.name(id(Identifier[Id]("A")))
  private val typeB = TypeIdentifier.name(id(Identifier[Id]("B")))
  private val typeC = TypeIdentifier.name(id(Identifier[Id]("C")))

  describe("TypeIdentifier") {
    describe("name") {
      it("should create a Name node") {
        val ti = TypeIdentifier.name(id(Identifier[Id]("Int")))
        ti shouldBe TypeIdentifier.Name(id(Identifier[Id]("Int")))
      }
    }

    describe("applied") {
      it("should create an Applied node with arguments") {
        val ti = TypeIdentifier.applied(id(TypeIdentifier.name(id(Identifier[Id]("List")))), id(typeA))
        ti shouldBe TypeIdentifier.Applied(id(TypeIdentifier.name(id(Identifier[Id]("List")))), ::(id(typeA), Nil))
      }

      it("should fall back to Name for zero arguments when possible") {
        val nameId = id(Identifier[Id]("Int"))
        val qualifier: TypeIdentifier[Id] = TypeIdentifier.name(nameId)
        val ti = TypeIdentifier.applied[Id](qualifier)
        ti shouldBe TypeIdentifier.Name(nameId)
      }
    }

    describe("union") {
      it("should NOT simplify A | A to A") {
        val ti = TypeIdentifier.union(id(typeA), id(typeA))
        ti shouldBe TypeIdentifier.Conjunction(ConjunctionType.Union, Vector(id(typeA), id(typeA)))
      }

      it("should be order-sensitive (A | B != B | A)") {
        val ti1 = TypeIdentifier.union(id(typeA), id(typeB))
        val ti2 = TypeIdentifier.union(id(typeB), id(typeA))
        ti1 shouldNot be(ti2)
      }

      it("should NOT flatten nested unions (A | (B | C) != A | B | C)") {
        val nested = TypeIdentifier.union(id(typeB), id(typeC))
        val ti = TypeIdentifier.union(id(typeA), id(nested))
        ti shouldBe TypeIdentifier.Conjunction(ConjunctionType.Union, Vector(id(typeA), id(nested)))
      }

      it("should NOT flatten different conjunction types (A | (B & C))") {
        val nested = TypeIdentifier.intersection(id(typeB), id(typeC))
        val ti = TypeIdentifier.union(id(typeA), id(nested))
        ti shouldBe TypeIdentifier.Conjunction(ConjunctionType.Union, Vector(id(typeA), id(nested)))
      }
    }

    describe("intersection") {
      it("should NOT simplify A & A to A") {
        val ti = TypeIdentifier.intersection(id(typeA), id(typeA))
        ti shouldBe TypeIdentifier.Conjunction(ConjunctionType.Intersection, Vector(id(typeA), id(typeA)))
      }

      it("should be order-sensitive (A & B != B & A)") {
        val ti1 = TypeIdentifier.intersection(id(typeA), id(typeB))
        val ti2 = TypeIdentifier.intersection(id(typeB), id(typeA))
        ti1 shouldNot be(ti2)
      }

      it("should NOT flatten nested intersections (A & (B & C) != A & B & C)") {
        val nested = TypeIdentifier.intersection(id(typeB), id(typeC))
        val ti = TypeIdentifier.intersection(id(typeA), id(nested))
        ti shouldBe TypeIdentifier.Conjunction(ConjunctionType.Intersection, Vector(id(typeA), id(nested)))
      }
    }

    describe("function") {
      it("should support zero parameters (() => A)") {
        val ti = TypeIdentifier.function(Vector.empty, id(typeA))
        ti match {
          case f: TypeIdentifier.Function[Id] =>
            f.params shouldBe Vector.empty
            f.result shouldBe id(typeA)
          case _ => fail("Expected a Function node")
        }
      }

      it("should support multiple parameters ((A, B) => C)") {
        val ti = TypeIdentifier.function(Vector(id(typeA), id(typeB)), id(typeC))
        ti match {
          case f: TypeIdentifier.Function[Id] =>
            f.params shouldBe Vector(id(typeA), id(typeB))
            f.result shouldBe id(typeC)
          case _ => fail("Expected a Function node")
        }
      }

      it("should have consistent equality") {
        val ti1 = TypeIdentifier.function(Vector(id(typeA)), id(typeB))
        val ti2 = TypeIdentifier.function(Vector(id(typeA)), id(typeB))
        ti1 shouldBe ti2
      }
    }

    describe("Conjunction equality") {
      it("should have consistent equals for Unions") {
        val ti1 = TypeIdentifier.union(id(typeA), id(typeB), id(typeC))
        val ti2 = TypeIdentifier.union(id(typeA), id(typeB), id(typeC))
        ti1 shouldBe ti2
      }

      it("should distinguish between Union and Intersection with same components") {
        val u = TypeIdentifier.union(id(typeA), id(typeB))
        val i = TypeIdentifier.intersection(id(typeA), id(typeB))
        u shouldNot be(i)
      }
    }

    describe("Conjunction extractor") {
      it("should correctly extract components") {
        val ti = TypeIdentifier.union(id(typeA), id(typeB))
        ti match {
          case TypeIdentifier.Conjunction(ct, components) =>
            ct shouldBe ConjunctionType.Union
            components shouldBe Vector(id(typeA), id(typeB))
          case _ => fail("Extractor failed")
        }
      }
    }

    describe("mapK") {
      val toId = new(Id ~> Id) {
        def apply[A](fa: Id[A]): Id[A] = fa
      }

      it("should transform Name") {
        val ti: TypeIdentifier[Id] = TypeIdentifier.name(id(Identifier[Id]("Int")))
        val mapped = ti.mapK(toId)
        mapped shouldBe ti
      }

      it("should transform Applied") {
        val ti: TypeIdentifier[Id] = TypeIdentifier.applied(id(TypeIdentifier.name(id(Identifier[Id]("List")))), id(typeA))
        val mapped = ti.mapK(toId)
        mapped shouldBe ti
      }

      it("should transform Function") {
        val ti: TypeIdentifier[Id] = TypeIdentifier.function(Vector(id(typeA)), id(typeB))
        val mapped = ti.mapK(toId)
        mapped shouldBe ti
      }

      it("should transform Conjunction") {
        val ti: TypeIdentifier[Id] = TypeIdentifier.union(id(typeA), id(typeB))
        val mapped = ti.mapK(toId)
        mapped shouldBe ti
      }
    }
  }
}
