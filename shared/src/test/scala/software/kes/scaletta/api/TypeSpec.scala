package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.util.NonEmptyVector

class TypeSpec extends AnyFunSpec with Matchers {
  private val typeInt = Type.nominal("Int")
  private val typeString = Type.nominal("String")
  private val typeBoolean = Type.nominal("Boolean")

  describe("Type") {
    describe("nominal") {
      it("should create a Nominal type") {
        val t = Type.nominal("Int")
        t shouldBe Type.Nominal("Int")
        t.isGround shouldBe true
      }
    }

    describe("applied") {
      it("should create an Applied type") {
        val params = NonEmptyVector(TypeParameter.invariant[String])
        val c = Type.constructor("Option", params)
        val arg = TypeArgument(params.head, typeInt)
        val t = Type.applied(c, arg)
        t shouldBe Type.Applied(c, NonEmptyVector(arg))
        t.isGround shouldBe true
      }

      it("should handle multiple arguments") {
        val params = NonEmptyVector(TypeParameter.invariant[String], TypeParameter.invariant[String])
        val c = Type.constructor("Map", params)
        val arg1 = TypeArgument(params.head, typeInt)
        val arg2 = TypeArgument(params.last, typeString)
        val t = Type.applied(c, arg1, arg2)
        t shouldBe Type.Applied(c, NonEmptyVector(arg1, arg2))
        t.isGround shouldBe true
      }
    }

    describe("function") {
      it("should create a Function type") {
        val t = Type.function(typeInt, typeString)(typeBoolean)
        t shouldBe Type.Function(Vector(typeInt, typeString), typeBoolean)
        t.isGround shouldBe true
      }

      it("should handle zero parameters") {
        val t = Type.function()(typeInt)
        t shouldBe Type.Function(Vector.empty, typeInt)
        t.isGround shouldBe true
      }
    }

    describe("intersection") {
      it("should create an Intersection type") {
        val t = Type.intersection(typeInt, typeString)
        t match {
          case i: Type.Intersection[_] =>
            i.types should contain allOf(typeInt, typeString)
          case _ => fail("Expected an Intersection type")
        }
        t.isGround shouldBe true
      }

      it("should handle more than two types") {
        val t = Type.intersection(typeInt, typeString, typeBoolean)
        t match {
          case i: Type.Intersection[_] =>
            i.types should contain allOf(typeInt, typeString, typeBoolean)
          case _ => fail("Expected an Intersection type")
        }
      }
    }

    describe("union") {
      it("should create a Union type") {
        val t = Type.union(typeInt, typeString)
        t match {
          case u: Type.Union[_] =>
            u.types should contain allOf(typeInt, typeString)
          case _ => fail("Expected a Union type")
        }
        t.isGround shouldBe true
      }

      it("should handle more than two types") {
        val t = Type.union(typeInt, typeString, typeBoolean)
        t match {
          case u: Type.Union[_] =>
            u.types should contain allOf(typeInt, typeString, typeBoolean)
          case _ => fail("Expected a Union type")
        }
      }
    }

    describe("variable") {
      it("should create a Variable type with scope 0") {
        val t = Type.variable(41)
        t shouldBe Type.Variable(0, 41)
        t.isGround shouldBe false
      }
    }

    describe("tuple") {
      it("should create a Tuple type") {
        val t = Type.tuple(typeInt, typeString)
        t match {
          case tu: Type.Tuple[_] =>
            tu.elements.toVector shouldBe Vector(typeInt, typeString)
          case _ => fail("Expected a Tuple type")
        }
        t.isGround shouldBe true
      }

      it("should handle more than two elements") {
        val t = Type.tuple(typeInt, typeString, typeBoolean)
        t match {
          case tu: Type.Tuple[_] =>
            tu.elements.toVector shouldBe Vector(typeInt, typeString, typeBoolean)
          case _ => fail("Expected a Tuple type")
        }
      }

      it("should be ground only if all elements are ground") {
        val groundTuple = Type.tuple(typeInt, typeString)
        groundTuple.isGround shouldBe true

        val nonGroundTuple = Type.tuple(typeInt, Type.variable(43))
        nonGroundTuple.isGround shouldBe false
      }
    }

    describe("ProperType and TypeConstructor") {
      it("should identify Nominal types as ProperType") {
        typeInt shouldBe a[ProperType[_]]
      }

      it("should identify Applied types as ProperType") {
        val params = NonEmptyVector(TypeParameter.invariant[String])
        val c = Type.constructor("Option", params)
        val arg = TypeArgument(params.head, typeInt)
        val t = Type.applied(c, arg)
        t shouldBe a[ProperType[_]]
      }

      it("should identify Function types as ProperType") {
        val t = Type.function(typeInt)(typeString)
        t shouldBe a[ProperType[_]]
      }

      it("should identify Union and Intersection types as ProperType") {
        val u = Type.union(typeInt, typeString)
        val i = Type.intersection(typeInt, typeString)
        u shouldBe a[ProperType[_]]
        i shouldBe a[ProperType[_]]
      }

      it("should identify Constructor types as TypeConstructor") {
        val params = NonEmptyVector(TypeParameter.invariant[String])
        val c = Type.constructor("Option", params)
        c shouldBe a[TypeConstructor[_]]
        c should not be a[ProperType[_]]
      }

      it("should NOT allow TypeConstructor in Union or Intersection") {
        val params = NonEmptyVector(TypeParameter.invariant[String])
        val c = Type.constructor("Option", params)

        // The following lines would fail to compile if uncommented:
        // Type.union(typeInt, c)
        // Type.intersection(typeInt, c)

        // We can check the type signature of the factory methods
        "Type.union(typeInt, typeString)" should compile
        "val params = software.kes.scaletta.util.NonEmptyVector(software.kes.scaletta.api.TypeParameter.invariant[String]); val c = Type.constructor(\"Option\", params); Type.union(Type.nominal(\"Int\"), c)" shouldNot compile
      }
    }
  }
}
