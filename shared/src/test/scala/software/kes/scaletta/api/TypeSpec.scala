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
        val param = TypeParameter.invariant[String]
        val arg = TypeArgument(param, typeInt)
        val t = Type.applied("Option", arg)
        t shouldBe Type.Applied("Option", NonEmptyVector(arg))
        t.isGround shouldBe true
      }

      it("should handle multiple arguments") {
        val param = TypeParameter.invariant[String]
        val arg1 = TypeArgument(param, typeInt)
        val arg2 = TypeArgument(param, typeString)
        val t = Type.applied("Map", arg1, arg2)
        t shouldBe Type.Applied("Map", NonEmptyVector(arg1, arg2))
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
        t shouldBe a[Type.Intersection[_]]
        t.asInstanceOf[Type.Intersection[String]].types should contain allOf(typeInt, typeString)
        t.isGround shouldBe true
      }

      it("should handle more than two types") {
        val t = Type.intersection(typeInt, typeString, typeBoolean)
        t shouldBe a[Type.Intersection[_]]
        t.asInstanceOf[Type.Intersection[String]].types should contain allOf(typeInt, typeString, typeBoolean)
      }
    }

    describe("union") {
      it("should create a Union type") {
        val t = Type.union(typeInt, typeString)
        t shouldBe a[Type.Union[_]]
        t.asInstanceOf[Type.Union[String]].types should contain allOf(typeInt, typeString)
        t.isGround shouldBe true
      }

      it("should handle more than two types") {
        val t = Type.union(typeInt, typeString, typeBoolean)
        t shouldBe a[Type.Union[_]]
        t.asInstanceOf[Type.Union[String]].types should contain allOf(typeInt, typeString, typeBoolean)
      }
    }

    describe("variable") {
      it("should create a Variable type with scope 0") {
        val t = Type.variable(41)
        t shouldBe Type.Variable(0, 41)
        t.isGround shouldBe false
      }
    }
  }
}
