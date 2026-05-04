package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Type
import software.kes.scaletta.util.NonEmptyVector

class TypeConstructorSpec extends AnyFunSpec with Matchers {
  private val typeName = "Option"
  private val param = TypeParameter.invariant[String]
  private val tc = TypeConstructor.create(typeName, NonEmptyVector(param))

  describe("TypeConstructor") {
    describe("creation and basic properties") {
      it("should correctly report arity") {
        tc.arity shouldBe 1

        val tc2 = TypeConstructor.create("Map", NonEmptyVector(param, param))
        tc2.arity shouldBe 2
      }

      it("should correctly handle equality and hashCode") {
        val tc1a = TypeConstructor.create("Option", NonEmptyVector(param))
        val tc1b = TypeConstructor.create("Option", NonEmptyVector(param))
        val tc2 = TypeConstructor.create("List", NonEmptyVector(param))

        tc1a shouldBe tc1b
        tc1a.hashCode() shouldBe tc1b.hashCode()
        tc1a shouldNot be(tc2)
        tc1a shouldNot be("something else")
      }

      it("should have a descriptive toString") {
        tc.toString shouldBe "TypeConstructor(Option, 1)"
      }
    }

    describe("applyAll") {
      it("should return Type.Applied when given the correct number of args") {
        val arg = Type.Nominal("Int")
        val result = tc.applyAll(arg)
        result.constructorName shouldBe typeName
        result.arguments.head.value shouldBe arg
      }

      it("should ignore extra arguments") {
        val arg1 = Type.Nominal("Int")
        val arg2 = Type.Nominal("String")
        val result = tc.applyAll(arg1, arg2)
        result.arguments.length shouldBe 1
        result.arguments.head.value shouldBe arg1
      }

      it("should throw IllegalArgumentException when too few arguments are provided") {
        val tc2 = TypeConstructor.create("Map", NonEmptyVector(param, param))
        val exception = intercept[IllegalArgumentException] {
          tc2.applyAll(Type.Nominal("Int"))
        }
        exception.getMessage should include("Not enough arguments to construct type (1 more needed)")
      }
    }

    describe("applyAllFromSeq") {
      it("should work correctly with a sequence of arguments") {
        val args = Seq(Type.Nominal("Int"))
        val result = tc.applyAllFromSeq(args)
        result.constructorName shouldBe typeName
        result.arguments.head.value shouldBe args.head
      }
    }

    describe("applyArgs") {
      it("should return Left(TypeConstructor) when partially applied") {
        val tc2 = TypeConstructor.create("Map", NonEmptyVector(param, param))
        val result = tc2.applyArgs(Type.Nominal("String"))

        result shouldBe a[Left[_, _]]
        val partialTc = result.left.getOrElse(fail("Expected Left"))
        partialTc.arity shouldBe 1
        partialTc.name shouldBe "Map"
      }

      it("should return Right(Type.Applied) when fully applied") {
        val result = tc.applyArgs(Type.Nominal("String"))

        result shouldBe a[Right[_, _]]
        val applied = result.toOption.getOrElse(fail("Expected Right"))
        applied.constructorName shouldBe typeName
        applied.arguments.head.value shouldBe Type.Nominal("String")
      }

      it("should return Left(original) when given zero arguments") {
        val result = tc.applyArgs()

        result shouldBe a[Left[_, _]]
        val partialTc = result.left.getOrElse(fail("Expected Left"))
        partialTc shouldBe tc
      }

      it("should handle chained applications") {
        val tc2 = TypeConstructor.create("Map", NonEmptyVector(param, param))
        val result1 = tc2.applyArgs(Type.Nominal("Int"))

        result1 shouldBe a[Left[_, _]]
        val partialTc = result1.left.getOrElse(fail("Expected Left"))

        val result2 = partialTc.applyArgs(Type.Nominal("String"))
        result2 shouldBe a[Right[_, _]]
        val applied = result2.toOption.getOrElse(fail("Expected Right"))
        applied.arguments.map(_.value) shouldBe NonEmptyVector(Type.Nominal("Int"), Type.Nominal("String"))
      }
    }

    describe("grounding") {
      it("should result in a ground type when ground types are applied") {
        val result = tc.applyAll(Type.Nominal("Int"))
        result.isGround shouldBe true
      }

      it("should result in a non-ground type when a Type.Variable is applied") {
        val result = tc.applyAll(Type.Variable(41, 43))
        result.isGround shouldBe false
      }
    }
  }
}
