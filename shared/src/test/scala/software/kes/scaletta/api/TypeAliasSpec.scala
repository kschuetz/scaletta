package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.util.NonEmptyVector

class TypeAliasSpec extends AnyFunSpec with Matchers {
  private val typeInt = Type.nominal("Int")
  private val typeString = Type.nominal("String")
  private val typeError = Type.nominal("Error")

  describe("Type Aliases") {
    it("should support simple non-generic aliases") {
      // type MyInt = Int
      val myIntAlias = typeInt

      // In a real scenario, the name "MyInt" would resolve to this type structure
      myIntAlias shouldBe typeInt
    }

    it("should support generic aliases via substitution") {
      // type Result[T] = Either[Error, T]

      val eitherParams = NonEmptyVector(TypeParameter.invariant[String], TypeParameter.invariant[String])
      val eitherConstructor = Type.constructor("Either", eitherParams)

      // The RHS of the alias: Either[Error, Variable(0)]
      val rhs = Type.applied(
        eitherConstructor,
        TypeArgument(eitherParams(0), typeError),
        TypeArgument(eitherParams(1), Type.variable(0))
      )

      // Now use TypeApplier to "instantiate" Result[Int]
      // The alias "Result" has one parameter T
      val resultParams = NonEmptyVector(TypeParameter.invariant[String])
      val applier = TypeApplier.createFromType(rhs, resultParams)

      val resultInt = applier.applyAll(typeInt)

      // resultInt should be Either[Error, Int]
      resultInt shouldBe a[Type.Applied[_]]
      val applied = resultInt.asInstanceOf[Type.Applied[String]]
      applied.constructor shouldBe eitherConstructor
      applied.arguments(0).value shouldBe typeError
      applied.arguments(1).value shouldBe typeInt
    }

    it("should support nested generic aliases") {
      // type List[T] = ...
      // type Lists[T] = List[List[T]]

      val listParams = NonEmptyVector(TypeParameter.invariant[String])
      val listConstructor = Type.constructor("List", listParams)

      // RHS of Lists[T]: List[List[Variable(0)]]
      val innerList = Type.applied(listConstructor, TypeArgument(listParams(0), Type.variable(0)))
      val rhsLists = Type.applied(listConstructor, TypeArgument(listParams(0), innerList))

      val listsApplier = TypeApplier.createFromType(rhsLists, listParams)

      val listsInt = listsApplier.applyAll(typeInt)

      // listsInt should be List[List[Int]]
      listsInt shouldBe a[Type.Applied[_]]
      val outerApplied = listsInt.asInstanceOf[Type.Applied[String]]
      outerApplied.constructor shouldBe listConstructor

      val innerApplied = outerApplied.arguments(0).value.asInstanceOf[Type.Applied[String]]
      innerApplied.constructor shouldBe listConstructor
      innerApplied.arguments(0).value shouldBe typeInt
    }

    it("should support multiple parameters in generic aliases") {
      // type MyMap[K] = Map[K, String]

      val mapParams = NonEmptyVector(TypeParameter.invariant[String], TypeParameter.invariant[String])
      val mapConstructor = Type.constructor("Map", mapParams)

      // RHS: Map[Variable(0), String]
      val rhs = Type.applied(
        mapConstructor,
        TypeArgument(mapParams(0), Type.variable(0)),
        TypeArgument(mapParams(1), typeString)
      )

      val myMapApplier = TypeApplier.createFromType(rhs, NonEmptyVector(TypeParameter.invariant[String]))
      val myMapInt = myMapApplier.applyAll(typeInt)

      myMapInt shouldBe a[Type.Applied[_]]
      val applied = myMapInt.asInstanceOf[Type.Applied[String]]
      applied.constructor shouldBe mapConstructor
      applied.arguments(0).value shouldBe typeInt
      applied.arguments(1).value shouldBe typeString
    }

    it("should be transparent (no Alias node in AST)") {
      // type MyInt = Int
      val myInt = typeInt
      myInt shouldBe typeInt
      myInt shouldBe a[Type.Nominal[_]]
    }
  }
}
