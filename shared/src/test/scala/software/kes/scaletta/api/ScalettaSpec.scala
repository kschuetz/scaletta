package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.scanner.ScannerConstants.DoubleQuotes3

final class ScalettaSpec extends AnyFunSpec with Matchers {

  describe("Scaletta.compile") {
    it("should successfully parse and link a simple expression") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 + 2")

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should successfully parse and link valid local bindings in blocks") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val a = 41; val b = 43; a + b }")

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should return errors for invalid syntax") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 +")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
    }

    it("should return errors for extra tokens") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("1 + 2 3")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
    }

    it("should return errors for forward references in blocks") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val x = y; val y = 41; x }")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Forward reference to 'y'")) shouldBe true
    }

    it("should return errors for unknown identifiers") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val x = z; x }")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Unknown identifier 'z'")) shouldBe true
    }

    it("should return errors for duplicate definitions") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("{ val x = 41; val x = 43; x }")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Duplicate definition of 'x'")) shouldBe true
    }

    it("should return errors for unknown type ascriptions") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("41: NonExistentType")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Unknown type 'NonExistentType'")) shouldBe true
    }

    it("should successfully parse, desugar, link, and typecheck simple interpolated strings") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile(
        """{
          |  val name = "world"
          |  val greeting = s"hello $name"
          |  greeting
          |}""".stripMargin
      )

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should successfully compile interpolated strings with complex block escapes") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile(
        """{
          |  val a = 41
          |  val b = 43
          |  val message = s"sum: ${a + b}"
          |  message
          |}""".stripMargin
      )

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should successfully compile multi-line interpolated strings") {
      val scaletta = Scaletta.builder.build
      val source =
        s"""{
           |  val title = "Report"
           |  val content = s${DoubleQuotes3}Title: $$title$DoubleQuotes3
           |  content
           |}""".stripMargin
      val result = scaletta.compile(source)

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    it("should enforce forward reference errors inside interpolated string escapes") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile(
        """{
          |  val a = s"value: $b"
          |  val b = 41
          |  a
          |}""".stripMargin
      )

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Forward reference to 'b'")) shouldBe true
    }

    it("should return errors for unknown identifiers in interpolated strings") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("""s"missing: $unknownIdentifier"""")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Unknown identifier 'unknownIdentifier'")) shouldBe true
    }

    it("should return errors for unknown identifiers inside block escapes in interpolated strings") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile("""s"missing: ${unknownExpr + 41}"""")

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
      result.errors.exists(_.message.contains("Unknown identifier 'unknownExpr'")) shouldBe true
    }

    it("should verify that interpolated strings typecheck as String") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile(
        """{
          |  val message: String = s"value is ${41 + 43}"
          |  message
          |}""".stripMargin
      )

      result.isSuccess shouldBe true
      result.errors shouldBe empty
    }

    // TODO: fix later
    ignore("should return type errors when an interpolated string is ascribed to a non-String type") {
      val scaletta = Scaletta.builder.build
      val result = scaletta.compile(
        """{
          |  val num: Int = s"not an int"
          |  num
          |}""".stripMargin
      )

      result.isFailure shouldBe true
      result.errors shouldNot be(empty)
    }
  }

}
