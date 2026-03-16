package software.kes.scaletta.common

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PackagePathSpec extends AnyFunSpec with Matchers {

  private val nameOrg = PackageSegment("org")
  private val nameExample = PackageSegment("example")
  private val nameApp = PackageSegment("app")

  describe("PackagePath") {

    describe("Absolute") {
      it("should represent the root") {
        val root = PackagePath.root
        root.isAbsolute shouldBe true
        root.components shouldBe Vector.empty
        root.fullName shouldBe "_root_"
      }

      it("should represent a nested path") {
        val path = PackagePath.absolute(nameOrg, nameExample)
        path.fullName shouldBe "org.example"
        path.components shouldBe Vector(nameOrg, nameExample)
      }

      it("should concatenate with a Relative path") {
        val abs = PackagePath.absolute(nameOrg)
        val rel = PackagePath.relative(nameExample, nameApp)
        val result = abs ++ rel
        result shouldBe PackagePath.absolute(nameOrg, nameExample, nameApp)
        result.isAbsolute shouldBe true
      }

      it("should append a component using /") {
        val abs = PackagePath.absolute(nameOrg)
        val result = abs / nameExample
        result shouldBe PackagePath.absolute(nameOrg, nameExample)
        result.isAbsolute shouldBe true
      }
    }

    describe("Relative") {
      it("should represent an empty relative path") {
        val rel = PackagePath.relative()
        rel.isRelative shouldBe true
        rel.components shouldBe Vector.empty
        rel.fullName shouldBe ""
      }

      it("should represent a nested relative path") {
        val rel = PackagePath.relative(nameOrg, nameExample)
        rel.fullName shouldBe "org.example"
        rel.components shouldBe Vector(nameOrg, nameExample)
      }

      it("should concatenate with another Relative path") {
        val rel1 = PackagePath.relative(nameOrg)
        val rel2 = PackagePath.relative(nameExample)
        val result = rel1 ++ rel2
        result shouldBe PackagePath.relative(nameOrg, nameExample)
        result.isRelative shouldBe true
      }

      it("should append a component using /") {
        val rel = PackagePath.relative(nameOrg)
        val result = rel / nameExample
        result shouldBe PackagePath.relative(nameOrg, nameExample)
        result.isRelative shouldBe true
      }
    }

    describe("parse") {
      it("should parse _root_") {
        PackagePath.parse("_root_") shouldBe Right(PackagePath.root)
      }

      it("should parse _root_.org.example") {
        PackagePath.parse("_root_.org.example") shouldBe Right(PackagePath.absolute(nameOrg, nameExample))
      }

      it("should parse org.example (relative)") {
        PackagePath.parse("org.example") shouldBe Right(PackagePath.relative(nameOrg, nameExample))
      }

      it("should parse an empty string as an empty relative path") {
        PackagePath.parse("") shouldBe Right(PackagePath.relative())
      }

      it("should reject _root_. with no components") {
        PackagePath.parse("_root_.") shouldBe a[Left[_, _]]
      }

      it("should reject invalid components") {
        PackagePath.parse("org.1example") shouldBe a[Left[_, _]]
        PackagePath.parse("_root_.org.if") shouldBe a[Left[_, _]]
      }

      it("should collect multiple errors") {
        val result = PackagePath.parse("1org.if")
        result match {
          case Left(err) =>
            err should include("Invalid package name component: '1org'")
            err should include("'if' is a reserved keyword")
          case _ => fail("Should have failed")
        }
      }
    }

    describe("parseAbsolute") {
      it("should parse _root_") {
        PackagePath.parseAbsolute("_root_") shouldBe Right(PackagePath.root)
      }

      it("should parse _root_.org.example") {
        PackagePath.parseAbsolute("_root_.org.example") shouldBe Right(PackagePath.absolute(nameOrg, nameExample))
      }

      it("should parse org.example without _root_ prefix") {
        PackagePath.parseAbsolute("org.example") shouldBe Right(PackagePath.absolute(nameOrg, nameExample))
      }

      it("should reject empty strings") {
        PackagePath.parseAbsolute("") shouldBe a[Left[_, _]]
      }
    }

    describe("parseRelative") {
      it("should parse org.example") {
        PackagePath.parseRelative("org.example") shouldBe Right(PackagePath.relative(nameOrg, nameExample))
      }

      it("should parse an empty string") {
        PackagePath.parseRelative("") shouldBe Right(PackagePath.relative())
      }

      it("should reject paths starting with _root_") {
        PackagePath.parseRelative("_root_") shouldBe a[Left[_, _]]
        PackagePath.parseRelative("_root_.org") shouldBe a[Left[_, _]]
      }
    }
  }
}
