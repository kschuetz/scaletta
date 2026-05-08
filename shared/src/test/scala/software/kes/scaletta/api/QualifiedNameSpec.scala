package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class QualifiedNameSpec extends AnyFunSpec with Matchers {
  describe("QualifiedName") {
    describe("tryParseFull") {
      it("should parse a name in the root package (no dot)") {
        val result = QualifiedName.tryParseFull("scaletta")
        result shouldBe Right(QualifiedName.Full(PackagePath.root, Name("scaletta")))
      }

      it("should parse a name with _root_ prefix") {
        val result = QualifiedName.tryParseFull("_root_.scaletta")
        result shouldBe Right(QualifiedName.Full(PackagePath.root, Name("scaletta")))
      }

      it("should parse a multi-level qualified name") {
        val result = QualifiedName.tryParseFull("org.example.User")
        val expectedPkg = PackagePath.parseAbsolute("org.example")
        result shouldBe Right(QualifiedName.Full(expectedPkg, Name("User")))
      }

      it("should fail on empty string") {
        QualifiedName.tryParseFull("") shouldBe a[Left[_, _]]
      }

      it("should fail on trailing dot") {
        QualifiedName.tryParseFull("pkg.") shouldBe a[Left[_, _]]
      }

      it("should fail on invalid identifier") {
        QualifiedName.tryParseFull("pkg.123") shouldBe a[Left[_, _]]
      }
    }

    describe("tryParsePartial") {
      it("should parse a local name (no dot)") {
        val result = QualifiedName.tryParsePartial("x")
        result shouldBe Right(QualifiedName.Partial(None, Name("x")))
      }

      it("should parse a relative qualified name") {
        val result = QualifiedName.tryParsePartial("models.User")
        val expectedPkg = PackagePath.parseRelative("models")
        result shouldBe Right(QualifiedName.Partial(Some(expectedPkg), Name("User")))
      }

      it("should parse an absolute qualified name") {
        val result = QualifiedName.tryParsePartial("_root_.pkg.User")
        val expectedPkg = PackagePath.parseAbsolute("pkg")
        result shouldBe Right(QualifiedName.Partial(Some(expectedPkg), Name("User")))
      }

      it("should fail on empty string") {
        QualifiedName.tryParsePartial("") shouldBe a[Left[_, _]]
      }
    }
  }
}
