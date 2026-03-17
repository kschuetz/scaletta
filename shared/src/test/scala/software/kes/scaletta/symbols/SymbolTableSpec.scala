package software.kes.scaletta.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.PackagePath

class SymbolTableSpec extends AnyFunSpec with Matchers {

  describe("SymbolTable") {
    it("should resolve a global symbol in the root package") {
      val table = SymbolTable.empty[Int].add(PackagePath.root, "x", 41)
      val result = table.resolve(None, "x", ImportScope.empty)
      result should have size 1
      result.head.name shouldBe "x"
      result.head.container shouldBe Some(PackagePath.root)
      result.head.value shouldBe 41
    }

    it("should resolve a global symbol in a nested package (absolute qualifier)") {
      val pkg = PackagePath.parseAbsolute("org.example").toOption.get
      val table = SymbolTable.empty[Int].add(pkg, "y", 43)
      val result = table.resolve(Some(pkg), "y", ImportScope.empty)
      result should have size 1
      result.head.name shouldBe "y"
      result.head.container shouldBe Some(pkg)
      result.head.value shouldBe 43
    }

    it("should resolve a local symbol (unqualified)") {
      val table = SymbolTable.empty[Int].enterScope.addLocal("z", 45)
      val result = table.resolve(None, "z", ImportScope.empty)
      result should have size 1
      result.head.name shouldBe "z"
      result.head.container shouldBe None
      result.head.value shouldBe 45
    }

    it("should shadow a global symbol with a local one") {
      val table = SymbolTable.empty[Int]
        .add(PackagePath.root, "x", 41)
        .enterScope
        .addLocal("x", 43)

      val result = table.resolve(None, "x", ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("should shadow an outer local symbol with an inner local one") {
      val table = SymbolTable.empty[Int]
        .enterScope
        .addLocal("x", 41)
        .enterScope
        .addLocal("x", 43)

      val result = table.resolve(None, "x", ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("should restore previous symbols when moving back to an outer scope (immutable)") {
      val table1 = SymbolTable.empty[Int].enterScope.addLocal("x", 41)
      val table2 = table1.enterScope.addLocal("x", 43)

      table2.resolve(None, "x", ImportScope.empty).head.value shouldBe 43
      table1.resolve(None, "x", ImportScope.empty).head.value shouldBe 41
    }

    it("should not resolve a symbol after adding it to a different absolute package") {
      val pkg1 = PackagePath.parseAbsolute("org.example1").toOption.get
      val pkg2 = PackagePath.parseAbsolute("org.example2").toOption.get
      val table = SymbolTable.empty[Int].add(pkg1, "x", 41)

      table.resolve(Some(pkg2), "x", ImportScope.empty) shouldBe empty
    }

    it("should return empty List for partially qualified lookup (not yet implemented)") {
      val rel = PackagePath.parseRelative("models").toOption.get
      val table = SymbolTable.empty[Int]
      table.resolve(Some(rel), "User", ImportScope.empty) shouldBe Nil
    }
  }
}
