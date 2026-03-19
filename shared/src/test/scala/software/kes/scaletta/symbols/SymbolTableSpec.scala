package software.kes.scaletta.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.PackagePath

class SymbolTableSpec extends AnyFunSpec with Matchers {

  describe("SymbolTable") {
    it("should resolve a global symbol in the root package") {
      val table = SymbolTable.empty[Int].add(QualifiedName.full(PackagePath.root, "x"), 41)
      val result = table.resolve(QualifiedName.local("x"), ImportScope.empty)
      result should have size 1
      result.head.name shouldBe "x"
      result.head.container shouldBe Some(PackagePath.root)
      result.head.value shouldBe 41
    }

    it("should resolve a global symbol in a nested package (absolute qualifier)") {
      val pkg = PackagePath.parseAbsolute("org.example").toOption.get
      val qName = QualifiedName.full(pkg, "y")
      val table = SymbolTable.empty[Int].add(qName, 43)
      val result = table.resolve(qName, ImportScope.empty)
      result should have size 1
      result.head.name shouldBe "y"
      result.head.container shouldBe Some(pkg)
      result.head.value shouldBe 43
    }

    it("should resolve a local symbol (unqualified)") {
      val table = SymbolTable.empty[Int].enterScope.addLocal("z", 45)
      val result = table.resolve(QualifiedName.local("z"), ImportScope.empty)
      result should have size 1
      result.head.name shouldBe "z"
      result.head.container shouldBe None
      result.head.value shouldBe 45
    }

    it("should shadow a global symbol with a local one") {
      val table = SymbolTable.empty[Int]
        .add(QualifiedName.full(PackagePath.root, "x"), 41)
        .enterScope
        .addLocal("x", 43)

      val result = table.resolve(QualifiedName.local("x"), ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("should shadow an outer local symbol with an inner local one") {
      val table = SymbolTable.empty[Int]
        .enterScope
        .addLocal("x", 41)
        .enterScope
        .addLocal("x", 43)

      val result = table.resolve(QualifiedName.local("x"), ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("should restore previous symbols when moving back to an outer scope (immutable)") {
      val table1 = SymbolTable.empty[Int].enterScope.addLocal("x", 41)
      val table2 = table1.enterScope.addLocal("x", 43)

      table2.resolve(QualifiedName.local("x"), ImportScope.empty).head.value shouldBe 43
      table1.resolve(QualifiedName.local("x"), ImportScope.empty).head.value shouldBe 41
    }

    it("should not resolve a symbol after adding it to a different absolute package") {
      val pkg1 = PackagePath.parseAbsolute("org.example1").toOption.get
      val pkg2 = PackagePath.parseAbsolute("org.example2").toOption.get
      val table = SymbolTable.empty[Int].add(QualifiedName.full(pkg1, "x"), 41)

      table.resolve(QualifiedName.full(pkg2, "x"), ImportScope.empty) shouldBe empty
    }

    it("should return empty List for partially qualified lookup (not yet implemented)") {
      val rel = PackagePath.parseRelative("models").toOption.get
      val table = SymbolTable.empty[Int]
      table.resolve(QualifiedName.Partial(Some(rel), "User"), ImportScope.empty) shouldBe Nil
    }

    it("should support direct lookup via get") {
      val pkg = PackagePath.parseAbsolute("org.example").toOption.get
      val qName = QualifiedName.full(pkg, "y")
      val table = SymbolTable.empty[Int].add(qName, 43)
      table.get(qName) shouldBe Some(43)
      table.get(QualifiedName.full(PackagePath.root, "y")) shouldBe None
    }

    it("should support existence check via contains") {
      val pkg = PackagePath.parseAbsolute("org.example").toOption.get
      val qName = QualifiedName.full(pkg, "y")
      val table = SymbolTable.empty[Int].add(qName, 43)
      table.contains(qName) shouldBe true
      table.contains(QualifiedName.full(PackagePath.root, "y")) shouldBe false
    }
  }

  describe("SymbolIndex") {
    it("should resolve a global symbol") {
      val index: SymbolIndex[Int] = SymbolIndex.empty[Int].add(QualifiedName.full(PackagePath.root, "x"), 41)
      val result = index.resolve(QualifiedName.local("x"), ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 41
    }

    it("should allow adding symbols and return a new SymbolIndex") {
      val index1 = SymbolIndex.empty[Int]
      val index2 = index1.add(QualifiedName.full(PackagePath.root, "x"), 41)
      index1.resolve(QualifiedName.local("x"), ImportScope.empty) shouldBe Nil
      index2.resolve(QualifiedName.local("x"), ImportScope.empty) should not be empty
    }

    it("should support direct lookup via get") {
      val pkg = PackagePath.parseAbsolute("org.example").toOption.get
      val qName = QualifiedName.full(pkg, "y")
      val index = SymbolIndex.empty[Int].add(qName, 43)
      index.get(qName) shouldBe Some(43)
    }

    it("should support existence check via contains") {
      val pkg = PackagePath.parseAbsolute("org.example").toOption.get
      val qName = QualifiedName.full(pkg, "y")
      val index = SymbolIndex.empty[Int].add(qName, 43)
      index.contains(qName) shouldBe true
      index.contains(QualifiedName.full(PackagePath.root, "y")) shouldBe false
    }
  }
}
