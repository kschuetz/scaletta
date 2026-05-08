package software.kes.scaletta.internal.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{Name, PackagePath, QualifiedName}

class SymbolTableSpec extends AnyFunSpec with Matchers {

  describe("SymbolTable") {
    it("should resolve a global symbol in the root package") {
      val table = SymbolTable.of(QualifiedName.full(PackagePath.root, Name("x")) -> 41)
      val result = table.resolve(QualifiedName.local(Name("x")), ImportScope.empty)
      result should have size 1
      result.head.name shouldBe Name("x")
      result.head.container shouldBe Some(PackagePath.root)
      result.head.value shouldBe 41
    }

    it("should resolve a global symbol in a nested package (absolute qualifier)") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val qName = QualifiedName.full(pkg, Name("y"))
      val table = SymbolTable.of(qName -> 43)
      val result = table.resolve(qName, ImportScope.empty)
      result should have size 1
      result.head.name shouldBe Name("y")
      result.head.container shouldBe Some(pkg)
      result.head.value shouldBe 43
    }

    it("should resolve a local symbol (unqualified)") {
      val table = SymbolTable.empty[Int].enterScope.addLocal(Name("z"), 45)
      val result = table.resolve(QualifiedName.local(Name("z")), ImportScope.empty)
      result should have size 1
      result.head.name shouldBe Name("z")
      result.head.container shouldBe None
      result.head.value shouldBe 45
    }

    it("should shadow a global symbol with a local one") {
      val table = SymbolTable.of(QualifiedName.full(PackagePath.root, Name("x")) -> 41)
        .enterScope
        .addLocal(Name("x"), 43)

      val result = table.resolve(QualifiedName.local(Name("x")), ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("shadow an outer local symbol with an inner local one") {
      val table = SymbolTable.empty[Int]
        .enterScope
        .addLocal(Name("x"), 41)
        .enterScope
        .addLocal(Name("x"), 43)

      val result = table.resolve(QualifiedName.local(Name("x")), ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("should restore previous symbols when moving back to an outer scope (immutable)") {
      val table1 = SymbolTable.empty[Int].enterScope.addLocal(Name("x"), 41)
      val table2 = table1.enterScope.addLocal(Name("x"), 43)

      table2.resolve(QualifiedName.local(Name("x")), ImportScope.empty).head.value shouldBe 43
      table1.resolve(QualifiedName.local(Name("x")), ImportScope.empty).head.value shouldBe 41
    }

    it("should not resolve a symbol after adding it to a different absolute package") {
      val pkg1 = PackagePath.parseAbsolute("org.example1")
      val pkg2 = PackagePath.parseAbsolute("org.example2")
      val table = SymbolTable.of(QualifiedName.full(pkg1, Name("x")) -> 41)

      table.resolve(QualifiedName.full(pkg2, Name("x")), ImportScope.empty) shouldBe empty
    }

    it("should resolve a partially qualified symbol via importPackage") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val rel = PackagePath.parseRelative("example.models")
      val absModels = pkg ++ PackagePath.parseRelative("models")
      val table = SymbolTable.of(QualifiedName.full(absModels, Name("User")) -> 41)
      val imports = ImportScope.importPackage(pkg)

      val result = table.resolve(QualifiedName.Partial(Some(rel), Name("User")), imports)
      result should have size 1
      result.head.value shouldBe 41
      result.head.container shouldBe Some(absModels)
    }

    it("should resolve an unqualified symbol via specific import") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val qName = QualifiedName.full(pkg, Name("x"))
      val table = SymbolTable.of(qName -> 41)
      val imports = ImportScope.importSymbol(qName)

      val result = table.resolve(QualifiedName.local(Name("x")), imports)
      result should have size 1
      result.head.value shouldBe 41
    }

    it("should resolve an unqualified symbol via wildcard import") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val table = SymbolTable.of(QualifiedName.full(pkg, Name("x")) -> 41)
      val imports = ImportScope.importWildcard(pkg)

      val result = table.resolve(QualifiedName.local(Name("x")), imports)
      result should have size 1
      result.head.value shouldBe 41
    }

    it("should prioritize specific import over root package") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val table = SymbolTable.of(
        QualifiedName.full(PackagePath.root, Name("x")) -> 41,
        QualifiedName.full(pkg, Name("x")) -> 43
      )
      val imports = ImportScope.importSymbol(QualifiedName.full(pkg, Name("x")))

      val result = table.resolve(QualifiedName.local(Name("x")), imports)
      result should have size 1
      result.head.value shouldBe 43
    }

    it("should prioritize root package over wildcard import") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val table = SymbolTable.of(
        QualifiedName.full(PackagePath.root, Name("x")) -> 41,
        QualifiedName.full(pkg, Name("x")) -> 43
      )
      val imports = ImportScope.importWildcard(pkg)

      val result = table.resolve(QualifiedName.local(Name("x")), imports)
      result should have size 1
      result.head.value shouldBe 41
    }

    it("should return all matches if ambiguous between multiple wildcard imports") {
      val pkg1 = PackagePath.parseAbsolute("org.example1")
      val pkg2 = PackagePath.parseAbsolute("org.example2")
      val table = SymbolTable.of(
        QualifiedName.full(pkg1, Name("x")) -> 41,
        QualifiedName.full(pkg2, Name("x")) -> 43
      )
      val imports = ImportScope.importWildcard(pkg1, pkg2)

      val result = table.resolve(QualifiedName.local(Name("x")), imports)
      result should have size 2
      result.map(_.value) should contain theSameElementsAs List(41, 43)
    }

    it("should support direct lookup via get") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val qName = QualifiedName.full(pkg, Name("y"))
      val table = SymbolTable.of(qName -> 43)
      table.get(qName) shouldBe Some(43)
      table.get(QualifiedName.full(PackagePath.root, Name("y"))) shouldBe None
    }

    it("should support existence check via contains") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val qName = QualifiedName.full(pkg, Name("y"))
      val table = SymbolTable.of(qName -> 43)
      table.contains(qName) shouldBe true
      table.contains(QualifiedName.full(PackagePath.root, Name("y"))) shouldBe false
    }
  }

  describe("SymbolIndex") {
    it("should resolve a global symbol") {
      val index: SymbolIndex[Int] = SymbolIndex.of(QualifiedName.full(PackagePath.root, Name("x")) -> 41)
      val result = index.resolve(QualifiedName.local(Name("x")), ImportScope.empty)
      result should have size 1
      result.head.value shouldBe 41
    }

    it("should allow adding symbols and return a new SymbolIndex") {
      val index1 = SymbolIndex.empty[Int]
      val index2 = index1.add(QualifiedName.full(PackagePath.root, Name("x")), 41)
      index1.resolve(QualifiedName.local(Name("x")), ImportScope.empty) shouldBe Nil
      index2.resolve(QualifiedName.local(Name("x")), ImportScope.empty) should not be empty
    }

    it("should support direct lookup via get") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val qName = QualifiedName.full(pkg, Name("y"))
      val index = SymbolIndex.of(qName -> 43)
      index.get(qName) shouldBe Some(43)
    }

    it("should support existence check via contains") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val qName = QualifiedName.full(pkg, Name("y"))
      val index = SymbolIndex.of(qName -> 43)
      index.contains(qName) shouldBe true
      index.contains(QualifiedName.full(PackagePath.root, Name("y"))) shouldBe false
    }

    it("should merge with another SymbolIndex") {
      val pkg1 = PackagePath.parseAbsolute("org.example1")
      val pkg2 = PackagePath.parseAbsolute("org.example2")
      val index1 = SymbolIndex.of(
        QualifiedName.full(pkg1, Name("x")) -> 41,
        QualifiedName.full(pkg1, Name("y")) -> 43
      )
      val index2 = SymbolIndex.of(
        QualifiedName.full(pkg1, Name("x")) -> 45, // Overwrite
        QualifiedName.full(pkg2, Name("z")) -> 47 // New
      )

      val merged = index1.merge(index2)

      merged.get(QualifiedName.full(pkg1, Name("x"))) shouldBe Some(45)
      merged.get(QualifiedName.full(pkg1, Name("y"))) shouldBe Some(43)
      merged.get(QualifiedName.full(pkg2, Name("z"))) shouldBe Some(47)
    }

    it("should convert to a SymbolTable") {
      val pkg = PackagePath.parseAbsolute("org.example")
      val index = SymbolIndex.of(QualifiedName.full(pkg, Name("x")) -> 41)
      val table = index.toSymbolTable

      table.get(QualifiedName.full(pkg, Name("x"))) shouldBe Some(41)
      table.resolve(QualifiedName.full(pkg, Name("x")), ImportScope.empty) should not be empty
    }

    it("should allow converting from SymbolTable to SymbolIndex") {
      val table = SymbolTable.of(QualifiedName.full(PackagePath.root, Name("x")) -> 41)
      val index = table.toSymbolIndex
      index.get(QualifiedName.full(PackagePath.root, Name("x"))) shouldBe Some(41)
    }
  }
}
