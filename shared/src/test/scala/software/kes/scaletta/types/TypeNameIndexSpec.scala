package software.kes.scaletta.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.PackagePath
import software.kes.scaletta.internal.symbols.{ImportScope, Name, QualifiedName}

class TypeNameIndexSpec extends AnyFunSpec with Matchers {
  describe("TypeNameIndex") {
    it("should be empty initially") {
      val index = TypeNameIndex.empty
      index.size shouldBe 0
      index.allNames shouldBe empty
    }

    it("should intern new names and assign incremental TypeIds") {
      val index0 = TypeNameIndex.empty
      val nameA = QualifiedName.parseFull("scaletta.lang.Int")
      val nameB = QualifiedName.parseFull("scaletta.lang.String")

      val (index1, idA) = index0.intern(nameA)
      val (index2, idB) = index1.intern(nameB)

      idA.value shouldBe 0
      idB.value shouldBe 1
      index2.size shouldBe 2
      index2.getName(idA) shouldBe nameA
      index2.getName(idB) shouldBe nameB
    }

    it("should be idempotent when interning the same name") {
      val index0 = TypeNameIndex.empty
      val name = QualifiedName.parseFull("scaletta.lang.Int")

      val (index1, id1) = index0.intern(name)
      val (index2, id2) = index1.intern(name)

      id1 shouldBe id2
      index1 shouldBe index2
      index2.size shouldBe 1
    }

    it("should support direct lookup via get") {
      val name = QualifiedName.parseFull("scaletta.lang.Int")
      val (index, id) = TypeNameIndex.empty.intern(name)

      index.get(name) shouldBe Some(id)
      index.get(QualifiedName.parseFull("other.Type")) shouldBe None
    }

    describe("resolve with imports") {
      it("should support resolution via resolve with empty imports (full name)") {
        val nameInt = QualifiedName.parseFull("scaletta.lang.Int")
        val (index, idInt) = TypeNameIndex.empty.intern(nameInt)

        val results = index.resolve(nameInt, ImportScope.empty)

        results should have size 1
        results.head.value shouldBe idInt
        results.head.name shouldBe Name("Int")
        results.head.qualifier shouldBe nameInt.qualifier
      }

      it("should support resolution via specific symbol import") {
        val nameA = QualifiedName.parseFull("pkg.A")
        val (index, idA) = TypeNameIndex.empty.intern(nameA)
        val imports = ImportScope.importSymbol(nameA)

        val results = index.resolve(QualifiedName.local(Name("A")), imports)

        results should have size 1
        results.head.value shouldBe idA
        results.head.qualifier shouldBe PackagePath.parseAbsolute("pkg")
      }

      it("should support resolution via wildcard import") {
        val nameA = QualifiedName.parseFull("pkg.A")
        val nameB = QualifiedName.parseFull("pkg.B")
        val (index1, idA) = TypeNameIndex.empty.intern(nameA)
        val (index2, idB) = index1.intern(nameB)
        val imports = ImportScope.importWildcard(PackagePath.parseAbsolute("pkg"))

        index2.resolve(QualifiedName.local(Name("A")), imports).map(_.value) should contain only idA
        index2.resolve(QualifiedName.local(Name("B")), imports).map(_.value) should contain only idB
      }

      it("should handle shadowing: specific import wins over wildcard") {
        val namePkgA = QualifiedName.parseFull("pkg.A")
        val nameOtherA = QualifiedName.parseFull("other.A")
        val (index1, idPkgA) = TypeNameIndex.empty.intern(namePkgA)
        val (index2, idOtherA) = index1.intern(nameOtherA)

        val imports = ImportScope.empty
          .importWildcard(PackagePath.parseAbsolute("pkg"))
          .importSymbol(nameOtherA)

        val results = index2.resolve(QualifiedName.local(Name("A")), imports)
        results should have size 1
        results.head.value shouldBe idOtherA
      }

      it("should handle ambiguity: multiple matches from different wildcards") {
        val namePkg1A = QualifiedName.parseFull("pkg1.A")
        val namePkg2A = QualifiedName.parseFull("pkg2.A")
        val (index1, id1) = TypeNameIndex.empty.intern(namePkg1A)
        val (index2, id2) = index1.intern(namePkg2A)

        val imports = ImportScope.empty
          .importWildcard(PackagePath.parseAbsolute("pkg1"))
          .importWildcard(PackagePath.parseAbsolute("pkg2"))

        val results = index2.resolve(QualifiedName.local(Name("A")), imports)
        results.map(_.value) should contain theSameElementsAs List(id1, id2)
      }

      it("should support resolution via package import and relative path") {
        val nameA = QualifiedName.parseFull("pkg.sub.A")
        val (index, idA) = TypeNameIndex.empty.intern(nameA)
        val imports = ImportScope.importPackage(PackagePath.parseAbsolute("pkg.sub"))

        val results = index.resolve(QualifiedName.tryParsePartial("sub.A").getOrElse(fail("failed to parse")), imports)

        results should have size 1
        results.head.value shouldBe idA
        results.head.qualifier shouldBe PackagePath.parseAbsolute("pkg.sub")
      }

      it("should handle shadowing: local root symbols over wildcard imports") {
        val nameRootA = QualifiedName.parseFull("A")
        val namePkgA = QualifiedName.parseFull("pkg.A")
        val (index1, idRootA) = TypeNameIndex.empty.intern(nameRootA)
        val (index2, idPkgA) = index1.intern(namePkgA)

        val imports = ImportScope.importWildcard(PackagePath.parseAbsolute("pkg"))

        val results = index2.resolve(QualifiedName.local(Name("A")), imports)
        // Root package has priority over wildcard imports in resolveGlobal
        results should have size 1
        results.head.value shouldBe idRootA
      }
    }

    it("should correctly retrieve name by TypeId") {
      val nameA = QualifiedName.parseFull("A")
      val nameB = QualifiedName.parseFull("B")
      val (index1, idA) = TypeNameIndex.empty.intern(nameA)
      val (index2, idB) = index1.intern(nameB)

      index2.getName(idA) shouldBe nameA
      index2.getName(idB) shouldBe nameB
    }

    describe("addUnique") {
      it("should add a name that doesn't exist") {
        val name = QualifiedName.parseFull("scaletta.lang.Int")
        val maybeResult = TypeNameIndex.empty.addUnique(name)

        maybeResult shouldBe defined
        val (index, id) = maybeResult.get
        index.size shouldBe 1
        id.value shouldBe 0
        index.get(name) shouldBe Some(id)
      }

      it("should return None when adding a name that already exists") {
        val name = QualifiedName.parseFull("scaletta.lang.Int")
        val (index1, _) = TypeNameIndex.empty.intern(name)

        index1.addUnique(name) shouldBe None
      }
    }

    it("should throw an exception when getName is called with an invalid TypeId") {
      val index = TypeNameIndex.empty
      intercept[IndexOutOfBoundsException] {
        index.getName(TypeId(41))
      }
    }
  }
}
