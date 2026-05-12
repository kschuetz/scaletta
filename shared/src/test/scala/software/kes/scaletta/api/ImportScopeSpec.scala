package software.kes.scaletta.api

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ImportScopeSpec extends AnyFunSpec with Matchers {

  describe("ImportScope") {
    it("should be empty by default") {
      val scope = ImportScope.empty
      scope.symbols shouldBe empty
      scope.wildcards shouldBe empty
      scope.packages shouldBe empty
    }

    it("should support importing a package") {
      val path = PackagePath.parseAbsolute("foo.bar.baz")
      val scope = ImportScope.importPackage(path)
      scope.packages should contain(Name("baz") -> path)
    }

    it("should support importing multiple packages") {
      val path1 = PackagePath.parseAbsolute("foo.bar.baz")
      val path2 = PackagePath.parseAbsolute("quux.corge")
      val scope = ImportScope.importPackage(path1, path2)
      scope.packages should contain(Name("baz") -> path1)
      scope.packages should contain(Name("corge") -> path2)
    }

    it("should support importing a specific symbol") {
      val name = QualifiedName.parseFull("foo.bar.baz.Quux")
      val scope = ImportScope.importSymbol(name)
      scope.symbols should contain(Name("Quux") -> name.qualifier)
    }

    it("should support importing multiple specific symbols") {
      val name1 = QualifiedName.parseFull("foo.bar.baz.Quux")
      val name2 = QualifiedName.parseFull("quux.corge.Grault")
      val scope = ImportScope.importSymbol(name1, name2)
      scope.symbols should contain(Name("Quux") -> name1.qualifier)
      scope.symbols should contain(Name("Grault") -> name2.qualifier)
    }

    it("should support importing a set of symbols from a package") {
      val path = PackagePath.parseAbsolute("foo.bar.baz")
      val scope = ImportScope.importSymbols(path, Set(Name("Quux"), Name("Corge")))
      scope.symbols should contain(Name("Quux") -> path)
      scope.symbols should contain(Name("Corge") -> path)
    }

    it("should support wildcard imports") {
      val path = PackagePath.parseAbsolute("foo.bar")
      val scope = ImportScope.importWildcard(path)
      scope.wildcards should contain(path)
    }

    it("should support multiple wildcard imports") {
      val path1 = PackagePath.parseAbsolute("foo.bar")
      val path2 = PackagePath.parseAbsolute("quux.corge")
      val scope = ImportScope.importWildcard(path1, path2)
      scope.wildcards should contain(path1)
      scope.wildcards should contain(path2)
    }

    it("should normalize: wildcard import removes existing specific symbols for the same package") {
      val path = PackagePath.parseAbsolute("foo.bar.baz")
      val scope = ImportScope.empty
        .importSymbols(path, Set(Name("Quux")))
        .importWildcard(path)

      scope.symbols should not contain key(Name("Quux"))
      scope.wildcards should contain(path)
    }

    it("should normalize: specific symbols are not added if a wildcard for that package already exists") {
      val path = PackagePath.parseAbsolute("foo.bar.baz")
      val scope = ImportScope.empty
        .importWildcard(path)
        .importSymbols(path, Set(Name("Quux")))

      scope.symbols should not contain key(Name("Quux"))
      scope.wildcards should contain(path)
    }

    it("should handle shadowing: more recently added specific symbols win") {
      val path1 = PackagePath.parseAbsolute("pkg1")
      val path2 = PackagePath.parseAbsolute("pkg2")
      val scope = ImportScope.empty
        .importSymbols(path1, Set(Name("X")))
        .importSymbols(path2, Set(Name("X")))

      scope.symbols should contain(Name("X") -> path2)
    }

    it("should handle shadowing: more recently added package paths win") {
      val path1 = PackagePath.parseAbsolute("pkg1.util")
      val path2 = PackagePath.parseAbsolute("pkg2.util")
      val scope = ImportScope.empty
        .importPackage(path1)
        .importPackage(path2)

      scope.packages should contain(Name("util") -> path2)
    }

    it("should ignore root package as an explicit package import") {
      val scope = ImportScope.empty.importPackage(PackagePath.root)
      scope.packages shouldBe empty
    }

    it("should support adding rules via the add method") {
      val path = PackagePath.parseAbsolute("foo.bar")
      val scope = ImportScope.empty
        .add(ImportRule.Package(path))
        .add(ImportRule.Symbols(path, Set(Name("X"))))
        .add(ImportRule.Wildcard(path))

      scope.wildcards should contain(path)
      scope.packages should contain(Name("bar") -> path)
      scope.symbols shouldBe empty // Removed because of wildcard
    }

    it("should support combining two ImportScopes with merge") {
      val path1 = PackagePath.parseAbsolute("pkg1")
      val path2 = PackagePath.parseAbsolute("pkg2")
      val path3 = PackagePath.parseAbsolute("pkg3")

      val scope1 = ImportScope.empty
        .importPackage(path1)
        .importSymbols(path2, Set(Name("X")))
        .importWildcard(path3)

      val path4 = PackagePath.parseAbsolute("pkg4")
      val path5 = PackagePath.parseAbsolute("pkg5")
      val path6 = PackagePath.parseAbsolute("pkg6")

      val scope2 = ImportScope.empty
        .importPackage(path4)
        .importSymbols(path5, Set(Name("Y")))
        .importWildcard(path6)

      val combined = scope1.merge(scope2)

      combined.packages should contain(Name("pkg1") -> path1)
      combined.packages should contain(Name("pkg4") -> path4)
      combined.symbols should contain(Name("X") -> path2)
      combined.symbols should contain(Name("Y") -> path5)
      combined.wildcards should contain(path3)
      combined.wildcards should contain(path6)
    }

    it("merge: later scope should shadow earlier scope symbols") {
      val path1 = PackagePath.parseAbsolute("pkg1")
      val path2 = PackagePath.parseAbsolute("pkg2")

      val scope1 = ImportScope.importSymbols(path1, Set(Name("X")))
      val scope2 = ImportScope.importSymbols(path2, Set(Name("X")))

      scope1.merge(scope2).symbols should contain(Name("X") -> path2)
    }

    it("merge: later scope should shadow earlier scope packages") {
      val path1 = PackagePath.parseAbsolute("pkg1.util")
      val path2 = PackagePath.parseAbsolute("pkg2.util")

      val scope1 = ImportScope.importPackage(path1)
      val scope2 = ImportScope.importPackage(path2)

      scope1.merge(scope2).packages should contain(Name("util") -> path2)
    }

    it("merge: should maintain normalization (wildcard in later scope removes symbol in earlier scope)") {
      val path = PackagePath.parseAbsolute("foo.bar")
      val scope1 = ImportScope.importSymbols(path, Set(Name("X")))
      val scope2 = ImportScope.importWildcard(path)

      scope1.merge(scope2).symbols should not contain key(Name("X"))
    }

    it("merge: should maintain normalization (wildcard in earlier scope prevents symbol in later scope)") {
      val path = PackagePath.parseAbsolute("foo.bar")
      val scope1 = ImportScope.importWildcard(path)
      val scope2 = ImportScope.importSymbols(path, Set(Name("X")))

      scope1.merge(scope2).symbols should not contain key(Name("X"))
    }
  }
}
