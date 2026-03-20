package software.kes.scaletta.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.PackagePath

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
      val scope = ImportScope.importPackages(path)
      scope.packages should contain("baz" -> path)
    }

    it("should support importing multiple packages") {
      val path1 = PackagePath.parseAbsolute("foo.bar.baz")
      val path2 = PackagePath.parseAbsolute("quux.corge")
      val scope = ImportScope.importPackages(path1, path2)
      scope.packages should contain("baz" -> path1)
      scope.packages should contain("corge" -> path2)
    }

    it("should support importing a specific symbol") {
      val name = QualifiedName.full("foo.bar.baz.Quux")
      val scope = ImportScope.importSymbol(name)
      scope.symbols should contain("Quux" -> name.qualifier)
    }

    it("should support importing multiple specific symbols") {
      val name1 = QualifiedName.full("foo.bar.baz.Quux")
      val name2 = QualifiedName.full("quux.corge.Grault")
      val scope = ImportScope.importSymbol(name1, name2)
      scope.symbols should contain("Quux" -> name1.qualifier)
      scope.symbols should contain("Grault" -> name2.qualifier)
    }

    it("should support importing a set of symbols from a package") {
      val path = PackagePath.parseAbsolute("foo.bar.baz")
      val scope = ImportScope.importSymbols(path, Set("Quux", "Corge"))
      scope.symbols should contain("Quux" -> path)
      scope.symbols should contain("Corge" -> path)
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
        .importSymbols(path, Set("Quux"))
        .importWildcard(path)

      scope.symbols should not contain key("Quux")
      scope.wildcards should contain(path)
    }

    it("should normalize: specific symbols are not added if a wildcard for that package already exists") {
      val path = PackagePath.parseAbsolute("foo.bar.baz")
      val scope = ImportScope.empty
        .importWildcard(path)
        .importSymbols(path, Set("Quux"))

      scope.symbols should not contain key("Quux")
      scope.wildcards should contain(path)
    }

    it("should handle shadowing: more recently added specific symbols win") {
      val path1 = PackagePath.parseAbsolute("pkg1")
      val path2 = PackagePath.parseAbsolute("pkg2")
      val scope = ImportScope.empty
        .importSymbols(path1, Set("X"))
        .importSymbols(path2, Set("X"))

      scope.symbols should contain("X" -> path2)
    }

    it("should handle shadowing: more recently added package paths win") {
      val path1 = PackagePath.parseAbsolute("pkg1.util")
      val path2 = PackagePath.parseAbsolute("pkg2.util")
      val scope = ImportScope.empty
        .importPackage(path1)
        .importPackage(path2)

      scope.packages should contain("util" -> path2)
    }

    it("should ignore root package as an explicit package import") {
      val scope = ImportScope.empty.importPackage(PackagePath.root)
      scope.packages shouldBe empty
    }

    it("should support adding rules via the add method") {
      val path = PackagePath.parseAbsolute("foo.bar")
      val scope = ImportScope.empty
        .add(ImportRule.Package(path))
        .add(ImportRule.Symbols(path, Set("X")))
        .add(ImportRule.Wildcard(path))

      scope.wildcards should contain(path)
      scope.packages should contain("bar" -> path)
      scope.symbols shouldBe empty // Removed because of wildcard
    }
  }
}
