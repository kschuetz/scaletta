package software.kes.scaletta.internal.symbols

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.{Name, PackagePath}
import software.kes.scaletta.internal.library.standard.CommonParameterNames

class PackageNodeSpec extends AnyFunSpec with Matchers {

  describe("PackageNode") {
    it("should be empty initially") {
      val node = PackageNode.empty[Int]
      node.symbols shouldBe empty
      node.subpackages shouldBe empty
    }

    it("should add a symbol at the root package") {
      val node = PackageNode.empty[Int].add(PackagePath.root, CommonParameterNames.x, 41)
      node.get(CommonParameterNames.x) shouldBe Some(41)
      node.subpackages shouldBe empty
    }

    it("should add a symbol at a nested package") {
      val path = PackagePath.parseAbsolute("org.example")
      val node = PackageNode.empty[Int].add(path, CommonParameterNames.y, 43)

      node.get(CommonParameterNames.y) shouldBe None
      val sub = node.findNode(path)
      sub shouldBe defined
      sub.get.get(CommonParameterNames.y) shouldBe Some(43)
    }

    it("should create multiple levels of nested packages") {
      val path = PackagePath.parseAbsolute("a.b.c")
      val node = PackageNode.empty[Int].add(path, Name("z"), 45)

      node.findNode(PackagePath.parseAbsolute("a")) shouldBe defined
      node.findNode(PackagePath.parseAbsolute("a.b")) shouldBe defined
      node.findNode(path).get.get(Name("z")) shouldBe Some(45)
    }

    it("should support structural sharing when adding symbols") {
      val path1 = PackagePath.parseAbsolute("org.example.p1")
      val path2 = PackagePath.parseAbsolute("org.example.p2")

      val node1 = PackageNode.empty[Int].add(path1, CommonParameterNames.x, 41)
      val node2 = node1.add(path2, CommonParameterNames.y, 43)

      val org1 = node1.findNode(PackagePath.parseAbsolute("org")).get
      val org2 = node2.findNode(PackagePath.parseAbsolute("org")).get

      // They should not be the same because the subpackages changed
      org1 should not be theSameInstanceAs(org2)

      // But if we add a symbol to the same package, unrelated packages should be shared
      val p1_1 = node1.findNode(path1).get
      val p1_2 = node2.findNode(path1).get

      p1_1 should be theSameInstanceAs p1_2
    }

    it("should return None when findNode cannot find the path") {
      val node = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a.b"), CommonParameterNames.x, 41)
      node.findNode(PackagePath.parseAbsolute("a.c")) shouldBe None
      node.findNode(PackagePath.parseAbsolute("a.b.c")) shouldBe None
    }

    it("should return the root node for an empty absolute path") {
      val node = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a"), CommonParameterNames.x, 41)
      node.findNode(PackagePath.root).get should be theSameInstanceAs node
    }

    it("should find nodes using relative paths") {
      val node = PackageNode.empty[Int]
        .add(PackagePath.parseAbsolute("a.b.c"), CommonParameterNames.x, 41)

      val aNode = node.findNode(PackagePath.parseAbsolute("a")).get
      val bcNode = aNode.findNode(PackagePath.parseRelative("b.c")).get

      bcNode.get(CommonParameterNames.x) shouldBe Some(41)
    }

    describe("merge") {
      it("should merge symbols in the same package") {
        val node1 = PackageNode.empty[Int].add(PackagePath.root, CommonParameterNames.x, 41)
        val node2 = PackageNode.empty[Int].add(PackagePath.root, CommonParameterNames.y, 43)

        val merged = node1.merge(node2)
        merged.get(CommonParameterNames.x) shouldBe Some(41)
        merged.get(CommonParameterNames.y) shouldBe Some(43)
      }

      it("should overwrite symbols when merging") {
        val node1 = PackageNode.empty[Int].add(PackagePath.root, CommonParameterNames.x, 41)
        val node2 = PackageNode.empty[Int].add(PackagePath.root, CommonParameterNames.x, 43)

        val merged = node1.merge(node2)
        merged.get(CommonParameterNames.x) shouldBe Some(43)
      }

      it("should merge nested packages") {
        val node1 = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a.b"), CommonParameterNames.x, 41)
        val node2 = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a.c"), CommonParameterNames.y, 43)

        val merged = node1.merge(node2)
        merged.findNode(PackagePath.parseAbsolute("a.b")).get.get(CommonParameterNames.x) shouldBe Some(41)
        merged.findNode(PackagePath.parseAbsolute("a.c")).get.get(CommonParameterNames.y) shouldBe Some(43)
      }

      it("should merge deeply nested packages with overlapping paths") {
        val node1 = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a.b.c"), CommonParameterNames.x, 41)
        val node2 = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a.b.d"), CommonParameterNames.y, 43)

        val merged = node1.merge(node2)
        val abNode = merged.findNode(PackagePath.parseAbsolute("a.b")).get
        abNode.findNode(PackagePath.parseRelative("c")).get.get(CommonParameterNames.x) shouldBe Some(41)
        abNode.findNode(PackagePath.parseRelative("d")).get.get(CommonParameterNames.y) shouldBe Some(43)
      }

      it("should be identity when merging with empty") {
        val node = PackageNode.empty[Int].add(PackagePath.parseAbsolute("a.b"), CommonParameterNames.x, 41)

        node.merge(PackageNode.empty[Int]) shouldBe node
        PackageNode.empty[Int].merge(node) shouldBe node
      }
    }
  }
}
