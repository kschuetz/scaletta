package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._

class TypeUniverseSpec extends AnyFunSpec with Matchers {
  val ns = Packages.scaletta

  describe("TypeRegistryImpl") {
    it("should register nominal types and return unique TypeIds") {
      val registry = new TypeRegistryImpl()
      val name1 = ns.qualify(Name("Type1"))
      val name2 = ns.qualify(Name("Type2"))

      val type1 = registry.addValueType(name1)
      val type2 = registry.addValueType(name2)

      type1.name should not be type2.name
      registry.build().nameIndex.get(name1) shouldBe Some(type1)
      registry.build().nameIndex.get(name2) shouldBe Some(type2)
    }

    it("should maintain subtype relationships") {
      val registry = new TypeRegistryImpl()
      val superName = ns.qualify(Name("Super"))
      val subName = ns.qualify(Name("Sub"))

      val superType = registry.addRefType(superName)
      val subType = registry.addRefType(subName)

      registry.addRelationship(superType, subType)

      val universe = registry.build()
      universe.hierarchy.relationshipFor(subType, superType).isStrictSubtype shouldBe true
      universe.hierarchy.relationshipFor(superType, subType).isStrictSupertype shouldBe true
    }

    it("should handle transitivity in hierarchy") {
      val registry = new TypeRegistryImpl()
      val aName = ns.qualify(Name("A"))
      val bName = ns.qualify(Name("B"))
      val cName = ns.qualify(Name("C"))

      val typeA = registry.addRefType(aName)
      val typeB = registry.addRefType(bName)
      val typeC = registry.addRefType(cName)

      registry.addRelationship(typeA, typeB)
      registry.addRelationship(typeB, typeC)

      val universe = registry.build()
      universe.hierarchy.relationshipFor(typeC, typeA).isStrictSubtype shouldBe true
    }

    it("should support multiple inheritance (common supertype)") {
      val registry = new TypeRegistryImpl()
      val rootName = ns.qualify(Name("Root"))
      val leftName = ns.qualify(Name("Left"))
      val rightName = ns.qualify(Name("Right"))
      val leafName = ns.qualify(Name("Leaf"))

      val root = registry.addRefType(rootName)
      val left = registry.addRefType(leftName)
      val right = registry.addRefType(rightName)
      val leaf = registry.addRefType(leafName)

      registry.addRelationship(root, left)
      registry.addRelationship(root, right)
      registry.addRelationship(left, leaf)
      registry.addRelationship(right, leaf)

      val universe = registry.build()
      val rel = universe.hierarchy.relationshipFor(left, right)
      rel.commonSupertype shouldBe Some(root)
    }
  }
}
