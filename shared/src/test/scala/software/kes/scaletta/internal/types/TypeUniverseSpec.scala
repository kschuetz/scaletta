package software.kes.scaletta.internal.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ast.TermSymbolId

class TypeUniverseSpec extends AnyFunSpec with Matchers {
  val ns = Packages.scaletta

  describe("TypeRegistryImpl") {
    it("should register static singleton names and runtime metadata") {
      val registry = new TypeRegistryImpl()
      val name = ns.qualify(Name("Singleton"))
      val otherName = ns.qualify(Name("OtherSingleton"))
      val id = TermSymbolId.Static(name)
      val otherId = TermSymbolId.Static(otherName)
      val strategy = UnapplyStrategy.unapplyZero(_ == "singleton")
      val info = RuntimeTypeInfo(_ == "singleton", strategy)
      val otherInfo = RuntimeTypeInfo(_ == "other")

      val singleton = registry.addStaticSingleton(name, info)
      val other = registry.addStaticSingleton(otherName, otherInfo)
      val universe = registry.build()

      singleton shouldBe Type.Singleton(id)
      other shouldBe Type.Singleton(otherId)
      universe.nameIndex.get(name) shouldBe Some(singleton)
      universe.nameIndex.get(otherName) shouldBe Some(other)
      universe.staticSingletonInfoMap.get(id) shouldBe Some(info)
      universe.staticSingletonInfoMap.get(otherId) shouldBe Some(otherInfo)
      universe.getIsInstance(id)("singleton") shouldBe true
      universe.getIsInstance(id)("other") shouldBe false
      universe.getIsInstance(otherId)("other") shouldBe true
      universe.getUnapplyStrategy(id) shouldBe strategy

      val missing = TermSymbolId.Static(ns.qualify(Name("Missing")))
      universe.staticSingletonInfoMap.get(missing) shouldBe None
      universe.getIsInstance(missing)("singleton") shouldBe false
      universe.getUnapplyStrategy(missing) shouldBe UnapplyStrategy.noUnapply
    }

    it("should register nominal types and return unique TypeIds") {
      val registry = new TypeRegistryImpl()
      val name1 = ns.qualify(Name("Type1"))
      val name2 = ns.qualify(Name("Type2"))

      val type1 = registry.addValueType(name1, RuntimeTypeInfo.any)
      val type2 = registry.addValueType(name2, RuntimeTypeInfo.any)

      type1.name should not be type2.name
      registry.build().nameIndex.get(name1) shouldBe Some(type1)
      registry.build().nameIndex.get(name2) shouldBe Some(type2)
    }

    it("should maintain subtype relationships") {
      val registry = new TypeRegistryImpl()
      val superName = ns.qualify(Name("Super"))
      val subName = ns.qualify(Name("Sub"))

      val superType = registry.addRefType(superName, RuntimeTypeInfo.any)
      val subType = registry.addRefType(subName, RuntimeTypeInfo.any)

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

      val typeA = registry.addRefType(aName, RuntimeTypeInfo.any)
      val typeB = registry.addRefType(bName, RuntimeTypeInfo.any)
      val typeC = registry.addRefType(cName, RuntimeTypeInfo.any)

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

      val root = registry.addRefType(rootName, RuntimeTypeInfo.any)
      val left = registry.addRefType(leftName, RuntimeTypeInfo.any)
      val right = registry.addRefType(rightName, RuntimeTypeInfo.any)
      val leaf = registry.addRefType(leafName, RuntimeTypeInfo.any)

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
