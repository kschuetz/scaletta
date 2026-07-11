package software.kes.scaletta.internal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins._
import software.kes.scaletta.internal.symbols.SignatureQuery
import software.kes.scaletta.internal.types.TypeRegistryImpl

class MethodInheritanceSpec extends AnyFunSpec with Matchers {

  describe("Method resolution with inheritance") {
    it("should find a method defined on a supertype") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val baseType = typeRegistry.addRefType(ns.qualify(Name("Base")))
      val derivedType = typeRegistry.addRefType(ns.qualify(Name("Derived")))
      typeRegistry.addRelationship(baseType, derivedType)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))
      val params = Vector(FormalParameter(Name("n"), intT))

      val mb = MethodUniverseBuilder.create()
      mb.addMethod(MethodName(ReceiverType.Instance(baseType), Name("foo")), params, intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)
      val query = SignatureQuery.of(intT)

      val candidates = universe.getMethodCandidates(derivedType, Name("foo"), query)
      candidates should not be empty
    }

    it("should prefer a method defined on the subtype over one on the supertype") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val baseType = typeRegistry.addRefType(ns.qualify(Name("Base")))
      val derivedType = typeRegistry.addRefType(ns.qualify(Name("Derived")))
      typeRegistry.addRelationship(baseType, derivedType)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))
      val params = Vector(FormalParameter(Name("n"), intT))

      val mb = MethodUniverseBuilder.create()
      val idBase = mb.addMethod(MethodName(ReceiverType.Instance(baseType), Name("foo")), params, intT, null)
      val idDerived = mb.addMethod(MethodName(ReceiverType.Instance(derivedType), Name("foo")), params, intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)
      val query = SignatureQuery.of(intT)

      val result = universe.resolveBestMethod(derivedType, Name("foo"), query)
      result.isRight shouldBe true
      assert(result.toOption.get.nativeFunctionId == idDerived)
    }

    it("should find a method defined on a grandparent") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val grandparentType = typeRegistry.addRefType(ns.qualify(Name("Grandparent")))
      val parentType = typeRegistry.addRefType(ns.qualify(Name("Parent")))
      val childType = typeRegistry.addRefType(ns.qualify(Name("Child")))
      typeRegistry.addRelationship(grandparentType, parentType)
      typeRegistry.addRelationship(parentType, childType)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))
      val params = Vector(FormalParameter(Name("n"), intT))

      val mb = MethodUniverseBuilder.create()
      val idGrandparent = mb.addMethod(MethodName(ReceiverType.Instance(grandparentType), Name("foo")), params, intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)
      val query = SignatureQuery.of(intT)

      val result = universe.resolveBestMethod(childType, Name("foo"), query)
      result.isRight shouldBe true
      assert(result.toOption.get.nativeFunctionId == idGrandparent)
    }

    it("should pick the closest method in a 3-level hierarchy") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val grandparentType = typeRegistry.addRefType(ns.qualify(Name("Grandparent")))
      val parentType = typeRegistry.addRefType(ns.qualify(Name("Parent")))
      val childType = typeRegistry.addRefType(ns.qualify(Name("Child")))
      typeRegistry.addRelationship(grandparentType, parentType)
      typeRegistry.addRelationship(parentType, childType)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))
      val params = Vector(FormalParameter(Name("n"), intT))

      val mb = MethodUniverseBuilder.create()
      mb.addMethod(MethodName(ReceiverType.Instance(grandparentType), Name("foo")), params, intT, null)
      val idParent = mb.addMethod(MethodName(ReceiverType.Instance(parentType), Name("foo")), params, intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)
      val query = SignatureQuery.of(intT)

      val result = universe.resolveBestMethod(childType, Name("foo"), query)
      result.isRight shouldBe true
      assert(result.toOption.get.nativeFunctionId == idParent)
    }

    it("should resolve overloads across the hierarchy") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val baseType = typeRegistry.addRefType(ns.qualify(Name("Base")))
      val derivedType = typeRegistry.addRefType(ns.qualify(Name("Derived")))
      typeRegistry.addRelationship(baseType, derivedType)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))
      val stringT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("String")))

      val mb = MethodUniverseBuilder.create()
      val idBaseInt = mb.addMethod(MethodName(ReceiverType.Instance(baseType), Name("foo")), Vector(FormalParameter(Name("n"), intT)), intT, null)
      val idDerivedString = mb.addMethod(MethodName(ReceiverType.Instance(derivedType), Name("foo")), Vector(FormalParameter(Name("s"), stringT)), intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)

      val resultInt = universe.resolveBestMethod(derivedType, Name("foo"), SignatureQuery.of(intT))
      resultInt.isRight shouldBe true
      assert(resultInt.toOption.get.nativeFunctionId == idBaseInt)

      val resultString = universe.resolveBestMethod(derivedType, Name("foo"), SignatureQuery.of(stringT))
      resultString.isRight shouldBe true
      assert(resultString.toOption.get.nativeFunctionId == idDerivedString)
    }

    it("should resolve methods from multiple supertypes") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val traitA = typeRegistry.addRefType(ns.qualify(Name("TraitA")))
      val traitB = typeRegistry.addRefType(ns.qualify(Name("TraitB")))
      val impl = typeRegistry.addRefType(ns.qualify(Name("Impl")))
      typeRegistry.addRelationship(traitA, impl)
      typeRegistry.addRelationship(traitB, impl)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))

      val mb = MethodUniverseBuilder.create()
      val idA = mb.addMethod(MethodName(ReceiverType.Instance(traitA), Name("foo")), Vector.empty, intT, null)
      val idB = mb.addMethod(MethodName(ReceiverType.Instance(traitB), Name("bar")), Vector.empty, intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)

      val resA = universe.resolveBestMethod(impl, Name("foo"), SignatureQuery.empty)
      resA.isRight shouldBe true
      assert(resA.toOption.get.nativeFunctionId == idA)

      val resB = universe.resolveBestMethod(impl, Name("bar"), SignatureQuery.empty)
      resB.isRight shouldBe true
      assert(resB.toOption.get.nativeFunctionId == idB)
    }

    it("should report ambiguity when multiple supertypes provide the same method") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val traitA = typeRegistry.addRefType(ns.qualify(Name("TraitA")))
      val traitB = typeRegistry.addRefType(ns.qualify(Name("TraitB")))
      val impl = typeRegistry.addRefType(ns.qualify(Name("Impl")))
      typeRegistry.addRelationship(traitA, impl)
      typeRegistry.addRelationship(traitB, impl)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))

      val mb = MethodUniverseBuilder.create()
      mb.addMethod(MethodName(ReceiverType.Instance(traitA), Name("foo")), Vector.empty, intT, null)
      mb.addMethod(MethodName(ReceiverType.Instance(traitB), Name("foo")), Vector.empty, intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)

      val result = universe.resolveBestMethod(impl, Name("foo"), SignatureQuery.empty)
      result shouldBe Left(ResolutionError.Ambiguous)
    }

    it("should resolve methods with multiple parameter groups across the hierarchy") {
      val ns = PackagePath.absolute(PackageSegment("test"))
      val typeRegistry = new TypeRegistryImpl()
      val baseType = typeRegistry.addRefType(ns.qualify(Name("Base")))
      val derivedType = typeRegistry.addRefType(ns.qualify(Name("Derived")))
      typeRegistry.addRelationship(baseType, derivedType)
      val typeUniverse = typeRegistry.build()

      val intT = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")))

      val mb = MethodUniverseBuilder.create()
      val idBase = mb.addMultiParamGroupMethod(MethodName(ReceiverType.Instance(baseType), Name("foo")),
        Vector(ParameterGroup(Vector(FormalParameter(Name("a"), intT))), ParameterGroup(Vector(FormalParameter(Name("b"), intT)))),
        intT, null)
      val mu = mb.build()

      val universe = Universe.create(typeUniverse, mu)
      val query = SignatureQuery.ofGroups(SignatureQuery.Group(Vector(intT)), SignatureQuery.Group(Vector(intT)))

      val result = universe.resolveBestMethod(derivedType, Name("foo"), query)
      result.isRight shouldBe true
      assert(result.toOption.get.nativeFunctionId == idBase)
    }

  }
}
