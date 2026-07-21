package software.kes.scaletta.internal

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins._
import software.kes.scaletta.internal.symbols.SignatureQuery
import software.kes.scaletta.internal.types.{TypeRegistryImpl, TypeUniverse}

class UniverseSpec extends AnyFunSpec with Matchers {

  describe("Universe") {
    it("should delegate getMethodCandidates to symbolTable and OverloadTable") {
      val f = new Fixtures
      val query = SignatureQuery.of(f.intT)
      val result = f.universe.getMethodCandidates(f.typeA, Name("method"), query)
      result should not be empty
      result.forall(_.returnType == f.intT) shouldBe true
    }

    it("should delegate getStaticFunctionCandidates (QualifiedName.Full) to symbolTable and OverloadTable") {
      val f = new Fixtures
      val query = SignatureQuery.of(f.intT)
      val result = f.universe.getStaticFunctionCandidates(f.fullName1, query)
      result should not be empty
      result.forall(_.returnType == f.intT) shouldBe true
    }

    it("should delegate getStaticFunctionCandidates (QualifiedName) to symbolTable and OverloadTable") {
      val f = new Fixtures
      val query = SignatureQuery.of(f.intT)
      // resolveStaticFunction will return both because of the import scope
      val result = f.universe.getStaticFunctionCandidates(QualifiedName.local(Name("fn")), f.importScope, query)
      result should not be empty
    }

    it("should resolveBestMethod") {
      val f = new Fixtures
      val query = SignatureQuery.of(f.intT)
      f.universe.resolveBestMethod(f.typeA, Name("method"), query) should be(a[Right[_, _]])
      f.universe.resolveBestMethod(f.typeA, Name("nonexistent"), query) shouldBe Left(ResolutionError.NotFound)
    }

    it("should resolveBestStaticFunction (QualifiedName.Full)") {
      val f = new Fixtures
      val query = SignatureQuery.of(f.intT)
      f.universe.resolveBestStaticFunction(f.fullName1, query) should be(a[Right[_, _]])
    }

    it("should resolveBestStaticFunction (QualifiedName)") {
      val f = new Fixtures
      val query = SignatureQuery.of(f.intT)
      // If resolving "fn" from import scope is not ambiguous in this specific test environment,
      // it should be Right. 
      f.universe.resolveBestStaticFunction(QualifiedName.local(Name("fn")), f.importScope, query) should be(a[Right[_, _]])

      // Single candidate
      val query2 = SignatureQuery.of(f.stringT)
      f.universe.resolveBestStaticFunction(QualifiedName.local(Name("fn")), f.importScope, query2) should be(a[Right[_, _]])
    }
  }

  final class Fixtures {
    val ns1: PackagePath.Absolute = PackagePath.absolute(PackageSegment("pkg1"))
    val ns2: PackagePath.Absolute = PackagePath.absolute(PackageSegment("pkg2"))

    val typeRegistry: TypeRegistryImpl = new TypeRegistryImpl()
    val intT: Type.Nominal[TypeId] = typeRegistry.addValueType(Packages.scaletta.qualify(Name("Int")), RuntimeTypeInfo.any)
    val stringT: Type.Nominal[TypeId] = typeRegistry.addRefType(Packages.scaletta.qualify(Name("String")), RuntimeTypeInfo.any)
    val typeA: Type.Nominal[TypeId] = typeRegistry.addRefType(ns1.qualify(Name("A")), RuntimeTypeInfo.any)
    val typeUniverse: TypeUniverse = typeRegistry.build()

    val params1: Vector[FormalParameter] = Vector(FormalParameter(Name("p"), intT))
    val params2: Vector[FormalParameter] = Vector(FormalParameter(Name("p"), stringT))

    val fullName1: QualifiedName.Full = ns1.qualify(Name("fn"))
    val fullName2: QualifiedName.Full = ns2.qualify(Name("fn"))

    val mb: MethodUniverseBuilder = MethodUniverseBuilder.create()
    mb.addMethod(MethodName(ReceiverType.Static(ns1), Name("fn")), params1, intT, null)
    mb.addMethod(MethodName(ReceiverType.Static(ns2), Name("fn")), params2, stringT, null)
    mb.addMethod(MethodName(ReceiverType.Instance(typeA), Name("method")), params1, intT, null)

    val mu: MethodUniverse = mb.build()
    val universe: Universe = Universe.create(typeUniverse, mu)

    val importScope: ImportScope = ImportScope.empty
      .importWildcard(ns1)
      .importWildcard(ns2)
  }
}
