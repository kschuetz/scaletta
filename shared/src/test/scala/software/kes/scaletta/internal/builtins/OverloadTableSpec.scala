package software.kes.scaletta.internal.builtins

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}
import software.kes.scaletta.internal.types.{TypeRegistryImpl, TypeUniverse}

class OverloadTableSpec extends AnyFunSpec with Matchers {

  import Fixtures._

  describe("OverloadTable.findCandidates") {
    it("should find an exact match") {
      val query = SignatureQuery.of(intT)
      table.findCandidates(universe, query) should contain(fn1)
    }

    it("should find candidates based on subtyping") {
      // Int is a subtype of Number, so fn2 should be a candidate for Int input
      val query = SignatureQuery.of(intT)
      val candidates = table.findCandidates(universe, query)
      candidates should contain allOf(fn1, fn2)
    }

    it("should NOT find candidates if query type is a supertype of formal parameter") {
      // Number is a supertype of Int. fn1 expects Int.
      // A function expecting Int cannot take a general Number.
      val query = SignatureQuery.of(numberT)
      val candidates = table.findCandidates(universe, query)
      candidates should contain(fn2)
      candidates should not contain fn1
    }

    it("should support Unknown parameter in query") {
      val query = SignatureQuery.of(SignatureQueryParameter.Unknown)
      val candidates = table.findCandidates(universe, query)
      candidates should contain allOf(fn1, fn2)
      candidates should not contain fn3 // fn3 has different group structure
    }

    it("should match multiple parameter groups") {
      val query = SignatureQuery.ofGroups(
        SignatureQuery.Group(Vector(intT)),
        SignatureQuery.Group(Vector(stringT))
      )
      table.findCandidates(universe, query) should contain(fn3)
    }

    it("should return empty list if no variations match the number of groups") {
      val query = SignatureQuery.of(intT, stringT) // 1 group with 2 params
      table.findCandidates(universe, query) shouldBe empty
    }

    it("should return empty list if no variations match the number of parameters in a group") {
      val query = SignatureQuery.ofGroups(
        SignatureQuery.Group(Vector(intT, intT))
      )
      table.findCandidates(universe, query) shouldBe empty
    }

    it("should match when Unknown is used in one of multiple groups") {
      val query = SignatureQuery.ofGroups(
        SignatureQuery.Group(Vector(SignatureQueryParameter.Unknown)),
        SignatureQuery.Group(Vector(stringT))
      )
      table.findCandidates(universe, query) should contain(fn3)
    }
  }

  describe("OverloadTable.resolveBestMatch") {
    it("should return the single matching candidate") {
      val query = SignatureQuery.ofGroups(
        SignatureQuery.Group(Vector(intT)),
        SignatureQuery.Group(Vector(stringT))
      )
      table.resolveBestMatch(universe, query) shouldBe Right(fn3)
    }

    it("should return NotFound if no candidates match") {
      val query = SignatureQuery.of(stringT)
      table.resolveBestMatch(universe, query) shouldBe Left(ResolutionError.NotFound)
    }

    it("should resolve to the most specific candidate") {
      // Both fn1 (Int) and fn2 (Number) match for Int input, but fn1 is more specific
      val query = SignatureQuery.of(intT)
      table.resolveBestMatch(universe, query) shouldBe Right(fn1)
    }

    it("should return Ambiguous if multiple candidates match and none is more specific") {
      val fnAmb1 = NativeFunctionDefinition(
        paramGroups = ParameterGroup.single(FormalParameter(Name("a"), anyT)),
        returnType = intT,
        pure = true,
        nativeFunctionId = NativeFunctionId(43)
      )
      val fnAmb2 = NativeFunctionDefinition(
        paramGroups = ParameterGroup.single(FormalParameter(Name("a"), anyT)),
        returnType = stringT,
        pure = true,
        nativeFunctionId = NativeFunctionId(47)
      )
      val tableAmb = OverloadTable(List(fnAmb1, fnAmb2))
      val query = SignatureQuery.of(intT)
      tableAmb.resolveBestMatch(universe, query) shouldBe Left(ResolutionError.Ambiguous)
    }

    it("should find a match via widening when no exact match is available") {
      import software.kes.scaletta.internal.runtime.CoreTypes
      val fnDouble = NativeFunctionDefinition(
        paramGroups = ParameterGroup.single(FormalParameter(Name("a"), CoreTypes.DoubleT)),
        returnType = CoreTypes.StringT,
        pure = true,
        nativeFunctionId = NativeFunctionId(101)
      )
      val tableOnlyDouble = OverloadTable(List(fnDouble))
      val query = SignatureQuery.of(CoreTypes.IntT)
      tableOnlyDouble.resolveBestMatch(universe, query) shouldBe Right(fnDouble)
    }

    it("should prefer subtyping match over widening match") {
      import software.kes.scaletta.internal.runtime.CoreTypes
      val fnDouble = NativeFunctionDefinition(
        paramGroups = ParameterGroup.single(FormalParameter(Name("a"), CoreTypes.DoubleT)),
        returnType = CoreTypes.StringT,
        pure = true,
        nativeFunctionId = NativeFunctionId(101)
      )
      // fn2 (Number) is a subtype match for Int input.
      // fnDouble is a widening match for Int input.
      val tableWithBoth = OverloadTable(List(fn2, fnDouble))
      val query = SignatureQuery.of(intT)
      tableWithBoth.resolveBestMatch(universe, query) shouldBe Right(fn2)
    }

    it("should prefer a widening match with lower cost") {
      import software.kes.scaletta.internal.runtime.CoreTypes
      val fnLong = NativeFunctionDefinition(
        paramGroups = ParameterGroup.single(FormalParameter(Name("a"), CoreTypes.LongT)),
        returnType = CoreTypes.StringT,
        pure = true,
        nativeFunctionId = NativeFunctionId(53)
      )
      val fnDouble = NativeFunctionDefinition(
        paramGroups = ParameterGroup.single(FormalParameter(Name("a"), CoreTypes.DoubleT)),
        returnType = CoreTypes.StringT,
        pure = true,
        nativeFunctionId = NativeFunctionId(59)
      )
      // Int -> Long cost 1
      // Int -> Double cost 3
      val table = OverloadTable(List(fnLong, fnDouble))
      val query = SignatureQuery.of(CoreTypes.IntT)
      table.resolveBestMatch(universe, query) shouldBe Right(fnLong)
    }
  }

  object Fixtures {
    val ns: PackagePath.Absolute = Packages.scaletta
    val registry: TypeRegistryImpl = new TypeRegistryImpl()
    val anyT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("Any")), RuntimeTypeInfo.any)
    val numberT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("Number")), RuntimeTypeInfo.any)
    val intT: Type.Nominal[TypeId] = registry.addValueType(ns.qualify(Name("Int")), RuntimeTypeInfo.any)
    val stringT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("String")), RuntimeTypeInfo.any)

    registry.addRelationship(anyT, numberT)
    registry.addRelationship(numberT, intT)
    registry.addRelationship(anyT, stringT)

    val universe: TypeUniverse = registry.build()

    val fn1 = NativeFunctionDefinition(
      paramGroups = ParameterGroup.single(FormalParameter(Name("a"), intT)),
      returnType = intT,
      pure = true,
      nativeFunctionId = NativeFunctionId(41)
    )

    val fn2 = NativeFunctionDefinition(
      paramGroups = ParameterGroup.single(FormalParameter(Name("a"), numberT)),
      returnType = stringT,
      pure = true,
      nativeFunctionId = NativeFunctionId(43)
    )

    val fn3 = NativeFunctionDefinition(
      paramGroups = Vector(
        ParameterGroup(Vector(FormalParameter(Name("a"), intT))),
        ParameterGroup(Vector(FormalParameter(Name("b"), stringT)))
      ),
      returnType = anyT,
      pure = true,
      nativeFunctionId = NativeFunctionId(47)
    )

    val table = OverloadTable(List(fn1, fn2, fn3))
  }
}
