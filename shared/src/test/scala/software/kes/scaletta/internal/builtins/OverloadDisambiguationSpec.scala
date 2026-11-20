package software.kes.scaletta.internal.builtins

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types.{TypeRegistryImpl, TypeUniverse}

final class OverloadDisambiguationSpec extends AnyFunSpec with Matchers {

  import OverloadDisambiguationSpec.Fixtures._

  describe("Overload disambiguation engine") {

    describe("Subtyping specificity") {
      it("should resolve Derived parameter over Base parameter for Derived argument") {
        val fnBase = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), baseT)),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(41)
        )
        val fnDerived = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), derivedT)),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(43)
        )

        val candidates = Vector(fnBase, fnDerived)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(derivedT)))

        result shouldBe Right(fnDerived)
      }

      it("should resolve Base parameter for Base argument when Derived is inapplicable") {
        val fnBase = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), baseT)),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(41)
        )
        val fnDerived = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), derivedT)),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(43)
        )

        val candidates = Vector(fnBase, fnDerived)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(baseT)))

        result shouldBe Right(fnBase)
      }
    }

    describe("Multi-group currying") {
      it("should resolve specificity across multiple parameter groups") {
        val fnGeneral = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup(Vector(FormalParameter(Name("a"), anyT))),
            ParameterGroup(Vector(FormalParameter(Name("b"), stringT)))
          ),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(47)
        )
        val fnSpecific = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup(Vector(FormalParameter(Name("a"), intT))),
            ParameterGroup(Vector(FormalParameter(Name("b"), stringT)))
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(53)
        )

        val candidates = Vector(fnGeneral, fnSpecific)
        val argTypes = Vector(Vector(intT), Vector(stringT))
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, argTypes)

        result shouldBe Right(fnSpecific)
      }

      it("should reject multi-group call when group count does not match") {
        val fnTwoGroups = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup(Vector(FormalParameter(Name("a"), intT))),
            ParameterGroup(Vector(FormalParameter(Name("b"), stringT)))
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(59)
        )

        val candidates = Vector(fnTwoGroups)
        val argTypes = Vector(Vector(intT, stringT))
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, argTypes)

        result shouldBe Left(ResolutionError.NotFound)
      }
    }

    describe("Variadics") {
      it("should resolve variadic method with 0 varargs") {
        val fnVar = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup.variadic(
              Vector(FormalParameter(Name("first"), intT)),
              VariadicParameter(Name("rest"), stringT)
            )
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(61)
        )

        val candidates = Vector(fnVar)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT)))

        result shouldBe Right(fnVar)
      }

      it("should resolve variadic method with 1 and N varargs") {
        val fnVar = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup.variadic(
              Vector(FormalParameter(Name("first"), intT)),
              VariadicParameter(Name("rest"), stringT)
            )
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(61)
        )

        val candidates = Vector(fnVar)

        val result1 = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT, stringT)))
        result1 shouldBe Right(fnVar)

        val resultN = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT, stringT, stringT, stringT)))
        resultN shouldBe Right(fnVar)
      }

      it("should prefer exact fixed-arity overload over variadic overload") {
        val fnFixed = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(
            FormalParameter(Name("a"), intT),
            FormalParameter(Name("b"), intT)
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(67)
        )
        val fnVar = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup.variadic(
              Vector(FormalParameter(Name("a"), intT)),
              VariadicParameter(Name("rest"), intT)
            )
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(71)
        )

        val candidates = Vector(fnFixed, fnVar)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT, intT)))

        result shouldBe Right(fnFixed)
      }

      it("should prefer variadic with fixed prefix over purely variadic overload") {
        val fnPureVar = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup.variadic(
              Vector.empty,
              VariadicParameter(Name("rest"), anyT)
            )
          ),
          returnType = anyT,
          pure = true,
          nativeFunctionId = NativeFunctionId(73)
        )
        val fnPrefixVar = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup.variadic(
              Vector(FormalParameter(Name("head"), intT)),
              VariadicParameter(Name("rest"), stringT)
            )
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(79)
        )

        val candidates = Vector(fnPureVar, fnPrefixVar)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT, stringT)))

        result shouldBe Right(fnPrefixVar)
      }

      it("should reject variadic when vararg types are incompatible") {
        val fnVar = NativeFunctionDefinition(
          paramGroups = Vector(
            ParameterGroup.variadic(
              Vector(FormalParameter(Name("first"), intT)),
              VariadicParameter(Name("rest"), stringT)
            )
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(83)
        )

        val candidates = Vector(fnVar)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT, intT)))

        result shouldBe Left(ResolutionError.NotFound)
      }
    }

    describe("Ambiguity detection") {
      it("should report Ambiguous for symmetric overloads") {
        val fn1 = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(
            FormalParameter(Name("a"), intT),
            FormalParameter(Name("b"), anyT)
          ),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(89)
        )
        val fn2 = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(
            FormalParameter(Name("a"), anyT),
            FormalParameter(Name("b"), intT)
          ),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(97)
        )

        val candidates = Vector(fn1, fn2)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT, intT)))

        result shouldBe Left(ResolutionError.Ambiguous)
      }
    }

    describe("Conversion costs") {
      it("should select lowest-cost conversion candidate") {
        val fnLong = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), CoreTypes.LongT)),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(101)
        )
        val fnDouble = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), CoreTypes.DoubleT)),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(103)
        )

        // Int -> Long has cost 1; Int -> Double has cost 3
        val candidates = Vector(fnLong, fnDouble)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(CoreTypes.IntT)))

        result shouldBe Right(fnLong)
      }

      it("should prefer exact subtype match over any conversion match") {
        val fnNumber = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), numberT)),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(107)
        )
        val fnDouble = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("x"), CoreTypes.DoubleT)),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(109)
        )

        val candidates = Vector(fnNumber, fnDouble)
        val result = OverloadResolver.resolveFunctionCall(universe, candidates, Vector(Vector(intT)))

        result shouldBe Right(fnNumber)
      }
    }

    describe("Method resolution API") {
      it("should prepend receiver type to the first parameter group") {
        val fnMethod = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(
            FormalParameter(Name("self"), baseT),
            FormalParameter(Name("arg"), intT)
          ),
          returnType = stringT,
          pure = true,
          nativeFunctionId = NativeFunctionId(113)
        )

        val candidates = Vector(fnMethod)
        val result = OverloadResolver.resolveMethodCall(universe, candidates, derivedT, Vector(Vector(intT)))

        result shouldBe Right(fnMethod)
      }

      it("should resolve nullary method call with receiver only") {
        val fnNullaryMethod = NativeFunctionDefinition(
          paramGroups = ParameterGroup.single(FormalParameter(Name("self"), baseT)),
          returnType = intT,
          pure = true,
          nativeFunctionId = NativeFunctionId(127)
        )

        val candidates = Vector(fnNullaryMethod)
        val result = OverloadResolver.resolveMethodCall(universe, candidates, derivedT, Vector.empty)

        result shouldBe Right(fnNullaryMethod)
      }
    }
  }
}

object OverloadDisambiguationSpec {
  object Fixtures {
    val ns: PackagePath.Absolute = Packages.scaletta
    val registry: TypeRegistryImpl = new TypeRegistryImpl()

    val anyT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("Any")), RuntimeTypeInfo.any)
    val numberT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("Number")), RuntimeTypeInfo.any)
    val intT: Type.Nominal[TypeId] = registry.addValueType(ns.qualify(Name("Int")), RuntimeTypeInfo.any)
    val stringT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("String")), RuntimeTypeInfo.any)

    val baseT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("Base")), RuntimeTypeInfo.any)
    val derivedT: Type.Nominal[TypeId] = registry.addRefType(ns.qualify(Name("Derived")), RuntimeTypeInfo.any)

    registry.addRelationship(anyT, numberT)
    registry.addRelationship(numberT, intT)
    registry.addRelationship(anyT, stringT)
    registry.addRelationship(anyT, baseT)
    registry.addRelationship(baseT, derivedT)

    val universe: TypeUniverse = registry.build()
  }
}
