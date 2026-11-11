package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, Pos}

final class SemanticDiagnosticsSpec extends AnyFunSpec with Matchers {

  private val lineMap: LineMap = LineMap.create()
  private val typeInt: ProperType[TypeId] = Type.nominal(TypeId(1))
  private val typeString: ProperType[TypeId] = Type.nominal(TypeId(3))
  private val typeBoolean: ProperType[TypeId] = Type.nominal(TypeId(5))

  private def createPosIdent(error: SemanticError, begin: Int = 0, end: Int = 5): Pos[LinkedPhase.Ident] =
    Pos(
      LinkedInfo.Error(error),
      CharIndex(begin),
      CharIndex(end)
    )

  describe("SemanticDiagnostics") {
    describe("error message formatting") {
      it("should format ForwardReference error") {
        val expr = LinkedPhase.Reference[Pos](createPosIdent(SemanticError.ForwardReference("count")))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Forward reference to 'count' is not allowed"
        diagnostics.head.begin shouldBe Position.of(1, 1)
        diagnostics.head.end shouldBe Position.of(1, 6)
      }

      it("should format RecursiveReference error") {
        val expr = LinkedPhase.Reference[Pos](createPosIdent(SemanticError.RecursiveReference("factorial"), begin = 2, end = 11))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Recursive reference to 'factorial' is not allowed"
        diagnostics.head.begin shouldBe Position.of(1, 3)
        diagnostics.head.end shouldBe Position.of(1, 12)
      }

      it("should format UnknownIdentifier error") {
        val expr = LinkedPhase.Reference[Pos](createPosIdent(SemanticError.UnknownIdentifier("missingVar")))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Unknown identifier 'missingVar'"
      }

      it("should format DuplicateDefinition error") {
        val expr = LinkedPhase.Reference[Pos](createPosIdent(SemanticError.DuplicateDefinition("x")))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Duplicate definition of 'x'"
      }

      it("should format TypeMismatch error") {
        val error = SemanticError.TypeMismatch(expected = typeInt, actual = typeString)
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Type mismatch: expected TypeId(1), found TypeId(3)"
      }

      it("should format ConditionNotBoolean error") {
        val error = SemanticError.ConditionNotBoolean(actual = typeInt)
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Condition expression must be of type Boolean, but found TypeId(1)"
      }

      it("should format NoSuchMethod error with arguments") {
        val error = SemanticError.NoSuchMethod(
          receiverType = typeInt,
          name = "plus",
          argTypes = Vector(typeString, typeBoolean)
        )
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "No matching method 'plus' on type TypeId(1) with arguments (TypeId(3), TypeId(5))"
      }

      it("should format NoSuchFunction error") {
        val error = SemanticError.NoSuchFunction(
          name = "compute",
          argTypes = Vector(typeInt)
        )
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "No matching function 'compute' with arguments (TypeId(1))"
      }

      it("should format AmbiguousCall error") {
        val error = SemanticError.AmbiguousCall(name = "process", candidateCount = 3)
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Ambiguous call to 'process' (3 candidates)"
      }

      it("should format NoSuchField error") {
        val error = SemanticError.NoSuchField(receiverType = typeString, name = "length")
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "No field 'length' found on type TypeId(3)"
      }

      it("should format NotCallable error") {
        val error = SemanticError.NotCallable(targetType = typeInt)
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Expression of type TypeId(1) cannot be called as a function"
      }

      it("should format RefutableValPattern error") {
        val error = SemanticError.RefutableValPattern("Some(x) may fail for None")
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Pattern in val definition must be irrefutable: Some(x) may fail for None"
      }

      it("should format InexhaustiveMatch error") {
        val error = SemanticError.InexhaustiveMatch("missing case: None")
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Match expression is not exhaustive: missing case: None"
      }

      it("should format UnreachableCase error") {
        val error = SemanticError.UnreachableCase()
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Unreachable case pattern in match expression"
      }

      it("should format UnsatisfiedContextRequirement error") {
        val error = SemanticError.UnsatisfiedContextRequirement(TypeId(47))
        val expr = LinkedPhase.Reference[Pos](createPosIdent(error))
        val diagnostics = SemanticDiagnostics.collect(expr, lineMap)

        diagnostics should have size 1
        diagnostics.head.message shouldBe "Required runtime context 'TypeId(47)' is not satisfied"
      }
    }

    describe("complex type formatting in diagnostics") {
      it("should format applied types, functions, unions, and tuples") {
        val param = TypeParameter.invariant[TypeId]
        val appliedType = Type.applied(Type.nominal(TypeId(7)), TypeArgument(param, typeInt))
        val fnType = Type.function(typeInt, typeString)(typeBoolean)
        val tupleType = Type.tuple(typeInt, typeString)
        val unionType = Type.union(typeInt, typeString)
        val intersectionType = Type.intersection(typeInt, typeString)

        val errorApplied = SemanticError.TypeMismatch(appliedType, fnType)
        val diagApplied = SemanticDiagnostics.collect(LinkedPhase.Reference[Pos](createPosIdent(errorApplied)), lineMap)
        diagApplied.head.message shouldBe "Type mismatch: expected TypeId(7)[TypeId(1)], found (TypeId(1), TypeId(3)) => TypeId(5)"

        val errorTuple = SemanticError.TypeMismatch(tupleType, unionType)
        val diagTuple = SemanticDiagnostics.collect(LinkedPhase.Reference[Pos](createPosIdent(errorTuple)), lineMap)
        diagTuple.head.message should (include("TypeId(1)") and include("TypeId(3)"))

        val errorSpecial = SemanticError.TypeMismatch(Type.unit, Type.bottom)
        val diagSpecial = SemanticDiagnostics.collect(LinkedPhase.Reference[Pos](createPosIdent(errorSpecial)), lineMap)
        diagSpecial.head.message shouldBe "Type mismatch: expected Unit, found Nothing"
      }
    }
  }
}
