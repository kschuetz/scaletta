package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.desugar.Desugarer
import software.kes.scaletta.internal.parser.Parser
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}

final class TypeCheckerSpec extends AnyFunSpec with Matchers {
  private val scaletta: ScalettaFacade = Scaletta.create().asInstanceOf[ScalettaFacade]

  private def typeCheck(source: String): TypeCheckingResult = {
    val reader = SourceReader.create(source.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parsed = Parser.create().parse(scanner).value.getOrElse(fail(s"Failed to parse: $source"))
    val desugared = Desugarer.create().desugar(parsed.value)
    val linker = Linker.create(scaletta.universe, ImportScope.default)
    val linked = linker.link(desugared)
    val typeChecker = TypeChecker.create(scaletta.universe)
    typeChecker.typeCheck(Pos(linked, parsed.begin, parsed.end))
  }

  describe("TypeChecker") {
    describe("literals and references") {
      it("attributes literal primitives with their exact types") {
        val rInt = typeCheck("41")
        rInt.hasErrors shouldBe false
        rInt.attributed.properType shouldBe CoreTypes.IntT

        val rStr = typeCheck("\"scaletta\"")
        rStr.hasErrors shouldBe false
        rStr.attributed.properType shouldBe CoreTypes.StringT

        val rBool = typeCheck("true")
        rBool.hasErrors shouldBe false
        rBool.attributed.properType shouldBe CoreTypes.BooleanT

        val rNull = typeCheck("null")
        rNull.hasErrors shouldBe false
        rNull.attributed.properType shouldBe CoreTypes.NullT

        val rUnit = typeCheck("()")
        rUnit.hasErrors shouldBe false
        rUnit.attributed.properType shouldBe CoreTypes.UnitT
      }

      it("attributes tuples and single elements") {
        val rTup = typeCheck("(41, \"scaletta\")")
        rTup.hasErrors shouldBe false
        rTup.attributed.properType shouldBe Type.tuple(CoreTypes.IntT, CoreTypes.StringT)
      }
    }

    describe("arithmetic and method overloads") {
      it("resolves infix arithmetic operators to MethodCall") {
        val res = typeCheck("41 + 43")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.IntT
        res.attributed.value match {
          case TypedPhase.Call.Infix(left, op, _, right) =>
            left.properType shouldBe CoreTypes.IntT
            right.properType shouldBe CoreTypes.IntT
            op.value shouldBe a[TypedInfo.MethodCall]
          case other => fail(s"Expected Infix Call, got $other")
        }
      }

      it("resolves comparison operators returning Boolean") {
        val res = typeCheck("41 < 43")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.BooleanT
      }

      it("records SemanticError on incompatible method arguments") {
        val res = typeCheck("41 + \"invalid\"")
        res.hasErrors shouldBe true
        res.errors.exists(_.value.isInstanceOf[SemanticError.NoSuchMethod]) shouldBe true
      }
    }

    describe("type ascription") {
      it("validates successful ascription") {
        val res = typeCheck("41: Any")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.AnyT
      }

      it("reports TypeMismatch on invalid ascription") {
        val res = typeCheck("41: String")
        res.hasErrors shouldBe true
        res.errors.exists { err =>
          err.value match {
            case SemanticError.TypeMismatch(expected, actual) =>
              expected == CoreTypes.StringT && actual == CoreTypes.IntT
            case _ => false
          }
        } shouldBe true
      }
    }

    describe("block declarations and lexical scoping") {
      it("propagates inferred types sequentially across val declarations") {
        val res = typeCheck("{ val a = 41; val b = a + 2; b }")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.IntT
      }

      it("binds variables from tuple destructuring in val declarations") {
        val res = typeCheck("{ val (x, y) = (41, \"hello\"); x }")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.IntT
      }

      it("reports RefutableValPattern when val pattern is not irrefutable") {
        val res = typeCheck("{ val (x, 41) = (43, 47); x }")
        res.hasErrors shouldBe true
        res.errors.exists(_.value.isInstanceOf[SemanticError.RefutableValPattern]) shouldBe true
      }

      it("supports local def declarations with explicit return types") {
        val res = typeCheck("{ def add(x: Int, y: Int): Int = x + y; add(41, 43) }")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.IntT
      }

      it("reports TypeMismatch when def body does not match declared return type") {
        val res = typeCheck("{ def bad(): String = 41; bad() }")
        res.hasErrors shouldBe true
        res.errors.exists(_.value.isInstanceOf[SemanticError.TypeMismatch]) shouldBe true
      }
    }

    describe("conditional expressions and branch unification") {
      it("unifies identical branch types") {
        val res = typeCheck("if (true) 41 else 43")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.IntT
      }

      it("computes least upper bound for diverse branch types") {
        val res = typeCheck("if (true) 41 else \"scaletta\"")
        res.hasErrors shouldBe false
        val lub = scaletta.universe.typeUniverse.hierarchy.leastUpperBound(CoreTypes.IntT, CoreTypes.StringT)
        res.attributed.properType shouldBe lub
      }

      it("reports ConditionNotBoolean when condition is not boolean") {
        val res = typeCheck("if (41) \"yes\" else \"no\"")
        res.hasErrors shouldBe true
        res.errors.exists(_.value.isInstanceOf[SemanticError.ConditionNotBoolean]) shouldBe true
      }
    }

    describe("lambda expressions") {
      it("types lambda parameters and body") {
        val res = typeCheck("(x: Int) => x + 1")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe Type.Function(Vector(CoreTypes.IntT), CoreTypes.IntT)
      }
    }

    describe("interpolated strings") {
      // TODO: implement parsing for interpolated strings
      ignore("attributes interpolated string as String type") {
        val res = typeCheck("s\"value is ${41}\"")
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.StringT
      }
    }

    describe("match expressions") {
      it("unifies case body types in exhaustive matches") {
        val res = typeCheck(
          """(41, "test") match {
            |  case (x, y) => x
            |}""".stripMargin
        )
        res.hasErrors shouldBe false
        res.attributed.properType shouldBe CoreTypes.IntT
      }

      it("reports InexhaustiveMatch when match is not exhaustive") {
        val res = typeCheck(
          """41 match {
            |  case 43 => "forty-three"
            |}""".stripMargin
        )
        res.hasErrors shouldBe true
        res.errors.exists(_.value.isInstanceOf[SemanticError.InexhaustiveMatch]) shouldBe true
      }

      it("reports UnreachableCase when a case is redundant") {
        val res = typeCheck(
          """41 match {
            |  case _ => "first"
            |  case x => "second"
            |}""".stripMargin
        )
        res.hasErrors shouldBe true
        res.errors.exists(_.value.isInstanceOf[SemanticError.UnreachableCase]) shouldBe true
      }
    }
  }
}
