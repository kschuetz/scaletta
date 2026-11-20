package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.ast.{LinkedInfo, LinkedPhase}
import software.kes.scaletta.internal.desugar.Desugarer
import software.kes.scaletta.internal.library.standard.StandardTypes
import software.kes.scaletta.internal.parser.Parser
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}
import software.kes.scaletta.internal.semantics.LocalBindingKind.PatternBinding

final class PatternCheckerSpec extends AnyFunSpec with Matchers {
  private val scaletta: ScalettaFacade = Scaletta.create().asInstanceOf[ScalettaFacade]
  private val checker: PatternChecker = PatternChecker.create(scaletta.universe.typeUniverse)
  private val zero: CharIndex = CharIndex(0)

  private def pos[A](value: A): Pos[A] = Pos(value, zero)

  private def local(name: String): Pos[LinkedPhase.Pattern[Pos]] =
    pos(LinkedPhase.Pattern.Identifier[Pos](pos(LinkedInfo.Local(0, 0, name, PatternBinding))))

  private def wildcard: Pos[LinkedPhase.Pattern[Pos]] =
    pos(LinkedPhase.Pattern.Wildcard[Pos]())

  private def literalInt(value: Int): Pos[LinkedPhase.Pattern[Pos]] =
    pos(LinkedPhase.Pattern.Literal[Pos](pos(LinkedPhase.Literal.IntLiteral[Pos](value))))

  private def literalBoolean(value: Boolean): Pos[LinkedPhase.Pattern[Pos]] = {
    val literal: LinkedPhase.Literal[Pos] = if (value) {
      LinkedPhase.Literal.True[Pos]()
    } else {
      LinkedPhase.Literal.False[Pos]()
    }
    pos(LinkedPhase.Pattern.Literal[Pos](pos(literal)))
  }

  private def typed(inner: Pos[LinkedPhase.Pattern[Pos]], target: ProperType[TypeId]): Pos[LinkedPhase.Pattern[Pos]] =
    pos(LinkedPhase.Pattern.Typed[Pos](inner, pos(Right(target))))

  private def clause(
                      pattern: Pos[LinkedPhase.Pattern[Pos]],
                      guard: Option[Pos[LinkedPhase.Expression[Pos]]] = None
                    ): Pos[LinkedPhase.Case[Pos]] =
    pos(LinkedPhase.Case[Pos](pattern, guard, pos(LinkedPhase.Literal.UnitLiteral[Pos]())))

  private def linkedCases(source: String): Vector[Pos[LinkedPhase.Case[Pos]]] = {
    val reader = SourceReader.create(source.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parseResult = Parser.create().parse(scanner)
    withClue(s"Failed to parse: $source. ") {
      parseResult.diagnostics.hasErrors shouldBe false
    }
    val parsed = parseResult.value.getOrElse(fail(s"Failed to parse: $source"))
    val desugared = Desugarer.create().desugar(parsed.value)
    val linked = Linker.create(scaletta.universe, ImportScope.default).link(desugared)
    linked match {
      case LinkedPhase.Match(_, cases) => cases
      case other => fail(s"Expected a match expression, got $other")
    }
  }

  private def optionType(argument: ProperType[TypeId]): ProperType[TypeId] = {
    val optionName = Packages.scaletta.qualify(StandardTypes.names.OptionT)
    scaletta.universe.typeUniverse.nameIndex.get(optionName) match {
      case Some(constructor: Type.Constructor[TypeId]) => TypeApplier.fromNode(constructor).applyAll(argument)
      case other => fail(s"Expected Option type constructor, got $other")
    }
  }

  private def nonePattern: Pos[LinkedPhase.Pattern[Pos]] = {
    val noneName = Packages.scaletta.qualify(StandardTypes.names.NoneT)
    val noneType = scaletta.universe.typeUniverse.nameIndex.get(noneName) match {
      case Some(singleton: Type.Singleton[TypeId]) => singleton
      case other => fail(s"Expected None singleton type, got $other")
    }
    pos(LinkedPhase.Pattern.Product[Pos](pos(Right(noneType)), Vector.empty))
  }

  describe("PatternChecker") {
    describe("pattern typing") {
      it("propagates tuple element types and as-pattern bindings") {
        val tupleType = Type.tuple(CoreTypes.IntT, CoreTypes.StringT)
        val tuplePattern: Pos[LinkedPhase.Pattern[Pos]] =
          pos(LinkedPhase.Pattern.Tuple[Pos](Vector(local("left"), local("right"))))
        checker.typePattern(tuplePattern, tupleType) shouldBe Right(Map(
          "left" -> CoreTypes.IntT,
          "right" -> CoreTypes.StringT
        ))

        val asPattern: Pos[LinkedPhase.Pattern[Pos]] = pos(LinkedPhase.Pattern.As[Pos](
          pos(LinkedInfo.Local(0, 0, "whole", PatternBinding)),
          tuplePattern
        ))
        checker.typePattern(asPattern, tupleType) shouldBe Right(Map(
          "whole" -> tupleType,
          "left" -> CoreTypes.IntT,
          "right" -> CoreTypes.StringT
        ))
      }

      it("rejects tuple patterns when one union alternative is not a tuple") {
        val tupleType = Type.tuple(CoreTypes.IntT, CoreTypes.StringT)
        val targetType = Type.union(tupleType, CoreTypes.IntT)
        val pattern: Pos[LinkedPhase.Pattern[Pos]] =
          pos(LinkedPhase.Pattern.Tuple[Pos](Vector(local("left"), local("right"))))

        checker.typePattern(pattern, targetType) shouldBe a[Left[_, _]]
      }

      it("narrows typed patterns and rejects disjoint patterns") {
        val pattern = typed(local("value"), CoreTypes.IntT)
        checker.typePattern(pattern, Type.Top) shouldBe Right(Map("value" -> CoreTypes.IntT))
        checker.typePattern(typed(local("value"), CoreTypes.StringT), CoreTypes.IntT) shouldBe
          Left(SemanticError.TypeMismatch(CoreTypes.IntT, CoreTypes.StringT))

        checker.typePattern(pos(LinkedPhase.Pattern.Literal[Pos](
          pos(LinkedPhase.Literal.IntLiteral[Pos](41)))), CoreTypes.StringT) shouldBe
          Left(SemanticError.TypeMismatch(CoreTypes.StringT, CoreTypes.IntT))
      }
    }

    describe("declaration irrefutability") {
      it("accepts tuple and as patterns when their elements are total") {
        val tupleType = Type.tuple(CoreTypes.IntT, CoreTypes.StringT)
        val tuplePattern: Pos[LinkedPhase.Pattern[Pos]] =
          pos(LinkedPhase.Pattern.Tuple[Pos](Vector(local("left"), local("right"))))
        checker.checkValPatternIrrefutable(tuplePattern, tupleType) shouldBe None

        val asPattern: Pos[LinkedPhase.Pattern[Pos]] = pos(LinkedPhase.Pattern.As[Pos](
          pos(LinkedInfo.Local(0, 0, "whole", PatternBinding)), tuplePattern
        ))
        checker.checkValPatternIrrefutable(asPattern, tupleType) shouldBe None
      }

      it("rejects literal and partial typed patterns") {
        checker.checkValPatternIrrefutable(literalInt(41), CoreTypes.IntT) shouldBe
          Some(SemanticError.RefutableValPattern("Pattern is refutable for type " + CoreTypes.IntT))
        checker.checkValPatternIrrefutable(typed(local("value"), CoreTypes.IntT), Type.Top) should not be empty
      }

      it("rejects a constructor pattern on Option") {
        val cases = linkedCases("None match { case Some(value) => 1; case _ => 0 }")
        checker.checkValPatternIrrefutable(cases.head.value.pattern, optionType(CoreTypes.IntT)) should not be empty
      }

      it("propagates generic product field types") {
        val pattern = linkedCases("None match { case Some(value) => 1 }").head.value.pattern
        withClue(s"Linked pattern: ${pattern.value}. ") {
          checker.typePattern(pattern, optionType(CoreTypes.IntT)) shouldBe Right(Map("value" -> CoreTypes.IntT))
        }
      }
    }

    describe("match coverage") {
      it("recognizes both Boolean literals and reports the missing literal") {
        checker.checkMatchExhaustiveness(CoreTypes.BooleanT,
          Vector(clause(literalBoolean(true)), clause(literalBoolean(false)))) shouldBe empty
        checker.checkMatchExhaustiveness(CoreTypes.BooleanT,
          Vector(clause(literalBoolean(true)))) should contain(SemanticError.InexhaustiveMatch("missing: false"))
      }

      it("uses typed slices to cover a union of types") {
        val cases = Vector(
          clause(typed(wildcard, CoreTypes.IntT)),
          clause(typed(wildcard, CoreTypes.StringT))
        )
        checker.checkMatchExhaustiveness(Type.union(CoreTypes.IntT, CoreTypes.StringT), cases) shouldBe empty
      }

      it("covers Option constructors and reports a missing constructor") {
        val someCase = linkedCases("None match { case Some(_) => 1 }").head
        val complete = Vector(someCase, clause(nonePattern))
        withClue(s"Linked patterns: ${complete.map(_.value.pattern.value)}. ") {
          checker.checkMatchExhaustiveness(optionType(CoreTypes.IntT), complete) shouldBe empty
        }

        val incomplete = linkedCases("None match { case Some(_) => 1 }")
        checker.checkMatchExhaustiveness(optionType(CoreTypes.IntT), incomplete) should contain(
          SemanticError.InexhaustiveMatch("missing: None")
        )
      }

      it("preserves nested constructor-field coverage") {
        val someCases = linkedCases("None match { case Some(true) => 1; case Some(false) => 2 }")
        val cases = someCases :+ clause(nonePattern)
        checker.checkMatchExhaustiveness(optionType(CoreTypes.BooleanT), cases) shouldBe empty
      }

      it("reports redundant rows and does not let guarded rows cover") {
        checker.checkMatchExhaustiveness(CoreTypes.IntT,
          Vector(clause(wildcard), clause(literalInt(43)))) should contain(SemanticError.UnreachableCase())

        checker.checkMatchExhaustiveness(CoreTypes.IntT,
          Vector(clause(typed(wildcard, CoreTypes.IntT)), clause(literalInt(43)))) should contain(
          SemanticError.UnreachableCase()
        )

        val guard: Pos[LinkedPhase.Expression[Pos]] = pos(LinkedPhase.Literal.True[Pos]())
        checker.checkMatchExhaustiveness(CoreTypes.BooleanT,
          Vector(clause(literalBoolean(true), Some(guard)), clause(literalBoolean(false)))) should contain(
          SemanticError.InexhaustiveMatch("missing: true")
        )

        checker.checkMatchExhaustiveness(CoreTypes.IntT,
          Vector(clause(wildcard, Some(guard)), clause(wildcard))) shouldBe empty
      }
    }
  }
}
