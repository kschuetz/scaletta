package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.parser.Parser
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}
import software.kes.scaletta.internal.types.TypeResolutionError

final class TypeAscriberSpec extends AnyFunSpec with Matchers {

  private val scaletta: ScalettaFacade =
    Scaletta.create().asInstanceOf[ScalettaFacade]

  describe("TypeAscriber") {
    describe("simple named types") {
      it("should resolve a known nominal type by name") {
        val ast = ascribe("41: Int")
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe Right(CoreTypes.IntT)
          case other => fail(s"Expected Typed, got $other")
        }
      }

      it("should report an unknown type name") {
        val ast = ascribe("41: DoesNotExist")
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe Left(TypeResolutionError.UnknownType("DoesNotExist"))
          case other => fail(s"Expected Typed, got $other")
        }
      }
    }

    describe("compound types") {
      it("should resolve tuple type ascriptions") {
        val ascriber = TypeAscriber.create(scaletta.universe.typeUniverse.nameIndex, ImportScope.default)
        val tupleTypeIdent: TypeIdentifier[Pos] =
          TypeIdentifier.tuple[Pos](Vector(typeName("Int"), typeName("String")))
        val target = Pos[PreResolutionPhase.Expression[Pos]](PreResolutionPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val typedExpr = PreResolutionPhase.Typed[Pos](target, Pos(tupleTypeIdent, CharIndex(4), CharIndex(20)))
        val ast = ascriber.ascribeTypes(typedExpr)
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe Right(Type.tuple(CoreTypes.IntT, CoreTypes.StringT))
          case other => fail(s"Expected Typed, got $other")
        }
      }

      it("should resolve union type ascriptions") {
        val ast = ascribe("41: Int | String")
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe Right(Type.union(CoreTypes.IntT, CoreTypes.StringT))
          case other => fail(s"Expected Typed, got $other")
        }
      }

      it("should propagate an error from a nested type component") {
        val ast = ascribe("41: (Int, DoesNotExist)")
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe Left(TypeResolutionError.UnknownType("DoesNotExist"))
          case other => fail(s"Expected Typed, got $other")
        }
      }
    }

    describe("applied generic types") {
      it("should resolve a type constructor applied to its type arguments") {
        val ast = ascribe("41: List[Int]")
        val stdLib = scaletta.universe.typeUniverse.nameIndex
        val listT = stdLib.resolve(QualifiedName.local(Name("List")), ImportScope.default) match {
          case entry :: Nil => entry.value
          case other => fail(s"Expected a single List entry, got $other")
        }
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe Right(TypeApplier.fromNode(listT.asInstanceOf[Type.Constructor[TypeId]]).applyAll(CoreTypes.IntT))
          case other => fail(s"Expected Typed, got $other")
        }
      }

      it("should report wrong arity when applying too many type arguments") {
        val ast = ascribe("41: List[Int, String]")
        ast match {
          case TypeAscriptionPhase.Typed(_, ascription) =>
            typeOf(ascription.value) shouldBe a[Left[_, _]]
          case other => fail(s"Expected Typed, got $other")
        }
      }
    }

    describe("def parameters and return types") {
      it("should resolve formal parameter and return types in a def") {
        val ast = ascribe("{ def fn(a: Int): String = \"x\"; fn }")
        ast match {
          case TypeAscriptionPhase.Block(decls, _) =>
            decls.head.value match {
              case TypeAscriptionPhase.Declaration.Def(_, params, returnType, _) =>
                typeOf(params.head.value.parameters.head.value.typ.value) shouldBe Right(CoreTypes.IntT)
                returnType.map(rt => typeOf(rt.value)) shouldBe Some(Right(CoreTypes.StringT))
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }
  }

  private def parseExpr(source: String): Pos[ParsingPhase.Expression[Pos]] = {
    val reader = SourceReader.create(source.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    val result = parser.parse(scanner)
    result.value.getOrElse(fail(s"Failed to parse: $source"))
  }

  private def ascribe(source: String, importScope: ImportScope = ImportScope.default): TypeAscriptionPhase.Expression[Pos] = {
    val parsed = parseExpr(source)
    val preResolver = PreResolver.create(scaletta.universe.methodUniverse.symbolTable, importScope)
    val preResolved = preResolver.preResolve(parsed.value)
    val ascriber = TypeAscriber.create(scaletta.universe.typeUniverse.nameIndex, importScope)
    ascriber.ascribeTypes(preResolved)
  }

  private def typeOf(ta: TypeAscriptionPhase.TypeIdent[Pos]): Either[TypeResolutionError, ProperType[TypeId]] = ta.value

  private def typeName(name: String): Pos[TypeIdentifier[Pos]] =
    Pos(TypeIdentifier.name[Pos](Pos(Identifier[Pos](name), CharIndex(0), CharIndex(name.length))), CharIndex(0), CharIndex(name.length))

}
