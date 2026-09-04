package software.kes.scaletta.internal.preresolver

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.parser.Parser
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}

final class PreResolverSpec extends AnyFunSpec with Matchers {

  private val nsMath: PackagePath.Absolute = Packages.scalettaMath
  private val nsUtil: PackagePath.Absolute = PackagePath.parseAbsolute("scaletta.util")

  private val utilModule: ScalettaModule[Unit] = ScalettaModule { setup =>
    setup.methodRegistry.addMethod(
      MethodName(ReceiverType.Static(nsUtil), Name("sqrt")),
      Vector(FormalParameter.double(Name("x"))),
      CoreTypes.DoubleT,
      FunctionImpl.doubleResult(args => args.readDouble(0))
    )
  }

  private val scaletta: ScalettaFacade =
    Scaletta.create(Scaletta.addModule(utilModule)).asInstanceOf[ScalettaFacade]
  private val stdLib: StandardLibraryLookup = StandardLibraryLookup.create(scaletta.universe)
  private val sqrtId: NativeFunctionSymbolId = NativeFunctionSymbolId(stdLib.math.sqrt)

  private def parseExpr(source: String): Pos[ParsingPhase.Expression[Pos]] = {
    val reader = SourceReader.create(source.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    val result = parser.parse(scanner)
    result.value.getOrElse(fail(s"Failed to parse: $source"))
  }

  private def preResolve(source: String, importScope: ImportScope = ImportScope.default): PreResolutionPhase.Expression[Pos] = {
    val parsed = parseExpr(source)
    val resolver = PreResolver.create(scaletta.universe.methodUniverse.symbolTable, importScope)
    resolver.preResolve(parsed.value)
  }

  describe("PreResolver") {
    describe("literals and primitives") {
      it("should convert literal expressions") {
        preResolve("41") shouldBe PreResolutionPhase.Literal.IntLiteral[Pos](41)
        preResolve("true") shouldBe PreResolutionPhase.Literal.True[Pos]()
        preResolve("false") shouldBe PreResolutionPhase.Literal.False[Pos]()
        preResolve("null") shouldBe PreResolutionPhase.Literal.Null[Pos]()
        preResolve("\"hello\"") shouldBe PreResolutionPhase.Literal.StringLiteral[Pos]("hello")
        preResolve("()") shouldBe PreResolutionPhase.Literal.UnitLiteral[Pos]()
      }

      it("should convert conditionals and tuples") {
        val cond = preResolve("if (true) 41 else 43")
        cond should matchPattern { case _: PreResolutionPhase.Conditional[_] => }

        val p1: Pos[ParsingPhase.Expression[Pos]] = Pos(ParsingPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val p2: Pos[ParsingPhase.Expression[Pos]] = Pos(ParsingPhase.Literal.true_[Pos](), CharIndex(3), CharIndex(7))
        val rawTuple = ParsingPhase.Tuple[Pos](Vector(p1, p2))
        val resolver = PreResolver.create(scaletta.universe.methodUniverse.symbolTable, ImportScope.default)
        val resTuple = resolver.preResolve(rawTuple)
        resTuple should matchPattern { case _: PreResolutionPhase.Tuple[_] => }
      }
    }

    describe("lexical scope and local bindings") {
      it("should resolve locally bound val references in blocks") {
        val ast = preResolve("{ val x = 41; x }")
        ast match {
          case PreResolutionPhase.Block(decls, result) =>
            decls.size shouldBe 1
            result.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Unresolved("x")
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should treat forward references in blocks as not in local scope") {
        val ast = preResolve("{ val x = y; val y = 41; x }")
        ast match {
          case PreResolutionPhase.Block(decls, result) =>
            decls.size shouldBe 2
            decls.head.value match {
              case PreResolutionPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case PreResolutionPhase.Reference(id) =>
                    // y was not in scope when x was declared
                    id.value.value shouldBe PreResolutionInfo.Unresolved("y")
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should track lambda parameters in lexical scope") {
        val pName = Pos(Identifier[Pos]("x"), CharIndex(1), CharIndex(2))
        val param = Pos(ParsingPhase.LambdaParameter[Pos](pName, None), CharIndex(1), CharIndex(2))
        val bodyIdent = Pos(Identifier[Pos]("x"), CharIndex(6), CharIndex(7))
        val body = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](bodyIdent), CharIndex(6), CharIndex(7))
        val rawLambda = ParsingPhase.Lambda[Pos](Vector(param), body)

        val resolver = PreResolver.create(scaletta.universe.methodUniverse.symbolTable, ImportScope.default)
        val ast = resolver.preResolve(rawLambda)

        ast match {
          case PreResolutionPhase.Lambda(params, b) =>
            params.size shouldBe 1
            params.head.value.name.value.value shouldBe PreResolutionInfo.Unresolved("x")
            b.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Unresolved("x")
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Lambda, got $other")
        }
      }

      it("should track def parameters in lexical scope") {
        val ast = preResolve("{ def fn(a: Int) = a; fn }")
        ast match {
          case PreResolutionPhase.Block(decls, result) =>
            decls.size shouldBe 1
            decls.head.value match {
              case PreResolutionPhase.Declaration.Def(name, params, _, body) =>
                name.value.value shouldBe PreResolutionInfo.Unresolved("fn")
                body.value match {
                  case PreResolutionPhase.Reference(id) =>
                    id.value.value shouldBe PreResolutionInfo.Unresolved("a")
                  case other => fail(s"Expected Reference in def body, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
            result.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Unresolved("fn")
              case other => fail(s"Expected Reference for result, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should track match pattern bindings in case body and guard") {
        val patIdent = Pos(Identifier[Pos]("x"), CharIndex(10), CharIndex(11))
        val pat = Pos[ParsingPhase.Pattern[Pos]](ParsingPhase.Pattern.Identifier[Pos](patIdent), CharIndex(10), CharIndex(11))

        val guardRef = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("x"), CharIndex(15), CharIndex(16))), CharIndex(15), CharIndex(16))
        val bodyRef = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("x"), CharIndex(20), CharIndex(21))), CharIndex(20), CharIndex(21))

        val kase = Pos(ParsingPhase.Case[Pos](pat, Some(guardRef), bodyRef), CharIndex(10), CharIndex(21))
        val targetExpr = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = ParsingPhase.Match[Pos](targetExpr, Vector(kase))

        val resolver = PreResolver.create(scaletta.universe.methodUniverse.symbolTable, ImportScope.default)
        val ast = resolver.preResolve(rawMatch)

        ast match {
          case PreResolutionPhase.Match(_, cases) =>
            cases.size shouldBe 1
            val c = cases.head.value
            c.guard shouldBe defined
            c.guard.get.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Unresolved("x")
              case other => fail(s"Expected Reference in guard, got $other")
            }
            c.body.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Unresolved("x")
              case other => fail(s"Expected Reference in case body, got $other")
            }
          case other => fail(s"Expected Match, got $other")
        }
      }
    }

    describe("static functions and imports") {
      it("should resolve single unambiguous static function to Bound") {
        val scope = ImportScope.empty.importWildcard(nsMath)
        val ast = preResolve("sqrt(41.0)", scope)
        ast match {
          case PreResolutionPhase.Call.Standard(target, _, args) =>
            args.size shouldBe 1
            target.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Bound(sqrtId)
              case other => fail(s"Expected Reference target, got $other")
            }
          case other => fail(s"Expected Standard call, got $other")
        }
      }

      it("should resolve overloaded static function to StaticFunctionSite") {
        val scope = ImportScope.empty.importWildcard(nsMath)
        val ast = preResolve("min(41, 43)", scope)
        ast match {
          case PreResolutionPhase.Call.Standard(target, _, args) =>
            args.size shouldBe 1
            target.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.StaticFunctionSite("scaletta.math.min", Vector(2))
              case other => fail(s"Expected Reference target, got $other")
            }
          case other => fail(s"Expected Standard call, got $other")
        }
      }

      it("should tag ambiguous imported functions with AmbiguousName") {
        val scope = ImportScope.empty
          .importWildcard(nsMath)
          .importWildcard(nsUtil)
        val ast = preResolve("sqrt(41.0)", scope)
        ast match {
          case PreResolutionPhase.Call.Standard(target, _, _) =>
            target.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.AmbiguousName("sqrt", 2)
              case other => fail(s"Expected Reference target, got $other")
            }
          case other => fail(s"Expected Standard call, got $other")
        }
      }

      it("should resolve fully qualified static function call") {
        val ast = preResolve("scaletta.math.sqrt(41.0)", ImportScope.empty)
        ast match {
          case PreResolutionPhase.Call.Standard(target, _, _) =>
            target.value match {
              case PreResolutionPhase.Select(_, name) =>
                name.value.value shouldBe PreResolutionInfo.Bound(sqrtId)
              case other => fail(s"Expected Select target, got $other")
            }
          case other => fail(s"Expected Standard call, got $other")
        }
      }

      it("should tag unknown names as Unresolved") {
        val ast = preResolve("nonExistent(41)", ImportScope.empty)
        ast match {
          case PreResolutionPhase.Call.Standard(target, _, _) =>
            target.value match {
              case PreResolutionPhase.Reference(id) =>
                id.value.value shouldBe PreResolutionInfo.Unresolved("nonExistent")
              case other => fail(s"Expected Reference target, got $other")
            }
          case other => fail(s"Expected Standard call, got $other")
        }
      }
    }

    describe("methods and field selections") {
      it("should tag infix operations as MethodSite with arity 1") {
        val ast = preResolve("1 + 2")
        ast match {
          case PreResolutionPhase.Call.Infix(left, op, _, right) =>
            op.value.value shouldBe PreResolutionInfo.MethodSite("+", Vector(1))
          case other => fail(s"Expected Infix call, got $other")
        }
      }

      it("should tag receiver member calls as MethodSite with matching arity") {
        val ast = preResolve("calculator.add(41, 43)")
        ast match {
          case PreResolutionPhase.Call.Standard(target, _, _) =>
            target.value match {
              case PreResolutionPhase.Select(_, name) =>
                name.value.value shouldBe PreResolutionInfo.MethodSite("add", Vector(2))
              case other => fail(s"Expected Select target, got $other")
            }
          case other => fail(s"Expected Standard call, got $other")
        }
      }

      it("should tag standalone property selections as FieldSite") {
        val ast = preResolve("user.name")
        ast match {
          case PreResolutionPhase.Select(qualifier, name) =>
            name.value.value shouldBe PreResolutionInfo.FieldSite("name")
          case other => fail(s"Expected Select, got $other")
        }
      }
    }
  }
}
