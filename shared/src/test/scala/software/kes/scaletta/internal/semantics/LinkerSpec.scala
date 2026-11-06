package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.parser.Parser
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}

final class LinkerSpec extends AnyFunSpec with Matchers {

  private val scaletta: ScalettaFacade =
    Scaletta.create().asInstanceOf[ScalettaFacade]

  private def parseExpr(source: String): Pos[ParsingPhase.Expression[Pos]] = {
    val reader = SourceReader.create(source.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    val result = parser.parse(scanner)
    result.value.getOrElse(fail(s"Failed to parse: $source"))
  }

  private def link(source: String, importScope: ImportScope = ImportScope.default): LinkedPhase.Expression[Pos] = {
    val parsed = parseExpr(source)
    linkAst(parsed.value, importScope)
  }

  private def linkAst(parsed: ParsingPhase.Expression[Pos], importScope: ImportScope = ImportScope.default): LinkedPhase.Expression[Pos] = {
    val preResolver = PreResolver.create(scaletta.universe.methodUniverse.symbolTable, importScope)
    val preResolved = preResolver.preResolve(parsed)
    val ascriber = TypeAscriber.create(scaletta.universe.typeUniverse.nameIndex, importScope)
    val ascribed = ascriber.ascribeTypes(preResolved)
    val linker = Linker.create()
    linker.link(ascribed)
  }

  describe("Linker") {
    describe("literals and primitives") {
      it("should carry over literals unchanged") {
        link("41") shouldBe LinkedPhase.Literal.IntLiteral[Pos](41)
        link("true") shouldBe LinkedPhase.Literal.True[Pos]()
        link("\"hello\"") shouldBe LinkedPhase.Literal.StringLiteral[Pos]("hello")
      }
    }

    describe("local val bindings in blocks") {
      it("should resolve a single local val reference with slot 0 and depth 0") {
        val ast = link("{ val x = 41; x }")
        ast match {
          case LinkedPhase.Block(decls, result) =>
            decls.size shouldBe 1
            result.value match {
              case LinkedPhase.Reference(id) =>
                id.value.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Val)
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should assign sequential slot indices to multiple val declarations") {
        val ast = link("{ val a = 41; val b = 43; b }")
        ast match {
          case LinkedPhase.Block(decls, result) =>
            decls.size shouldBe 2
            decls.head.value match {
              case LinkedPhase.Declaration.Val(pattern, _) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Val)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
            decls(1).value match {
              case LinkedPhase.Declaration.Val(pattern, _) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Val)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
            result.value match {
              case LinkedPhase.Reference(id) =>
                id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Val)
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should resolve outer lexical variables with correct relative depth") {
        val ast = link("{ val x = 41; { val y = 43; x } }")
        ast match {
          case LinkedPhase.Block(_, result) =>
            result.value match {
              case LinkedPhase.Block(_, innerResult) =>
                innerResult.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value.value shouldBe LinkedInfo.Local(1, 0, "x", LocalBindingKind.Val)
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected inner Block, got $other")
            }
          case other => fail(s"Expected outer Block, got $other")
        }
      }
    }

    describe("forward references") {
      it("should flag a forward reference in a block as an error") {
        val ast = link("{ val x = y; val y = 41; x }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("y"))
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should report unknown identifier for references that are not declared") {
        val ast = link("{ val x = z; x }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value.value shouldBe LinkedInfo.Error(SemanticError.UnknownIdentifier("z"))
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }

    describe("duplicate definitions") {
      it("should flag duplicate val definitions in the same scope") {
        val ast = link("{ val x = 41; val x = 43; x }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls(1).value match {
              case LinkedPhase.Declaration.Val(pattern, _) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value.value shouldBe LinkedInfo.Error(SemanticError.DuplicateDefinition("x"))
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag duplicate parameters in a lambda AST") {
        val p1Name = Pos(Identifier[Pos]("x"), CharIndex(1), CharIndex(2))
        val p1 = Pos(ParsingPhase.LambdaParameter[Pos](p1Name, None), CharIndex(1), CharIndex(2))
        val p2Name = Pos(Identifier[Pos]("x"), CharIndex(3), CharIndex(4))
        val p2 = Pos(ParsingPhase.LambdaParameter[Pos](p2Name, None), CharIndex(3), CharIndex(4))
        val bodyIdent = Pos(Identifier[Pos]("x"), CharIndex(6), CharIndex(7))
        val body = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](bodyIdent), CharIndex(6), CharIndex(7))
        val rawLambda = ParsingPhase.Lambda[Pos](Vector(p1, p2), body)

        val ast = linkAst(rawLambda)
        ast match {
          case LinkedPhase.Lambda(params, _) =>
            params(1).value.name.value.value shouldBe LinkedInfo.Error(SemanticError.DuplicateDefinition("x"))
          case other => fail(s"Expected Lambda, got $other")
        }
      }
    }

    describe("lambdas and closures") {
      it("should resolve lambda parameters and outer captured variables in AST") {
        val pName = Pos(Identifier[Pos]("x"), CharIndex(1), CharIndex(2))
        val param = Pos(ParsingPhase.LambdaParameter[Pos](pName, None), CharIndex(1), CharIndex(2))
        val refOuter = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("outer"), CharIndex(5), CharIndex(10))), CharIndex(5), CharIndex(10))
        val refX = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("x"), CharIndex(12), CharIndex(13))), CharIndex(12), CharIndex(13))
        val tupleBody = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Tuple[Pos](Vector(refOuter, refX)), CharIndex(5), CharIndex(14))
        val rawLambda = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Lambda[Pos](Vector(param), tupleBody), CharIndex(0), CharIndex(15))

        val valDecl = Pos[ParsingPhase.Declaration[Pos]](
          ParsingPhase.Declaration.Val[Pos](
            Pos[ParsingPhase.Pattern[Pos]](ParsingPhase.Pattern.Identifier[Pos](Pos(Identifier[Pos]("outer"), CharIndex(0), CharIndex(5))), CharIndex(0), CharIndex(5)),
            Pos(ParsingPhase.Literal.int[Pos](41), CharIndex(6), CharIndex(8))
          ),
          CharIndex(0), CharIndex(8)
        )
        val rawBlock = ParsingPhase.Block[Pos](Vector(valDecl), rawLambda)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(_, result) =>
            result.value match {
              case LinkedPhase.Lambda(params, body) =>
                params.head.value.name.value.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Parameter)
                body.value match {
                  case LinkedPhase.Tuple(elements) =>
                    elements.head.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Local(1, 0, "outer", LocalBindingKind.Val)
                      case other => fail(s"Expected Reference, got $other")
                    }
                    elements(1).value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Parameter)
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Tuple, got $other")
                }
              case other => fail(s"Expected Lambda, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }

    describe("def declarations") {
      it("should resolve local def parameters and result calls") {
        val ast = link("{ def add(a: Int, b: Int): Int = a + b; add(41, 43) }")
        ast match {
          case LinkedPhase.Block(decls, result) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(name, paramGroups, _, body) =>
                name.value.value shouldBe LinkedInfo.Local(0, 0, "add", LocalBindingKind.Def)
                val params = paramGroups.head.value.parameters
                params.head.value.name.value.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Parameter)
                params(1).value.name.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Parameter)
                body.value match {
                  case LinkedPhase.Call.Infix(left, _, _, right) =>
                    left.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Parameter)
                      case other => fail(s"Expected Reference, got $other")
                    }
                    right.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Parameter)
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Infix call, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
            result.value match {
              case LinkedPhase.Call.Standard(target, _, _) =>
                target.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 0, "add", LocalBindingKind.Def)
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Standard call, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should reject recursive self-calls in def body") {
        val ast = link("{ def f(x: Int): Int = f(x); f(41) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Call.Standard(target, _, _) =>
                    target.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Error(SemanticError.UnknownIdentifier("f"))
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Call.Standard, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }

    describe("pattern matching") {
      it("should bind pattern variables in match cases AST") {
        val pIdent: Pos[ParsingPhase.Pattern[Pos]] = Pos(ParsingPhase.Pattern.Identifier[Pos](Pos(Identifier[Pos]("x"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1))
        val bodyIdent = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("x"), CharIndex(3), CharIndex(4))), CharIndex(3), CharIndex(4))
        val c = Pos(ParsingPhase.Case[Pos](pIdent, None, bodyIdent), CharIndex(0), CharIndex(4))
        val scrutinee = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = ParsingPhase.Match[Pos](scrutinee, Vector(c))

        val ast = linkAst(rawMatch)
        ast match {
          case LinkedPhase.Match(_, cases) =>
            cases.head.value match {
              case LinkedPhase.Case(pattern, _, body) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.PatternBinding)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
                body.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.PatternBinding)
                  case other => fail(s"Expected Reference, got $other")
                }
            }
          case other => fail(s"Expected Match, got $other")
        }
      }

      it("should assign slots for destructuring tuple patterns in AST") {
        val pA: Pos[ParsingPhase.Pattern[Pos]] = Pos(ParsingPhase.Pattern.Identifier[Pos](Pos(Identifier[Pos]("a"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1))
        val pB: Pos[ParsingPhase.Pattern[Pos]] = Pos(ParsingPhase.Pattern.Identifier[Pos](Pos(Identifier[Pos]("b"), CharIndex(2), CharIndex(3))), CharIndex(2), CharIndex(3))
        val pTuple: Pos[ParsingPhase.Pattern[Pos]] = Pos(ParsingPhase.Pattern.Tuple[Pos](Vector(pA, pB)), CharIndex(0), CharIndex(4))
        val refA = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("a"), CharIndex(5), CharIndex(6))), CharIndex(5), CharIndex(6))
        val refB = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Reference[Pos](Pos(Identifier[Pos]("b"), CharIndex(7), CharIndex(8))), CharIndex(7), CharIndex(8))
        val bodyTuple = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Tuple[Pos](Vector(refA, refB)), CharIndex(5), CharIndex(9))
        val c = Pos(ParsingPhase.Case[Pos](pTuple, None, bodyTuple), CharIndex(0), CharIndex(9))
        val scrutinee = Pos[ParsingPhase.Expression[Pos]](ParsingPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = ParsingPhase.Match[Pos](scrutinee, Vector(c))

        val ast = linkAst(rawMatch)
        ast match {
          case LinkedPhase.Match(_, cases) =>
            cases.head.value match {
              case LinkedPhase.Case(pattern, _, body) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Tuple(elements) =>
                    elements.head.value match {
                      case LinkedPhase.Pattern.Identifier(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Pattern.Identifier, got $other")
                    }
                    elements(1).value match {
                      case LinkedPhase.Pattern.Identifier(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Pattern.Identifier, got $other")
                    }
                  case other => fail(s"Expected Pattern.Tuple, got $other")
                }
                body.value match {
                  case LinkedPhase.Tuple(elements) =>
                    elements.head.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Reference, got $other")
                    }
                    elements(1).value match {
                      case LinkedPhase.Reference(id) =>
                        id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Tuple, got $other")
                }
            }
          case other => fail(s"Expected Match, got $other")
        }
      }
    }

    describe("lazy val declarations") {
      it("should link lazy val bindings and permit references to prior bindings") {
        val ast = link("{ val a = 41; lazy val b = a; b }")
        ast match {
          case LinkedPhase.Block(decls, result) =>
            decls.size shouldBe 2
            decls(1).value match {
              case LinkedPhase.Declaration.LazyVal(pattern, rhs) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.LazyVal)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
                rhs.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Val)
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected LazyVal, got $other")
            }
            result.value match {
              case LinkedPhase.Reference(id) =>
                id.value.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.LazyVal)
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }
  }
}
