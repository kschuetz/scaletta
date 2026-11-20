package software.kes.scaletta.internal.semantics

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.desugar.Desugarer
import software.kes.scaletta.internal.parser.Parser
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.runtime.CoreTypes._
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}
import software.kes.scaletta.internal.types.TypeResolutionError

final class LinkerSpec extends AnyFunSpec with Matchers {

  private val scaletta: ScalettaFacade =
    Scaletta.create().asInstanceOf[ScalettaFacade]

  private def parseExpr(source: String): Pos[ParsedPhase.Expression[Pos]] = {
    val reader = SourceReader.create(source.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    val result = parser.parse(scanner)
    result.value.getOrElse(fail(s"Failed to parse: $source"))
  }

  private def link(source: String, importScope: ImportScope = ImportScope.default): LinkedPhase.Expression[Pos] = {
    val parsed = parseExpr(source)
    val desugared = Desugarer.create().desugar(parsed.value)
    linkAst(desugared, importScope)
  }

  private def linkAst(desugared: DesugaredPhase.Expression[Pos], importScope: ImportScope = ImportScope.default): LinkedPhase.Expression[Pos] = {
    val linker = Linker.create(scaletta.universe, importScope)
    linker.link(desugared)
  }

  private def typeOf(ta: LinkedPhase.TypeIdent[Pos]): Either[TypeResolutionError, ProperType[TypeId]] = ta

  private def typeName(name: String): Pos[TypeIdentifier[Pos]] =
    Pos(TypeIdentifier.name[Pos](Pos(Identifier(name), CharIndex(0), CharIndex(name.length))), CharIndex(0), CharIndex(name.length))

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
                id.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Val)
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
                    id.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Val)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
            decls(1).value match {
              case LinkedPhase.Declaration.Val(pattern, _) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Val)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
            result.value match {
              case LinkedPhase.Reference(id) =>
                id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Val)
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
                    id.value shouldBe LinkedInfo.Local(1, 0, "x", LocalBindingKind.Val)
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
                    id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("y"))
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
                    id.value shouldBe LinkedInfo.Error(SemanticError.UnknownIdentifier("z"))
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag a forward reference in a nested block as an error") {
        val ast = link("{ val a = { val inner = futureVal; inner }; val futureVal = 41; a }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Block(innerDecls, _) =>
                    innerDecls.head.value match {
                      case LinkedPhase.Declaration.Val(_, innerRhs) =>
                        innerRhs.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                          case other => fail(s"Expected Reference, got $other")
                        }
                      case other => fail(s"Expected Val, got $other")
                    }
                  case other => fail(s"Expected Block, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag a forward reference in a lambda body as an error") {
        val pName = Pos(Identifier("x"), CharIndex(1), CharIndex(2))
        val param = Pos(DesugaredPhase.LambdaParameter[Pos](pName, Some(typeName("Int"))), CharIndex(1), CharIndex(8))
        val refX = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("x"), CharIndex(12), CharIndex(13))), CharIndex(12), CharIndex(13))
        val opIdent = Pos(Identifier("+"), CharIndex(14), CharIndex(15))
        val refFuture = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("futureVal"), CharIndex(16), CharIndex(25))), CharIndex(16), CharIndex(25))
        val bodyInfix = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Call.Infix[Pos](refX, opIdent, Vector.empty, refFuture), CharIndex(12), CharIndex(25))
        val rawLambda = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda[Pos](Vector(param), bodyInfix), CharIndex(0), CharIndex(25))

        val valF = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("f"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1)),
            rawLambda
          ),
          CharIndex(0), CharIndex(25)
        )
        val valFuture = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("futureVal"), CharIndex(27), CharIndex(36))), CharIndex(27), CharIndex(36)),
            Pos(DesugaredPhase.Literal.int[Pos](41), CharIndex(39), CharIndex(41))
          ),
          CharIndex(27), CharIndex(41)
        )
        val resRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("f"), CharIndex(43), CharIndex(44))), CharIndex(43), CharIndex(44))
        val rawBlock = DesugaredPhase.Block[Pos](Vector(valF, valFuture), resRef)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Lambda(_, body) =>
                    body.value match {
                      case LinkedPhase.Call.Infix(_, _, _, right) =>
                        right.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                          case other => fail(s"Expected Reference, got $other")
                        }
                      case other => fail(s"Expected Infix, got $other")
                    }
                  case other => fail(s"Expected Lambda, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag a forward reference in a pattern match guard as an error") {
        val pIdent: Pos[DesugaredPhase.Pattern[Pos]] = Pos(DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("x"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1))
        val guardRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("futureVal"), CharIndex(2), CharIndex(11))), CharIndex(2), CharIndex(11))
        val bodyRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("x"), CharIndex(13), CharIndex(14))), CharIndex(13), CharIndex(14))
        val c = Pos(DesugaredPhase.Case[Pos](pIdent, Some(guardRef), bodyRef), CharIndex(0), CharIndex(14))
        val scrutinee = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Match[Pos](scrutinee, Vector(c)), CharIndex(0), CharIndex(14))

        val valRes = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("res"), CharIndex(0), CharIndex(3))), CharIndex(0), CharIndex(3)),
            rawMatch
          ),
          CharIndex(0), CharIndex(14)
        )
        val valFuture = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("futureVal"), CharIndex(16), CharIndex(25))), CharIndex(16), CharIndex(25)),
            Pos(DesugaredPhase.Literal.int[Pos](41), CharIndex(28), CharIndex(30))
          ),
          CharIndex(16), CharIndex(30)
        )
        val resRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("res"), CharIndex(32), CharIndex(35))), CharIndex(32), CharIndex(35))
        val rawBlock = DesugaredPhase.Block[Pos](Vector(valRes, valFuture), resRef)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Match(_, cases) =>
                    cases.head.value match {
                      case LinkedPhase.Case(_, Some(guard), _) =>
                        guard.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                          case other => fail(s"Expected Reference, got $other")
                        }
                      case other => fail(s"Expected Case with guard, got $other")
                    }
                  case other => fail(s"Expected Match, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag a forward reference in a pattern match body as an error") {
        val pIdent: Pos[DesugaredPhase.Pattern[Pos]] = Pos(DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("x"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1))
        val bodyRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("futureVal"), CharIndex(3), CharIndex(12))), CharIndex(3), CharIndex(12))
        val c = Pos(DesugaredPhase.Case[Pos](pIdent, None, bodyRef), CharIndex(0), CharIndex(12))
        val scrutinee = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Match[Pos](scrutinee, Vector(c)), CharIndex(0), CharIndex(12))

        val valRes = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("res"), CharIndex(0), CharIndex(3))), CharIndex(0), CharIndex(3)),
            rawMatch
          ),
          CharIndex(0), CharIndex(12)
        )
        val valFuture = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("futureVal"), CharIndex(14), CharIndex(23))), CharIndex(14), CharIndex(23)),
            Pos(DesugaredPhase.Literal.int[Pos](41), CharIndex(26), CharIndex(28))
          ),
          CharIndex(14), CharIndex(28)
        )
        val resRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("res"), CharIndex(30), CharIndex(33))), CharIndex(30), CharIndex(33))
        val rawBlock = DesugaredPhase.Block[Pos](Vector(valRes, valFuture), resRef)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Match(_, cases) =>
                    cases.head.value match {
                      case LinkedPhase.Case(_, _, body) =>
                        body.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                          case other => fail(s"Expected Reference, got $other")
                        }
                    }
                  case other => fail(s"Expected Match, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag a forward reference in a def body as an error") {
        val ast = link("{ def f(x: Int): Int = futureVal; val futureVal = 41; f(1) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should not flag a forward reference when an inner parameter shadows an outer pending binding") {
        val pName = Pos(Identifier("futureVal"), CharIndex(1), CharIndex(10))
        val param = Pos(DesugaredPhase.LambdaParameter[Pos](pName, Some(typeName("Int"))), CharIndex(1), CharIndex(15))
        val bodyRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("futureVal"), CharIndex(17), CharIndex(26))), CharIndex(17), CharIndex(26))
        val rawLambda = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda[Pos](Vector(param), bodyRef), CharIndex(0), CharIndex(26))

        val valF = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("f"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1)),
            rawLambda
          ),
          CharIndex(0), CharIndex(26)
        )
        val valFuture = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("futureVal"), CharIndex(28), CharIndex(37))), CharIndex(28), CharIndex(37)),
            Pos(DesugaredPhase.Literal.int[Pos](41), CharIndex(40), CharIndex(42))
          ),
          CharIndex(28), CharIndex(42)
        )
        val resRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("f"), CharIndex(44), CharIndex(45))), CharIndex(44), CharIndex(45))
        val rawBlock = DesugaredPhase.Block[Pos](Vector(valF, valFuture), resRef)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Lambda(_, body) =>
                    body.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "futureVal", LocalBindingKind.Parameter)
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Lambda, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should preserve outer scope resolution when outer binding is already declared") {
        val pName = Pos(Identifier("x"), CharIndex(1), CharIndex(2))
        val param = Pos(DesugaredPhase.LambdaParameter[Pos](pName, Some(typeName("Int"))), CharIndex(1), CharIndex(8))
        val refOuter = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("outerVal"), CharIndex(10), CharIndex(18))), CharIndex(10), CharIndex(18))
        val opIdent = Pos(Identifier("+"), CharIndex(19), CharIndex(20))
        val refX = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("x"), CharIndex(21), CharIndex(22))), CharIndex(21), CharIndex(22))
        val bodyInfix = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Call.Infix[Pos](refOuter, opIdent, Vector.empty, refX), CharIndex(10), CharIndex(22))
        val rawLambda = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda[Pos](Vector(param), bodyInfix), CharIndex(0), CharIndex(22))

        val valOuter = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("outerVal"), CharIndex(0), CharIndex(8))), CharIndex(0), CharIndex(8)),
            Pos(DesugaredPhase.Literal.int[Pos](41), CharIndex(11), CharIndex(13))
          ),
          CharIndex(0), CharIndex(13)
        )
        val valF = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("f"), CharIndex(15), CharIndex(16))), CharIndex(15), CharIndex(16)),
            rawLambda
          ),
          CharIndex(15), CharIndex(35)
        )
        val valFuture = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("futureVal"), CharIndex(37), CharIndex(46))), CharIndex(37), CharIndex(46)),
            Pos(DesugaredPhase.Literal.int[Pos](43), CharIndex(49), CharIndex(51))
          ),
          CharIndex(37), CharIndex(51)
        )
        val resRef = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("f"), CharIndex(53), CharIndex(54))), CharIndex(53), CharIndex(54))
        val rawBlock = DesugaredPhase.Block[Pos](Vector(valOuter, valF, valFuture), resRef)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls(1).value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Lambda(_, body) =>
                    body.value match {
                      case LinkedPhase.Call.Infix(left, _, _, _) =>
                        left.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Local(1, 0, "outerVal", LocalBindingKind.Val)
                          case other => fail(s"Expected Reference, got $other")
                        }
                      case other => fail(s"Expected Infix, got $other")
                    }
                  case other => fail(s"Expected Lambda, got $other")
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
                    id.value shouldBe LinkedInfo.Error(SemanticError.DuplicateDefinition("x"))
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag duplicate parameters in a lambda AST") {
        val p1Name = Pos(Identifier("x"), CharIndex(1), CharIndex(2))
        val p1 = Pos(DesugaredPhase.LambdaParameter[Pos](p1Name, None), CharIndex(1), CharIndex(2))
        val p2Name = Pos(Identifier("x"), CharIndex(3), CharIndex(4))
        val p2 = Pos(DesugaredPhase.LambdaParameter[Pos](p2Name, None), CharIndex(3), CharIndex(4))
        val bodyIdent = Pos(Identifier("x"), CharIndex(6), CharIndex(7))
        val body = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](bodyIdent), CharIndex(6), CharIndex(7))
        val rawLambda = DesugaredPhase.Lambda[Pos](Vector(p1, p2), body)

        val ast = linkAst(rawLambda)
        ast match {
          case LinkedPhase.Lambda(params, _) =>
            params(1).value.name.value shouldBe LinkedInfo.Error(SemanticError.DuplicateDefinition("x"))
          case other => fail(s"Expected Lambda, got $other")
        }
      }
    }

    describe("lambdas and closures") {
      it("should resolve lambda parameters and outer captured variables in AST") {
        val pName = Pos(Identifier("x"), CharIndex(1), CharIndex(2))
        val param = Pos(DesugaredPhase.LambdaParameter[Pos](pName, None), CharIndex(1), CharIndex(2))
        val refOuter = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("outer"), CharIndex(5), CharIndex(10))), CharIndex(5), CharIndex(10))
        val refX = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("x"), CharIndex(12), CharIndex(13))), CharIndex(12), CharIndex(13))
        val tupleBody = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple[Pos](Vector(refOuter, refX)), CharIndex(5), CharIndex(14))
        val rawLambda = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Lambda[Pos](Vector(param), tupleBody), CharIndex(0), CharIndex(15))

        val valDecl = Pos[DesugaredPhase.Declaration[Pos]](
          DesugaredPhase.Declaration.Val[Pos](
            Pos[DesugaredPhase.Pattern[Pos]](DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("outer"), CharIndex(0), CharIndex(5))), CharIndex(0), CharIndex(5)),
            Pos(DesugaredPhase.Literal.int[Pos](41), CharIndex(6), CharIndex(8))
          ),
          CharIndex(0), CharIndex(8)
        )
        val rawBlock = DesugaredPhase.Block[Pos](Vector(valDecl), rawLambda)

        val ast = linkAst(rawBlock)
        ast match {
          case LinkedPhase.Block(_, result) =>
            result.value match {
              case LinkedPhase.Lambda(params, body) =>
                params.head.value.name.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Parameter)
                body.value match {
                  case LinkedPhase.Tuple(elements) =>
                    elements.head.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(1, 0, "outer", LocalBindingKind.Val)
                      case other => fail(s"Expected Reference, got $other")
                    }
                    elements(1).value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Parameter)
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
                name.value shouldBe LinkedInfo.Local(0, 0, "add", LocalBindingKind.Def)
                val params = paramGroups.head.value.parameters
                params.head.value.name.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Parameter)
                params(1).value.name.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Parameter)
                body.value match {
                  case LinkedPhase.Call.Infix(left, _, _, right) =>
                    left.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Parameter)
                      case other => fail(s"Expected Reference, got $other")
                    }
                    right.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.Parameter)
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
                    id.value shouldBe LinkedInfo.Local(0, 0, "add", LocalBindingKind.Def)
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
                        id.value shouldBe LinkedInfo.Error(SemanticError.RecursiveReference("f"))
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
        val pIdent: Pos[DesugaredPhase.Pattern[Pos]] = Pos(DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("x"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1))
        val bodyIdent = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("x"), CharIndex(3), CharIndex(4))), CharIndex(3), CharIndex(4))
        val c = Pos(DesugaredPhase.Case[Pos](pIdent, None, bodyIdent), CharIndex(0), CharIndex(4))
        val scrutinee = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = DesugaredPhase.Match[Pos](scrutinee, Vector(c))

        val ast = linkAst(rawMatch)
        ast match {
          case LinkedPhase.Match(_, cases) =>
            cases.head.value match {
              case LinkedPhase.Case(pattern, _, body) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Identifier(id) =>
                    id.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.PatternBinding)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
                body.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.PatternBinding)
                  case other => fail(s"Expected Reference, got $other")
                }
            }
          case other => fail(s"Expected Match, got $other")
        }
      }

      it("should assign slots for destructuring tuple patterns in AST") {
        val pA: Pos[DesugaredPhase.Pattern[Pos]] = Pos(DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("a"), CharIndex(0), CharIndex(1))), CharIndex(0), CharIndex(1))
        val pB: Pos[DesugaredPhase.Pattern[Pos]] = Pos(DesugaredPhase.Pattern.Identifier[Pos](Pos(Identifier("b"), CharIndex(2), CharIndex(3))), CharIndex(2), CharIndex(3))
        val pTuple: Pos[DesugaredPhase.Pattern[Pos]] = Pos(DesugaredPhase.Pattern.Tuple[Pos](Vector(pA, pB)), CharIndex(0), CharIndex(4))
        val refA = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("a"), CharIndex(5), CharIndex(6))), CharIndex(5), CharIndex(6))
        val refB = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Reference[Pos](Pos(Identifier("b"), CharIndex(7), CharIndex(8))), CharIndex(7), CharIndex(8))
        val bodyTuple = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Tuple[Pos](Vector(refA, refB)), CharIndex(5), CharIndex(9))
        val c = Pos(DesugaredPhase.Case[Pos](pTuple, None, bodyTuple), CharIndex(0), CharIndex(9))
        val scrutinee = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
        val rawMatch = DesugaredPhase.Match[Pos](scrutinee, Vector(c))

        val ast = linkAst(rawMatch)
        ast match {
          case LinkedPhase.Match(_, cases) =>
            cases.head.value match {
              case LinkedPhase.Case(pattern, _, body) =>
                pattern.value match {
                  case LinkedPhase.Pattern.Tuple(elements) =>
                    elements.head.value match {
                      case LinkedPhase.Pattern.Identifier(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Pattern.Identifier, got $other")
                    }
                    elements(1).value match {
                      case LinkedPhase.Pattern.Identifier(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Pattern.Identifier, got $other")
                    }
                  case other => fail(s"Expected Pattern.Tuple, got $other")
                }
                body.value match {
                  case LinkedPhase.Tuple(elements) =>
                    elements.head.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.PatternBinding)
                      case other => fail(s"Expected Reference, got $other")
                    }
                    elements(1).value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.PatternBinding)
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
                    id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.LazyVal)
                  case other => fail(s"Expected Pattern.Identifier, got $other")
                }
                rhs.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value shouldBe LinkedInfo.Local(0, 0, "a", LocalBindingKind.Val)
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected LazyVal, got $other")
            }
            result.value match {
              case LinkedPhase.Reference(id) =>
                id.value shouldBe LinkedInfo.Local(0, 1, "b", LocalBindingKind.LazyVal)
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }

    describe("call site candidate overload enrichment") {
      it("should resolve single candidate static function with exact arity match") {
        val ast = link("sqrt(16.0)")
        ast match {
          case LinkedPhase.Call.Standard(target, _, args) =>
            args.size shouldBe 1
            target.value match {
              case LinkedPhase.Reference(id) =>
                id.value match {
                  case LinkedInfo.StaticFunctionCandidates(name, arities, candidates) =>
                    name shouldBe "sqrt"
                    arities shouldBe Vector(1)
                    candidates.size shouldBe 1
                    candidates.head.returnType shouldBe DoubleT
                  case other => fail(s"Expected StaticFunctionCandidates, got $other")
                }
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Call.Standard, got $other")
        }
      }

      it("should preserve multiple candidate overloads when parameter shapes match") {
        val ast = link("abs(41)")
        ast match {
          case LinkedPhase.Call.Standard(target, _, args) =>
            args.size shouldBe 1
            target.value match {
              case LinkedPhase.Reference(id) =>
                id.value match {
                  case LinkedInfo.StaticFunctionCandidates(name, arities, candidates) =>
                    name shouldBe "abs"
                    arities shouldBe Vector(1)
                    candidates.size shouldBe 4
                  case other => fail(s"Expected StaticFunctionCandidates with 4 overloads, got $other")
                }
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Call.Standard, got $other")
        }
      }

      it("should report NoSuchFunction error when no static function matches") {
        val ast = link("nonexistentFunction(41, 43)")
        ast match {
          case LinkedPhase.Call.Standard(target, _, _) =>
            target.value match {
              case LinkedPhase.Reference(id) =>
                id.value match {
                  case LinkedInfo.Error(SemanticError.NoSuchFunction(name, _)) =>
                    name shouldBe "nonexistentFunction"
                  case other => fail(s"Expected NoSuchFunction error, got $other")
                }
              case other => fail(s"Expected Reference, got $other")
            }
          case other => fail(s"Expected Call.Standard, got $other")
        }
      }

      it("should enrich infix calls with matching method candidates from MethodResolver") {
        val ast = link("41 + 43")
        ast match {
          case LinkedPhase.Call.Infix(_, op, _, _) =>
            op.value match {
              case LinkedInfo.MethodCandidates(name, arities, candidates) =>
                name shouldBe "+"
                arities shouldBe Vector(1)
                candidates.nonEmpty shouldBe true
              case other => fail(s"Expected MethodCandidates, got $other")
            }
          case other => fail(s"Expected Call.Infix, got $other")
        }
      }

      it("should narrow method candidates when receiver type is explicitly ascribed") {
        val ast = link("(41: Int) + (43: Int)")
        ast match {
          case LinkedPhase.Call.Infix(_, op, _, _) =>
            op.value match {
              case LinkedInfo.MethodCandidates(name, arities, candidates) =>
                name shouldBe "+"
                arities shouldBe Vector(1)
                candidates.nonEmpty shouldBe true
                candidates.foreach { c =>
                  c.paramGroups.head.params.head.typ shouldBe IntT
                }
              case other => fail(s"Expected MethodCandidates for Int, got $other")
            }
          case other => fail(s"Expected Call.Infix, got $other")
        }
      }

      it("should resolve qualified static function calls") {
        val ast = link("scaletta.math.sqrt(16.0)")
        ast match {
          case LinkedPhase.Call.Standard(target, _, args) =>
            args.size shouldBe 1
            target.value match {
              case LinkedPhase.Select(_, nameIdent) =>
                nameIdent.value match {
                  case LinkedInfo.StaticFunctionCandidates(name, arities, candidates) =>
                    name shouldBe "sqrt"
                    arities shouldBe Vector(1)
                    candidates.nonEmpty shouldBe true
                  case other => fail(s"Expected StaticFunctionCandidates, got $other")
                }
              case other => fail(s"Expected Select, got $other")
            }
          case other => fail(s"Expected Call.Standard, got $other")
        }
      }

      describe("variadic native method and function linking") {
        val variadicModule = ScalettaModule { setup =>
          setup.methodRegistry.addVariadicMethod(
            MethodName(ReceiverType.Static(Packages.scaletta), Name("logMessage")),
            Vector(FormalParameter(Name("level"), StringT)),
            VariadicParameter(Name("messages"), StringT),
            UnitT,
            null
          )

          val formatterType = setup.typeRegistry.addRefType(Packages.scaletta.qualify(Name("Formatter")), RuntimeTypeInfo.any)
          setup.methodRegistry.addVariadicMethod(
            MethodName(ReceiverType.Instance(formatterType), Name("format")),
            Vector(FormalParameter(Name("pattern"), StringT)),
            VariadicParameter(Name("args"), AnyT),
            StringT,
            null
          )

          setup.methodRegistry.addVariadicMethod(
            MethodName(ReceiverType.Instance(StringT), Name("+++")),
            Vector.empty,
            VariadicParameter(Name("others"), StringT),
            StringT,
            null
          )
        }

        val vScaletta = Scaletta.create(Scaletta.addModule(variadicModule)).asInstanceOf[ScalettaFacade]

        def linkVariadic(source: String): LinkedPhase.Expression[Pos] = {
          val parsed = parseExpr(source)
          val desugared = Desugarer.create().desugar(parsed.value)
          val linker = Linker.create(vScaletta.universe, ImportScope.default)
          linker.link(desugared)
        }

        it("should resolve static variadic function calls with 1 argument (0 varargs)") {
          val ast = linkVariadic("logMessage(\"INFO\")")
          ast match {
            case LinkedPhase.Call.Standard(target, _, args) =>
              args.size shouldBe 1
              target.value match {
                case LinkedPhase.Reference(id) =>
                  id.value match {
                    case LinkedInfo.StaticFunctionCandidates(name, arities, candidates) =>
                      name shouldBe "logMessage"
                      arities shouldBe Vector(1)
                      candidates.size shouldBe 1
                      candidates.head.returnType shouldBe UnitT
                    case other => fail(s"Expected StaticFunctionCandidates, got $other")
                  }
                case other => fail(s"Expected Reference, got $other")
              }
            case other => fail(s"Expected Call.Standard, got $other")
          }
        }

        it("should resolve static variadic function calls with 2 arguments (1 vararg)") {
          val ast = linkVariadic("logMessage(\"INFO\", \"started\")")
          ast match {
            case LinkedPhase.Call.Standard(target, _, args) =>
              args.size shouldBe 1
              target.value match {
                case LinkedPhase.Reference(id) =>
                  id.value match {
                    case LinkedInfo.StaticFunctionCandidates(name, arities, candidates) =>
                      name shouldBe "logMessage"
                      arities shouldBe Vector(2)
                      candidates.size shouldBe 1
                    case other => fail(s"Expected StaticFunctionCandidates, got $other")
                  }
                case other => fail(s"Expected Reference, got $other")
              }
            case other => fail(s"Expected Call.Standard, got $other")
          }
        }

        it("should resolve static variadic function calls with 3 arguments (2 varargs)") {
          val ast = linkVariadic("logMessage(\"INFO\", \"a\", \"b\")")
          ast match {
            case LinkedPhase.Call.Standard(target, _, args) =>
              args.size shouldBe 1
              target.value match {
                case LinkedPhase.Reference(id) =>
                  id.value match {
                    case LinkedInfo.StaticFunctionCandidates(name, arities, candidates) =>
                      name shouldBe "logMessage"
                      arities shouldBe Vector(3)
                      candidates.size shouldBe 1
                    case other => fail(s"Expected StaticFunctionCandidates, got $other")
                  }
                case other => fail(s"Expected Reference, got $other")
              }
            case other => fail(s"Expected Call.Standard, got $other")
          }
        }

        it("should resolve instance variadic method calls on receiver") {
          val ast = linkVariadic("(formatter: Formatter).format(\"%d %s\", 41, \"test\")")
          ast match {
            case LinkedPhase.Call.Standard(target, _, args) =>
              args.size shouldBe 1
              target.value match {
                case LinkedPhase.Select(_, nameIdent) =>
                  nameIdent.value match {
                    case LinkedInfo.MethodCandidates(name, arities, candidates) =>
                      name shouldBe "format"
                      arities shouldBe Vector(3)
                      candidates.size shouldBe 1
                      candidates.head.returnType shouldBe StringT
                    case other => fail(s"Expected MethodCandidates, got $other")
                  }
                case other => fail(s"Expected Select, got $other")
              }
            case other => fail(s"Expected Call.Standard, got $other")
          }
        }

        it("should resolve infix calls for variadic methods") {
          val ast = linkVariadic("(\"hello\": String) +++ (\"world\": String)")
          ast match {
            case LinkedPhase.Call.Infix(_, op, _, _) =>
              op.value match {
                case LinkedInfo.MethodCandidates(name, arities, candidates) =>
                  name shouldBe "+++"
                  arities shouldBe Vector(1)
                  candidates.size shouldBe 1
                case other => fail(s"Expected MethodCandidates, got $other")
              }
            case other => fail(s"Expected Call.Infix, got $other")
          }
        }

        it("should report NoSuchFunction when calling a variadic function with fewer arguments than fixed parameters") {
          val ast = linkVariadic("logMessage()")
          ast match {
            case LinkedPhase.Call.Standard(target, _, _) =>
              target.value match {
                case LinkedPhase.Reference(id) =>
                  id.value match {
                    case LinkedInfo.Error(SemanticError.NoSuchFunction(name, _)) =>
                      name shouldBe "logMessage"
                    case other => fail(s"Expected NoSuchFunction error, got $other")
                  }
                case other => fail(s"Expected Reference, got $other")
              }
            case other => fail(s"Expected Call.Standard, got $other")
          }
        }

        it("should report NoSuchMethod when calling an instance variadic method with fewer arguments than fixed parameters") {
          val ast = linkVariadic("(formatter: Formatter).format()")
          ast match {
            case LinkedPhase.Call.Standard(target, _, _) =>
              target.value match {
                case LinkedPhase.Select(_, nameIdent) =>
                  nameIdent.value match {
                    case LinkedInfo.Error(SemanticError.NoSuchMethod(_, name, _)) =>
                      name shouldBe "format"
                    case other => fail(s"Expected NoSuchMethod error, got $other")
                  }
                case other => fail(s"Expected Select, got $other")
              }
            case other => fail(s"Expected Call.Standard, got $other")
          }
        }
      }
    }

    describe("type ascriptions and resolution") {
      describe("simple named types") {
        it("should resolve a known nominal type by name") {
          val ast = link("41: Int")
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe Right(IntT)
            case other => fail(s"Expected Typed, got $other")
          }
        }

        it("should report an unknown type name") {
          val ast = link("41: DoesNotExist")
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe Left(TypeResolutionError.UnknownType("DoesNotExist"))
            case other => fail(s"Expected Typed, got $other")
          }
        }
      }

      describe("compound types") {
        it("should resolve tuple type ascriptions") {
          val linker = Linker.create(scaletta.universe, ImportScope.default)
          val tupleTypeIdent: TypeIdentifier[Pos] =
            TypeIdentifier.tuple[Pos](Vector(typeName("Int"), typeName("String")))
          val target = Pos[DesugaredPhase.Expression[Pos]](DesugaredPhase.Literal.int[Pos](41), CharIndex(0), CharIndex(2))
          val typedExpr = DesugaredPhase.Typed[Pos](target, Pos(tupleTypeIdent, CharIndex(4), CharIndex(20)))
          val ast = linker.link(typedExpr)
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe Right(Type.tuple(IntT, StringT))
            case other => fail(s"Expected Typed, got $other")
          }
        }

        it("should resolve union type ascriptions") {
          val ast = link("41: Int | String")
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe Right(Type.union(IntT, StringT))
            case other => fail(s"Expected Typed, got $other")
          }
        }

        it("should propagate an error from a nested type component") {
          val ast = link("41: (Int, DoesNotExist)")
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe Left(TypeResolutionError.UnknownType("DoesNotExist"))
            case other => fail(s"Expected Typed, got $other")
          }
        }
      }

      describe("applied generic types") {
        it("should resolve a type constructor applied to its type arguments") {
          val ast = link("41: List[Int]")
          val stdLib = scaletta.universe.typeUniverse.nameIndex
          val listT = stdLib.resolve(QualifiedName.local(Name("List")), ImportScope.default) match {
            case entry :: Nil => entry.value
            case other => fail(s"Expected a single List entry, got $other")
          }
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe Right(TypeApplier.fromNode(listT.asInstanceOf[Type.Constructor[TypeId]]).applyAll(IntT))
            case other => fail(s"Expected Typed, got $other")
          }
        }

        it("should report wrong arity when applying too many type arguments") {
          val ast = link("41: List[Int, String]")
          ast match {
            case LinkedPhase.Typed(_, ascription) =>
              typeOf(ascription.value) shouldBe a[Left[_, _]]
            case other => fail(s"Expected Typed, got $other")
          }
        }
      }

      describe("def parameters and return types") {
        it("should resolve formal parameter and return types in a def") {
          val ast = link("{ def fn(a: Int): String = \"x\"; fn }")
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls.head.value match {
                case LinkedPhase.Declaration.Def(_, params, returnType, _) =>
                  typeOf(params.head.value.parameters.head.value.typ.value) shouldBe Right(IntT)
                  returnType.map(rt => typeOf(rt.value)) shouldBe Some(Right(StringT))
                case other => fail(s"Expected Def, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }
      }

      describe("singleton types") {
        def ascribedType(ast: LinkedPhase.Expression[Pos], index: Int = 0): Either[TypeResolutionError, ProperType[TypeId]] = {
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls(index).value match {
                case LinkedPhase.Declaration.Val(pattern, _) =>
                  pattern.value match {
                    case LinkedPhase.Pattern.Typed(_, ascription) => typeOf(ascription.value)
                    case other => fail(s"Expected Pattern.Typed, got $other")
                  }
                case other => fail(s"Expected Val, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }

        def linkCustom(source: String, imports: ImportScope, paths: Vector[PackagePath.Absolute]): LinkedPhase.Expression[Pos] = {
          val customModule = ScalettaModule.methodsOnly { registry =>
            paths.foreach { path =>
              registry.addMethod(MethodName(ReceiverType.Static(path), Name("Item")), Vector.empty, IntT, FunctionImpl.intResult(_ => 41))
            }
          }
          val customScaletta = Scaletta.builder.addModule(customModule).build.asInstanceOf[ScalettaFacade]
          Linker.create(customScaletta.universe, imports).link(Desugarer.create().desugar(parseExpr(source).value))
        }

        it("should resolve a static root symbol singleton") {
          val ast = linkCustom("{ val x: Item.type = 41; x }", ImportScope.default, Vector(PackagePath.root))
          ascribedType(ast) shouldBe Right(Type.Singleton(TermSymbolId.Static(QualifiedName.Full(PackagePath.root, Name("Item")))))
        }

        it("should resolve default imported and qualified overloaded static symbols") {
          val expected = Right(Type.Singleton(TermSymbolId.Static(QualifiedName.Full(Packages.scalettaMath, Name("abs")))))
          ascribedType(link("{ val x: abs.type = 41; x }")) shouldBe expected
          ascribedType(link("{ val x: scaletta.math.abs.type = 41; x }")) shouldBe expected
        }

        it("should resolve a static singleton through an explicit import") {
          val name = QualifiedName.Full(Packages.scalettaCollectionVector, Name("Item"))
          val ast = linkCustom("{ val x: Item.type = 41; x }", ImportScope.importSymbol(name), Vector(Packages.scalettaCollectionVector))
          ascribedType(ast) shouldBe Right(Type.Singleton(TermSymbolId.Static(name)))
        }

        it("should report unknown unqualified and qualified static terms") {
          ascribedType(link("{ val x: NonExistent.type = 41; x }")) shouldBe Left(TypeResolutionError.UnknownTerm("NonExistent"))
          ascribedType(link("{ val x: some.pkg.MissingObj.type = 41; x }")) shouldBe Left(TypeResolutionError.UnknownTerm("some.pkg.MissingObj"))
        }

        it("should report ambiguous imported static symbols") {
          val pkg1 = PackagePath.parseAbsolute("org.example1")
          val pkg2 = PackagePath.parseAbsolute("org.example2")
          val ast = linkCustom("{ val x: Item.type = 41; x }", ImportScope.importWildcard(pkg1, pkg2), Vector(pkg1, pkg2))
          ascribedType(ast) shouldBe Left(TypeResolutionError.AmbiguousTerm("Item", 2))
        }

        it("should prioritize local bindings over static symbols") {
          ascribedType(link("{ val abs = 41; val y: abs.type = 43; y }"), 1) shouldBe
            Right(Type.Singleton(TermSymbolId.Local("abs", 0, 0)))
        }

        it("should resolve a local val singleton type in the same block") {
          val ast = link("{ val x: Int = 41; val y: x.type = x; y }")
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls(1).value match {
                case LinkedPhase.Declaration.Val(pattern, _) =>
                  pattern.value match {
                    case LinkedPhase.Pattern.Typed(_, ascription) =>
                      typeOf(ascription.value) shouldBe Right(Type.Singleton(TermSymbolId.Local("x", 0, 0)))
                    case other => fail(s"Expected Pattern.Typed, got $other")
                  }
                case other => fail(s"Expected Val, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }

        it("should resolve an outer lexical val singleton type with correct depth") {
          val ast = link("{ val x: Int = 41; { val y: x.type = x; y } }")
          ast match {
            case LinkedPhase.Block(decls, result) =>
              result.value match {
                case LinkedPhase.Block(innerDecls, _) =>
                  innerDecls.head.value match {
                    case LinkedPhase.Declaration.Val(pattern, _) =>
                      pattern.value match {
                        case LinkedPhase.Pattern.Typed(_, ascription) =>
                          typeOf(ascription.value) shouldBe Right(Type.Singleton(TermSymbolId.Local("x", 1, 0)))
                        case other => fail(s"Expected Pattern.Typed, got $other")
                      }
                    case other => fail(s"Expected Val, got $other")
                  }
                case other => fail(s"Expected inner Block, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }

        it("should resolve static root symbol singletons like None.type") {
          val customModule = ScalettaModule.methodsOnly { registry =>
            registry.addMethod(MethodName(ReceiverType.Static(PackagePath.root), Name("None")), Vector.empty, IntT, FunctionImpl.intResult(_ => 41))
          }
          val customScaletta = Scaletta.builder.addModule(customModule).build.asInstanceOf[ScalettaFacade]
          val ast = Linker.create(customScaletta.universe, ImportScope.default).link(
            Desugarer.create().desugar(parseExpr("{ val x: None.type = None; x }").value)
          )
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls.head.value match {
                case LinkedPhase.Declaration.Val(pattern, _) =>
                  pattern.value match {
                    case LinkedPhase.Pattern.Typed(_, ascription) =>
                      typeOf(ascription.value) shouldBe Right(
                        Type.Singleton(TermSymbolId.Static(QualifiedName.Full(PackagePath.root, Name("None"))))
                      )
                    case other => fail(s"Expected Pattern.Typed, got $other")
                  }
                case other => fail(s"Expected Val, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }

        it("should resolve qualified package static singletons") {
          val ast = link("{ val x: scaletta.math.abs.type = scaletta.math.abs; x }")
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls.head.value match {
                case LinkedPhase.Declaration.Val(pattern, _) =>
                  pattern.value match {
                    case LinkedPhase.Pattern.Typed(_, ascription) =>
                      typeOf(ascription.value) shouldBe Right(
                        Type.Singleton(TermSymbolId.Static(QualifiedName.Full(Packages.scalettaMath, Name("abs"))))
                      )
                    case other => fail(s"Expected Pattern.Typed, got $other")
                  }
                case other => fail(s"Expected Val, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }

        it("should report ForwardReference when referencing a term declared later in the block") {
          val ast = link("{ val x: y.type = 41; val y: Int = 43; x }")
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls.head.value match {
                case LinkedPhase.Declaration.Val(pattern, _) =>
                  pattern.value match {
                    case LinkedPhase.Pattern.Typed(_, ascription) =>
                      typeOf(ascription.value) shouldBe Left(TypeResolutionError.ForwardReference("y"))
                    case other => fail(s"Expected Pattern.Typed, got $other")
                  }
                case other => fail(s"Expected Val, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }

        it("should report RecursiveReference when referencing the term being defined") {
          val ast = link("{ val x: x.type = 41; x }")
          ast match {
            case LinkedPhase.Block(decls, _) =>
              decls.head.value match {
                case LinkedPhase.Declaration.Val(pattern, _) =>
                  pattern.value match {
                    case LinkedPhase.Pattern.Typed(_, ascription) =>
                      typeOf(ascription.value) shouldBe Left(TypeResolutionError.RecursiveReference("x"))
                    case other => fail(s"Expected Pattern.Typed, got $other")
                  }
                case other => fail(s"Expected Val, got $other")
              }
            case other => fail(s"Expected Block, got $other")
          }
        }
      }
    }

    describe("recursion and self-reference constraints") {
      it("should flag direct def self-recursion as an error") {
        val ast = link("{ def factorial(n: Int): Int = factorial(n - 1); factorial(5) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Call.Standard(target, _, _) =>
                    target.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Error(SemanticError.RecursiveReference("factorial"))
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Call.Standard, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag self-reference in a parameterless def body") {
        val ast = link("{ def f: Int = f; f }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value shouldBe LinkedInfo.Error(SemanticError.RecursiveReference("f"))
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should prioritize recursive error over shadowing an outer symbol with the same name") {
        val ast = link("{ val f = 11; def f(x: Int): Int = f(x); f(5) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls(1).value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Call.Standard(target, _, _) =>
                    target.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Error(SemanticError.RecursiveReference("f"))
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Call.Standard, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should resolve to formal parameter when a parameter shadows the def name") {
        val ast = link("{ def f(f: Int): Int = f; f(5) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Reference(id) =>
                    id.value shouldBe LinkedInfo.Local(0, 0, "f", LocalBindingKind.Parameter)
                  case other => fail(s"Expected Reference, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should resolve to local val when an inner block shadows the def name") {
        val ast = link("{ def f(x: Int): Int = { val f = 41; f }; f(5) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Block(innerDecls, innerResult) =>
                    innerDecls.head.value match {
                      case LinkedPhase.Declaration.Val(pattern, _) =>
                        pattern.value match {
                          case LinkedPhase.Pattern.Identifier(id) =>
                            id.value shouldBe LinkedInfo.Local(0, 0, "f", LocalBindingKind.Val)
                          case other => fail(s"Expected Pattern.Identifier, got $other")
                        }
                      case other => fail(s"Expected Val, got $other")
                    }
                    innerResult.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "f", LocalBindingKind.Val)
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Block, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag recursive references to enclosing defs within nested defs") {
        val ast = link("{ def outer(x: Int): Int = { def inner(y: Int): Int = outer(y); inner(x) }; outer(5) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, outerBody) =>
                outerBody.value match {
                  case LinkedPhase.Block(innerDecls, _) =>
                    innerDecls.head.value match {
                      case LinkedPhase.Declaration.Def(_, _, _, innerBody) =>
                        innerBody.value match {
                          case LinkedPhase.Call.Standard(target, _, _) =>
                            target.value match {
                              case LinkedPhase.Reference(id) =>
                                id.value shouldBe LinkedInfo.Error(SemanticError.RecursiveReference("outer"))
                              case other => fail(s"Expected Reference, got $other")
                            }
                          case other => fail(s"Expected Call.Standard, got $other")
                        }
                      case other => fail(s"Expected Def, got $other")
                    }
                  case other => fail(s"Expected Block, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }

    describe("lazy val and val self-reference and forward reference checks") {
      it("should flag self-reference in lazy val rhs as forward reference error") {
        val ast = link("{ lazy val x: Int = x + 1; x }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.LazyVal(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Call.Infix(left, _, _, _) =>
                    left.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("x"))
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Infix, got $other")
                }
              case other => fail(s"Expected LazyVal, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag lazy val self-reference rather than binding to outer symbol") {
        val ast = link("{ val x = 11; { lazy val x: Int = x + 1; x } }")
        ast match {
          case LinkedPhase.Block(_, outerResult) =>
            outerResult.value match {
              case LinkedPhase.Block(innerDecls, _) =>
                innerDecls.head.value match {
                  case LinkedPhase.Declaration.LazyVal(_, rhs) =>
                    rhs.value match {
                      case LinkedPhase.Call.Infix(left, _, _, _) =>
                        left.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("x"))
                          case other => fail(s"Expected Reference, got $other")
                        }
                      case other => fail(s"Expected Infix, got $other")
                    }
                  case other => fail(s"Expected LazyVal, got $other")
                }
              case other => fail(s"Expected inner Block, got $other")
            }
          case other => fail(s"Expected outer Block, got $other")
        }
      }

      it("should resolve to inner local val when nested block in lazy val rhs defines the symbol") {
        val ast = link("{ lazy val x: Int = { val x = 41; x }; x }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.LazyVal(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Block(innerDecls, innerResult) =>
                    innerDecls.head.value match {
                      case LinkedPhase.Declaration.Val(pattern, _) =>
                        pattern.value match {
                          case LinkedPhase.Pattern.Identifier(id) =>
                            id.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Val)
                          case other => fail(s"Expected Pattern.Identifier, got $other")
                        }
                      case other => fail(s"Expected Val, got $other")
                    }
                    innerResult.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Local(0, 0, "x", LocalBindingKind.Val)
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Block, got $other")
                }
              case other => fail(s"Expected LazyVal, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag self-reference in val rhs as forward reference error") {
        val ast = link("{ val x: Int = x + 1; x }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Call.Infix(left, _, _, _) =>
                    left.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("x"))
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Infix, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag forward reference across multiple nested block levels") {
        val ast = link("{ val a = { { futureVal } }; val futureVal = 41; a }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Block(_, midResult) =>
                    midResult.value match {
                      case LinkedPhase.Block(_, innerResult) =>
                        innerResult.value match {
                          case LinkedPhase.Reference(id) =>
                            id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                          case other => fail(s"Expected Reference, got $other")
                        }
                      case other => fail(s"Expected innermost Block, got $other")
                    }
                  case other => fail(s"Expected middle Block, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag forward reference between sibling def declarations") {
        val ast = link("{ def first(x: Int): Int = second(x); def second(x: Int): Int = x; first(5) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Def(_, _, _, body) =>
                body.value match {
                  case LinkedPhase.Call.Standard(target, _, _) =>
                    target.value match {
                      case LinkedPhase.Reference(id) =>
                        id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("second"))
                      case other => fail(s"Expected Reference, got $other")
                    }
                  case other => fail(s"Expected Call.Standard, got $other")
                }
              case other => fail(s"Expected Def, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }

      it("should flag forward reference inside pattern match inside lambda") {
        val ast = link("{ val f = (x: Int) => x match { case 0 => futureVal; case _ => 41 }; val futureVal = 43; f(0) }")
        ast match {
          case LinkedPhase.Block(decls, _) =>
            decls.head.value match {
              case LinkedPhase.Declaration.Val(_, rhs) =>
                rhs.value match {
                  case LinkedPhase.Lambda(_, lambdaBody) =>
                    lambdaBody.value match {
                      case LinkedPhase.Match(_, cases) =>
                        cases.head.value match {
                          case LinkedPhase.Case(_, _, caseBody) =>
                            caseBody.value match {
                              case LinkedPhase.Reference(id) =>
                                id.value shouldBe LinkedInfo.Error(SemanticError.ForwardReference("futureVal"))
                              case other => fail(s"Expected Reference, got $other")
                            }
                        }
                      case other => fail(s"Expected Match, got $other")
                    }
                  case other => fail(s"Expected Lambda, got $other")
                }
              case other => fail(s"Expected Val, got $other")
            }
          case other => fail(s"Expected Block, got $other")
        }
      }
    }
  }
}
