package software.kes.scaletta.internal.desugar

import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.desugar.Desugarer.Identifiers
import software.kes.scaletta.internal.library.standard.CommonMethodNames
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}

object Desugarer {
  type Input = ParsedPhase.Expression[Pos]
  type Output = DesugaredPhase.Expression[Pos]

  def create(): Desugarer = new Desugarer()

  private object Identifiers {
    final val FlatMap = Identifier(CommonMethodNames.flatMap.value)
    final val Map = Identifier(CommonMethodNames.map.value)
    final val WithFilter = Identifier(CommonMethodNames.withFilter.value)
  }
}

/**
 * Transforms an attributed AST from [[ParsedPhase]] to [[DesugaredPhase]].
 *
 * In this initial implementation, both phases are structurally identical, and this
 * class acts as an identity bridge between the raw parsed syntax tree and the desugared AST.
 */
final class Desugarer private() {

  def desugar(input: Desugarer.Input): Desugarer.Output =
    desugarExpression(input)

  def desugar(posExpr: Pos[ParsedPhase.Expression[Pos]]): Pos[DesugaredPhase.Expression[Pos]] =
    desugarPosExpression(posExpr)

  private def desugarPosExpression(posExpr: Pos[ParsedPhase.Expression[Pos]]): Pos[DesugaredPhase.Expression[Pos]] =
    Pos(desugarExpression(posExpr.value), posExpr.begin, posExpr.end)

  private def desugarExpression(expr: ParsedPhase.Expression[Pos]): DesugaredPhase.Expression[Pos] =
    expr match {
      case ParsedPhase.Expression.Error(err) =>
        DesugaredPhase.Expression.Error(err)

      case lit: ParsedPhase.Literal[Pos] @unchecked =>
        desugarLiteral(lit)

      case ParsedPhase.Reference(id) =>
        DesugaredPhase.Reference(id)

      case ParsedPhase.Select(qualifier, name) =>
        DesugaredPhase.Select(desugarPosExpression(qualifier), name)

      case ParsedPhase.Typed(expression, ascription) =>
        DesugaredPhase.Typed(desugarPosExpression(expression), ascription)

      case ParsedPhase.Tuple(elements) =>
        DesugaredPhase.Tuple(
          elements.foldLeft(Vector.empty[Pos[DesugaredPhase.Expression[Pos]]]) { (acc, el) =>
            acc :+ desugarPosExpression(el)
          }
        )

      case ParsedPhase.Conditional(condition, thenBranch, elseBranch) =>
        DesugaredPhase.Conditional(
          desugarPosExpression(condition),
          desugarPosExpression(thenBranch),
          desugarPosExpression(elseBranch)
        )

      case ParsedPhase.Call.Infix(left, operation, typeArgs, right) =>
        DesugaredPhase.Call.Infix(
          desugarPosExpression(left),
          operation,
          typeArgs.foldLeft(Vector.empty[Pos[DesugaredPhase.TypeArgument[Pos]]]) { (acc, ta) =>
            acc :+ desugarPosTypeArgument(ta)
          },
          desugarPosExpression(right)
        )

      case ParsedPhase.Call.Standard(target, typeArgs, args) =>
        DesugaredPhase.Call.Standard(
          desugarPosExpression(target),
          typeArgs.foldLeft(Vector.empty[Pos[DesugaredPhase.TypeArgument[Pos]]]) { (acc, ta) =>
            acc :+ desugarPosTypeArgument(ta)
          },
          args.foldLeft(Vector.empty[Pos[DesugaredPhase.ArgumentGroup[Pos]]]) { (acc, ag) =>
            acc :+ desugarPosArgumentGroup(ag)
          }
        )

      case ParsedPhase.Lambda(params, body) =>
        DesugaredPhase.Lambda(
          params.foldLeft(Vector.empty[Pos[DesugaredPhase.LambdaParameter[Pos]]]) { (acc, p) =>
            acc :+ desugarPosLambdaParameter(p)
          },
          desugarPosExpression(body)
        )

      case ParsedPhase.InterpolatedString(interpolator, initial, segments) =>
        DesugaredPhase.InterpolatedString(
          interpolator,
          initial,
          segments.foldLeft(Vector.empty[(Pos[DesugaredPhase.Expression[Pos]], String)]) {
            case (acc, (exprPart, textPart)) =>
              acc :+ (desugarPosExpression(exprPart), textPart)
          }
        )

      case ParsedPhase.Match(expression, cases) =>
        DesugaredPhase.Match(
          desugarPosExpression(expression),
          cases.foldLeft(Vector.empty[Pos[DesugaredPhase.Case[Pos]]]) { (acc, c) =>
            acc :+ desugarPosCase(c)
          }
        )

      case ParsedPhase.Block(declarations, result) =>
        DesugaredPhase.Block(
          declarations.foldLeft(Vector.empty[Pos[DesugaredPhase.Declaration[Pos]]]) { (acc, d) =>
            acc :+ desugarPosDeclaration(d)
          },
          desugarPosExpression(result)
        )

      case ParsedPhase.ForYield(enumerators, body) =>
        desugarForYield(
          enumerators,
          body,
          if (enumerators.nonEmpty) enumerators.head.begin else body.begin,
          body.end
        )
    }

  private type ExprPos = Pos[DesugaredPhase.Expression[Pos]]
  private type PatPos = Pos[DesugaredPhase.Pattern[Pos]]
  private type DeclPos = Pos[DesugaredPhase.Declaration[Pos]]
  private type CasePos = Pos[DesugaredPhase.Case[Pos]]
  private type ArgPos = Pos[DesugaredPhase.Argument[Pos]]
  private type ArgGroupPos = Pos[DesugaredPhase.ArgumentGroup[Pos]]
  private type ParamPos = Pos[DesugaredPhase.LambdaParameter[Pos]]

  private def posExpr(expr: DesugaredPhase.Expression[Pos], begin: CharIndex, end: CharIndex): ExprPos =
    Pos[DesugaredPhase.Expression[Pos]](expr, begin, end)

  private def posPat(pat: DesugaredPhase.Pattern[Pos], begin: CharIndex, end: CharIndex): PatPos =
    Pos[DesugaredPhase.Pattern[Pos]](pat, begin, end)

  private def posDecl(decl: DesugaredPhase.Declaration[Pos], begin: CharIndex, end: CharIndex): DeclPos =
    Pos[DesugaredPhase.Declaration[Pos]](decl, begin, end)

  private def posCase(kase: DesugaredPhase.Case[Pos], begin: CharIndex, end: CharIndex): CasePos =
    Pos[DesugaredPhase.Case[Pos]](kase, begin, end)

  private def posArg(arg: DesugaredPhase.Argument[Pos], begin: CharIndex, end: CharIndex): ArgPos =
    Pos[DesugaredPhase.Argument[Pos]](arg, begin, end)

  private def posArgGroup(group: DesugaredPhase.ArgumentGroup[Pos], begin: CharIndex, end: CharIndex): ArgGroupPos =
    Pos[DesugaredPhase.ArgumentGroup[Pos]](group, begin, end)

  private def posParam(param: DesugaredPhase.LambdaParameter[Pos], begin: CharIndex, end: CharIndex): ParamPos =
    Pos[DesugaredPhase.LambdaParameter[Pos]](param, begin, end)

  private def desugarForYield(enumerators: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                              body: Pos[ParsedPhase.Expression[Pos]],
                              begin: CharIndex,
                              end: CharIndex): DesugaredPhase.Expression[Pos] = {
    var synthCounter: Int = 0

    def nextSynthIdent(): Identifier = {
      synthCounter += 1
      Identifier(s"$$synth$$$synthCounter")
    }

    def desugarEnums(
                      enums: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                      spanBegin: CharIndex,
                      spanEnd: CharIndex
                    ): ExprPos = {
      val leadingBindings = enums.takeWhile {
        case Pos(_: ParsedPhase.Enumerator.ValueBinding[Pos], _, _) => true
        case _ => false
      }
      val restEnums = enums.drop(leadingBindings.length)

      if (leadingBindings.nonEmpty) {
        val valDecls = leadingBindings.foldLeft(Vector.empty[DeclPos]) { (acc, vb) =>
          val b = vb.value.asInstanceOf[ParsedPhase.Enumerator.ValueBinding[Pos]]
          acc :+ posDecl(
            DesugaredPhase.Declaration.Val(
              desugarPosPattern(b.pattern),
              desugarPosExpression(b.expression)
            ),
            vb.begin,
            vb.end
          )
        }
        if (restEnums.isEmpty) {
          posExpr(
            DesugaredPhase.Block(valDecls, desugarPosExpression(body)),
            spanBegin,
            spanEnd
          )
        } else {
          val inner = desugarEnums(restEnums, restEnums.head.begin, spanEnd)
          posExpr(
            DesugaredPhase.Block(valDecls, inner),
            spanBegin,
            spanEnd
          )
        }
      } else if (restEnums.isEmpty) {
        desugarPosExpression(body)
      } else {
        desugarGeneratorStep(restEnums, spanBegin, spanEnd)
      }
    }

    def desugarGeneratorStep(enums: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                             spanBegin: CharIndex,
                             spanEnd: CharIndex): ExprPos = {
      val firstEnum = enums.head
      firstEnum.value match {
        case gen: ParsedPhase.Enumerator.Generator[Pos] =>
          val afterGen = enums.tail
          val guards = afterGen.takeWhile {
            case Pos(_: ParsedPhase.Enumerator.Guard[Pos], _, _) => true
            case _ => false
          }
          val afterGuards = afterGen.drop(guards.length)
          val valueBindings = afterGuards.takeWhile {
            case Pos(_: ParsedPhase.Enumerator.ValueBinding[Pos], _, _) => true
            case _ => false
          }
          val remaining = afterGuards.drop(valueBindings.length)

          val hasMoreGenerators = remaining.exists {
            case Pos(_: ParsedPhase.Enumerator.Generator[Pos], _, _) => true
            case _ => false
          }

          val methodIdentifier = if (hasMoreGenerators) Identifiers.FlatMap else Identifiers.Map
          val innerResult = if (hasMoreGenerators) {
            desugarEnums(remaining, remaining.head.begin, spanEnd)
          } else {
            desugarPosExpression(body)
          }

          val initialTarget = desugarPosExpression(gen.expression)

          val (filteredTarget, lambda) = if (gen.isCase) {
            desugarCaseGenerator(gen, firstEnum.begin, firstEnum.end, initialTarget, guards, valueBindings, innerResult, spanEnd)
          } else {
            gen.pattern.value match {
              case ParsedPhase.Pattern.Identifier(ident) =>
                desugarSimpleIdentifierGenerator(gen, ident, initialTarget, guards, valueBindings, innerResult, spanEnd)
              case _ =>
                desugarDestructuringGenerator(gen, initialTarget, guards, valueBindings, innerResult, spanEnd)
            }
          }

          val methodNamePos = Pos(methodIdentifier, firstEnum.begin, firstEnum.end)
          val select = posExpr(DesugaredPhase.Select(filteredTarget, methodNamePos), filteredTarget.begin, firstEnum.end)
          val arg = posArg(DesugaredPhase.Argument(lambda, None), lambda.begin, lambda.end)
          val argGroup = posArgGroup(DesugaredPhase.ArgumentGroup(Vector(arg), None), lambda.begin, lambda.end)
          posExpr(
            DesugaredPhase.Call.Standard(select, Vector.empty, Vector(argGroup)),
            spanBegin,
            spanEnd
          )

        case _ =>
          desugarPosExpression(body)
      }
    }

    def desugarSimpleIdentifierGenerator(gen: ParsedPhase.Enumerator.Generator[Pos],
                                         ident: Pos[Identifier],
                                         initialTarget: ExprPos,
                                         guards: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                                         valueBindings: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                                         innerResult: ExprPos,
                                         spanEnd: CharIndex): (ExprPos, ExprPos) = {
      val filteredTarget = guards.foldLeft(initialTarget) { (accTarget, g) =>
        val guard = g.value.asInstanceOf[ParsedPhase.Enumerator.Guard[Pos]]
        val condExpr = desugarPosExpression(guard.condition)
        val paramPos = posParam(DesugaredPhase.LambdaParameter(ident, None), gen.pattern.begin, gen.pattern.end)
        val guardLambda = posExpr(DesugaredPhase.Lambda(Vector(paramPos), condExpr), g.begin, g.end)
        wrapWithFilter(accTarget, guardLambda, g.begin, g.end)
      }

      val paramPos = posParam(DesugaredPhase.LambdaParameter(ident, None), gen.pattern.begin, gen.pattern.end)
      val valDecls = valueBindings.foldLeft(Vector.empty[DeclPos]) { (acc, vb) =>
        val b = vb.value.asInstanceOf[ParsedPhase.Enumerator.ValueBinding[Pos]]
        acc :+ posDecl(
          DesugaredPhase.Declaration.Val(
            desugarPosPattern(b.pattern),
            desugarPosExpression(b.expression)
          ),
          vb.begin,
          vb.end
        )
      }

      val lambdaBody = if (valDecls.nonEmpty) {
        posExpr(DesugaredPhase.Block(valDecls, innerResult), valDecls.head.begin, innerResult.end)
      } else {
        innerResult
      }

      val lambda = posExpr(DesugaredPhase.Lambda(Vector(paramPos), lambdaBody), gen.pattern.begin, spanEnd)
      (filteredTarget, lambda)
    }

    def desugarDestructuringGenerator(gen: ParsedPhase.Enumerator.Generator[Pos],
                                      initialTarget: ExprPos,
                                      guards: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                                      valueBindings: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                                      innerResult: ExprPos,
                                      spanEnd: CharIndex): (ExprPos, ExprPos) = {
      val filteredTarget = guards.foldLeft(initialTarget) { (accTarget, g) =>
        val guard = g.value.asInstanceOf[ParsedPhase.Enumerator.Guard[Pos]]
        val condExpr = desugarPosExpression(guard.condition)
        val guardSynth = nextSynthIdent()
        val guardSynthPos = Pos(guardSynth, gen.pattern.begin, gen.pattern.end)
        val guardSynthRef = posExpr(DesugaredPhase.Reference(guardSynthPos), gen.pattern.begin, gen.pattern.end)
        val guardParam = posParam(DesugaredPhase.LambdaParameter(guardSynthPos, None), gen.pattern.begin, gen.pattern.end)
        val valDecl = posDecl(DesugaredPhase.Declaration.Val(desugarPosPattern(gen.pattern), guardSynthRef), gen.pattern.begin, gen.pattern.end)
        val guardBody = posExpr(DesugaredPhase.Block(Vector(valDecl), condExpr), gen.pattern.begin, condExpr.end)
        val guardLambda = posExpr(DesugaredPhase.Lambda(Vector(guardParam), guardBody), g.begin, g.end)
        wrapWithFilter(accTarget, guardLambda, g.begin, g.end)
      }

      val mapSynth = nextSynthIdent()
      val mapSynthPos = Pos(mapSynth, gen.pattern.begin, gen.pattern.end)
      val mapSynthRef = posExpr(DesugaredPhase.Reference(mapSynthPos), gen.pattern.begin, gen.pattern.end)
      val mapParam = posParam(DesugaredPhase.LambdaParameter(mapSynthPos, None), gen.pattern.begin, gen.pattern.end)
      val destructDecl = posDecl(DesugaredPhase.Declaration.Val(desugarPosPattern(gen.pattern), mapSynthRef), gen.pattern.begin, gen.pattern.end)

      val extraValDecls = valueBindings.foldLeft(Vector.empty[DeclPos]) { (acc, vb) =>
        val b = vb.value.asInstanceOf[ParsedPhase.Enumerator.ValueBinding[Pos]]
        acc :+ posDecl(
          DesugaredPhase.Declaration.Val(
            desugarPosPattern(b.pattern),
            desugarPosExpression(b.expression)
          ),
          vb.begin,
          vb.end
        )
      }

      val allValDecls = destructDecl +: extraValDecls
      val lambdaBody = posExpr(DesugaredPhase.Block(allValDecls, innerResult), destructDecl.begin, innerResult.end)
      val lambda = posExpr(DesugaredPhase.Lambda(Vector(mapParam), lambdaBody), gen.pattern.begin, spanEnd)

      (filteredTarget, lambda)
    }

    def desugarCaseGenerator(gen: ParsedPhase.Enumerator.Generator[Pos],
                             genBegin: CharIndex,
                             genEnd: CharIndex,
                             initialTarget: ExprPos,
                             guards: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                             valueBindings: Vector[Pos[ParsedPhase.Enumerator[Pos]]],
                             innerResult: ExprPos,
                             spanEnd: CharIndex): (ExprPos, ExprPos) = {
      val filteredTarget = if (guards.isEmpty) {
        val synthId = nextSynthIdent()
        val synthPos = Pos(synthId, gen.pattern.begin, gen.pattern.end)
        val synthRef = posExpr(DesugaredPhase.Reference(synthPos), gen.pattern.begin, gen.pattern.end)
        val synthParam = posParam(DesugaredPhase.LambdaParameter(synthPos, None), gen.pattern.begin, gen.pattern.end)

        val truePos = posExpr(DesugaredPhase.Literal.True[Pos](), genBegin, genEnd)
        val falsePos = posExpr(DesugaredPhase.Literal.False[Pos](), genBegin, genEnd)
        val wildcardPos = posPat(DesugaredPhase.Pattern.Wildcard[Pos](), genBegin, genEnd)

        val case1 = posCase(DesugaredPhase.Case(desugarPosPattern(gen.pattern), None, truePos), gen.pattern.begin, genEnd)
        val case2 = posCase(DesugaredPhase.Case(wildcardPos, None, falsePos), genBegin, genEnd)
        val matchExpr = posExpr(DesugaredPhase.Match(synthRef, Vector(case1, case2)), genBegin, genEnd)
        val filterLambda = posExpr(DesugaredPhase.Lambda(Vector(synthParam), matchExpr), genBegin, genEnd)

        wrapWithFilter(initialTarget, filterLambda, genBegin, genEnd)
      } else {
        guards.foldLeft(initialTarget) { (accTarget, g) =>
          val guard = g.value.asInstanceOf[ParsedPhase.Enumerator.Guard[Pos]]
          val condExpr = desugarPosExpression(guard.condition)

          val synthId = nextSynthIdent()
          val synthPos = Pos(synthId, gen.pattern.begin, gen.pattern.end)
          val synthRef = posExpr(DesugaredPhase.Reference(synthPos), gen.pattern.begin, gen.pattern.end)
          val synthParam = posParam(DesugaredPhase.LambdaParameter(synthPos, None), gen.pattern.begin, gen.pattern.end)

          val truePos = posExpr(DesugaredPhase.Literal.True[Pos](), g.begin, g.end)
          val falsePos = posExpr(DesugaredPhase.Literal.False[Pos](), g.begin, g.end)
          val wildcardPos = posPat(DesugaredPhase.Pattern.Wildcard[Pos](), g.begin, g.end)

          val case1 = posCase(DesugaredPhase.Case(desugarPosPattern(gen.pattern), Some(condExpr), truePos), gen.pattern.begin, g.end)
          val case2 = posCase(DesugaredPhase.Case(wildcardPos, None, falsePos), g.begin, g.end)
          val matchExpr = posExpr(DesugaredPhase.Match(synthRef, Vector(case1, case2)), g.begin, g.end)
          val filterLambda = posExpr(DesugaredPhase.Lambda(Vector(synthParam), matchExpr), g.begin, g.end)

          wrapWithFilter(accTarget, filterLambda, g.begin, g.end)
        }
      }

      val mapSynthId = nextSynthIdent()
      val mapSynthPos = Pos(mapSynthId, gen.pattern.begin, gen.pattern.end)
      val mapSynthRef = posExpr(DesugaredPhase.Reference(mapSynthPos), gen.pattern.begin, gen.pattern.end)
      val mapSynthParam = posParam(DesugaredPhase.LambdaParameter(mapSynthPos, None), gen.pattern.begin, gen.pattern.end)

      val valDecls = valueBindings.foldLeft(Vector.empty[DeclPos]) { (acc, vb) =>
        val b = vb.value.asInstanceOf[ParsedPhase.Enumerator.ValueBinding[Pos]]
        acc :+ posDecl(
          DesugaredPhase.Declaration.Val(
            desugarPosPattern(b.pattern),
            desugarPosExpression(b.expression)
          ),
          vb.begin,
          vb.end
        )
      }

      val caseBody = if (valDecls.nonEmpty) {
        posExpr(DesugaredPhase.Block(valDecls, innerResult), valDecls.head.begin, innerResult.end)
      } else {
        innerResult
      }

      val matchCase = posCase(DesugaredPhase.Case(desugarPosPattern(gen.pattern), None, caseBody), gen.pattern.begin, caseBody.end)
      val matchExpr = posExpr(DesugaredPhase.Match(mapSynthRef, Vector(matchCase)), mapSynthRef.begin, caseBody.end)
      val lambda = posExpr(DesugaredPhase.Lambda(Vector(mapSynthParam), matchExpr), gen.pattern.begin, spanEnd)

      (filteredTarget, lambda)
    }

    def wrapWithFilter(target: ExprPos,
                       filterLambda: ExprPos,
                       filterBegin: CharIndex,
                       filterEnd: CharIndex): ExprPos = {
      val filterMethodPos = Pos(Identifiers.WithFilter, filterBegin, filterEnd)
      val select = posExpr(DesugaredPhase.Select(target, filterMethodPos), target.begin, filterEnd)
      val arg = posArg(DesugaredPhase.Argument(filterLambda, None), filterLambda.begin, filterLambda.end)
      val argGroup = posArgGroup(DesugaredPhase.ArgumentGroup(Vector(arg), None), filterLambda.begin, filterLambda.end)
      posExpr(
        DesugaredPhase.Call.Standard(select, Vector.empty, Vector(argGroup)),
        target.begin,
        filterEnd
      )
    }

    val spanBegin = if (enumerators.nonEmpty) enumerators.head.begin else begin
    val spanEnd = end
    desugarEnums(enumerators, spanBegin, spanEnd).value
  }

  private def desugarLiteral(lit: ParsedPhase.Literal[Pos]): DesugaredPhase.Literal[Pos] =
    lit match {
      case ParsedPhase.Literal.IntLiteral(v) => DesugaredPhase.Literal.IntLiteral(v)
      case ParsedPhase.Literal.LongLiteral(v) => DesugaredPhase.Literal.LongLiteral(v)
      case ParsedPhase.Literal.FloatLiteral(v) => DesugaredPhase.Literal.FloatLiteral(v)
      case ParsedPhase.Literal.DoubleLiteral(v) => DesugaredPhase.Literal.DoubleLiteral(v)
      case ParsedPhase.Literal.True() => DesugaredPhase.Literal.True()
      case ParsedPhase.Literal.False() => DesugaredPhase.Literal.False()
      case ParsedPhase.Literal.Null() => DesugaredPhase.Literal.Null()
      case ParsedPhase.Literal.CharLiteral(v) => DesugaredPhase.Literal.CharLiteral(v)
      case ParsedPhase.Literal.StringLiteral(v) => DesugaredPhase.Literal.StringLiteral(v)
      case ParsedPhase.Literal.UnitLiteral() => DesugaredPhase.Literal.UnitLiteral()
    }

  private def desugarPosDeclaration(posDecl: Pos[ParsedPhase.Declaration[Pos]]): Pos[DesugaredPhase.Declaration[Pos]] =
    Pos(desugarDeclaration(posDecl.value), posDecl.begin, posDecl.end)

  private def desugarDeclaration(decl: ParsedPhase.Declaration[Pos]): DesugaredPhase.Declaration[Pos] =
    decl match {
      case ParsedPhase.Declaration.Val(pattern, rhs) =>
        DesugaredPhase.Declaration.Val(desugarPosPattern(pattern), desugarPosExpression(rhs))

      case ParsedPhase.Declaration.LazyVal(pattern, rhs) =>
        DesugaredPhase.Declaration.LazyVal(desugarPosPattern(pattern), desugarPosExpression(rhs))

      case ParsedPhase.Declaration.Def(name, params, returnType, body) =>
        DesugaredPhase.Declaration.Def(
          name,
          params.foldLeft(Vector.empty[Pos[DesugaredPhase.FormalParameterGroup[Pos]]]) { (acc, g) =>
            acc :+ desugarPosFormalParameterGroup(g)
          },
          returnType,
          desugarPosExpression(body)
        )

      case ParsedPhase.Declaration.Error(err) =>
        DesugaredPhase.Declaration.Error(err)
    }

  private def desugarPosPattern(posPat: Pos[ParsedPhase.Pattern[Pos]]): Pos[DesugaredPhase.Pattern[Pos]] =
    Pos(desugarPattern(posPat.value), posPat.begin, posPat.end)

  private def desugarPattern(pat: ParsedPhase.Pattern[Pos]): DesugaredPhase.Pattern[Pos] =
    pat match {
      case ParsedPhase.Pattern.Error(err) =>
        DesugaredPhase.Pattern.Error(err)

      case ParsedPhase.Pattern.Wildcard() =>
        DesugaredPhase.Pattern.Wildcard()

      case ParsedPhase.Pattern.Identifier(name) =>
        DesugaredPhase.Pattern.Identifier(name)

      case ParsedPhase.Pattern.Literal(lit) =>
        DesugaredPhase.Pattern.Literal(Pos(desugarLiteral(lit.value), lit.begin, lit.end))

      case ParsedPhase.Pattern.As(name, pattern) =>
        DesugaredPhase.Pattern.As(name, desugarPosPattern(pattern))

      case ParsedPhase.Pattern.Typed(pattern, ascription) =>
        DesugaredPhase.Pattern.Typed(desugarPosPattern(pattern), ascription)

      case ParsedPhase.Pattern.Tuple(elements) =>
        DesugaredPhase.Pattern.Tuple(
          elements.foldLeft(Vector.empty[Pos[DesugaredPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ desugarPosPattern(el)
          }
        )

      case ParsedPhase.Pattern.Product(typeId, patterns) =>
        DesugaredPhase.Pattern.Product(
          typeId,
          patterns.foldLeft(Vector.empty[Pos[DesugaredPhase.Pattern[Pos]]]) { (acc, p) =>
            acc :+ desugarPosPattern(p)
          }
        )
    }

  private def desugarPosCase(posCase: Pos[ParsedPhase.Case[Pos]]): Pos[DesugaredPhase.Case[Pos]] =
    Pos(desugarCase(posCase.value), posCase.begin, posCase.end)

  private def desugarCase(c: ParsedPhase.Case[Pos]): DesugaredPhase.Case[Pos] = {
    val guard = c.guard match {
      case Some(g) => Some(desugarPosExpression(g))
      case None => None
    }
    DesugaredPhase.Case(desugarPosPattern(c.pattern), guard, desugarPosExpression(c.body))
  }

  private def desugarPosTypeArgument(posTA: Pos[ParsedPhase.TypeArgument[Pos]]): Pos[DesugaredPhase.TypeArgument[Pos]] =
    Pos(DesugaredPhase.TypeArgument(posTA.value.typ), posTA.begin, posTA.end)

  private def desugarPosArgumentGroup(posAG: Pos[ParsedPhase.ArgumentGroup[Pos]]): Pos[DesugaredPhase.ArgumentGroup[Pos]] = {
    val arguments = posAG.value.arguments.foldLeft(Vector.empty[Pos[DesugaredPhase.Argument[Pos]]]) { (acc, a) =>
      acc :+ desugarPosArgument(a)
    }
    val splat = posAG.value.splat match {
      case Some(s) => Some(desugarPosArgument(s))
      case None => None
    }
    Pos(DesugaredPhase.ArgumentGroup(arguments, splat), posAG.begin, posAG.end)
  }

  private def desugarPosArgument(posArg: Pos[ParsedPhase.Argument[Pos]]): Pos[DesugaredPhase.Argument[Pos]] =
    Pos(
      DesugaredPhase.Argument(
        desugarPosExpression(posArg.value.value),
        posArg.value.name
      ),
      posArg.begin,
      posArg.end
    )

  private def desugarPosLambdaParameter(posLP: Pos[ParsedPhase.LambdaParameter[Pos]]): Pos[DesugaredPhase.LambdaParameter[Pos]] =
    Pos(DesugaredPhase.LambdaParameter(posLP.value.name, posLP.value.typ), posLP.begin, posLP.end)

  private def desugarPosFormalParameterGroup(posFPG: Pos[ParsedPhase.FormalParameterGroup[Pos]]): Pos[DesugaredPhase.FormalParameterGroup[Pos]] = {
    val parameters = posFPG.value.parameters.foldLeft(Vector.empty[Pos[DesugaredPhase.FormalParameter[Pos]]]) { (acc, p) =>
      acc :+ desugarPosFormalParameter(p)
    }
    val variadic = posFPG.value.variadic match {
      case Some(v) => Some(desugarPosFormalParameter(v))
      case None => None
    }
    Pos(DesugaredPhase.FormalParameterGroup(parameters, variadic), posFPG.begin, posFPG.end)
  }

  private def desugarPosFormalParameter(posFP: Pos[ParsedPhase.FormalParameter[Pos]]): Pos[DesugaredPhase.FormalParameter[Pos]] = {
    val default = posFP.value.default match {
      case Some(d) => Some(desugarPosExpression(d))
      case None => None
    }
    Pos(DesugaredPhase.FormalParameter(posFP.value.name, posFP.value.typ, default), posFP.begin, posFP.end)
  }
}
