package software.kes.scaletta.internal.desugar

import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.reporting.Pos

object Desugarer {
  type Input = ParsedPhase.Expression[Pos]
  type Output = DesugaredPhase.Expression[Pos]

  def create(): Desugarer = new Desugarer()
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
              acc :+ ((desugarPosExpression(exprPart), textPart))
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
