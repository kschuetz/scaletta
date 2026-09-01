package software.kes.scaletta.internal.preresolver

import software.kes.scaletta.api._
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.builtins.{FunctionSymbolTable, NativeFunctionDefinition, OverloadTable}
import software.kes.scaletta.internal.preresolver.PreResolver.{Input, Output}
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.symbols.SymbolEntry

object PreResolver {
  def create(symbolTable: FunctionSymbolTable,
             importScope: ImportScope): PreResolver =
    new PreResolver(symbolTable, importScope)

  type Input = ParsingPhase.Expression[Pos]
  type Output = PreResolutionPhase.Expression[Pos]
}

/**
 * Performs preliminary name resolution and AST tagging between the parsing phase
 * and full type checking / linking. Identifiers are tagged with PreResolutionInfo
 * metadata to distinguish lexical bindings, member/static call sites, and global symbols.
 */
final class PreResolver private(symbolTable: FunctionSymbolTable,
                                importScope: ImportScope) {

  def preResolve(input: Input): Output =
    preResolveExpression(input, Set.empty)

  private def preResolvePosExpression(posExpr: Pos[ParsingPhase.Expression[Pos]],
                                      env: Set[String]): Pos[PreResolutionPhase.Expression[Pos]] =
    Pos(preResolveExpression(posExpr.value, env), posExpr.begin, posExpr.end)

  private def preResolveExpression(expr: ParsingPhase.Expression[Pos],
                                   env: Set[String]): PreResolutionPhase.Expression[Pos] = {
    expr match {
      case ParsingPhase.Expression.Error(err) =>
        PreResolutionPhase.Expression.Error(err)

      case lit: ParsingPhase.Literal[Pos @unchecked] =>
        preResolveLiteral(lit)

      case ParsingPhase.Reference(id) =>
        val name = id.value.name
        if (env.contains(name)) {
          PreResolutionPhase.Reference(posIdent(PreResolutionInfo.Unresolved(name), id.begin, id.end))
        } else {
          val resolvedIdent = resolveUnqualifiedTerm(name, id.begin, id.end, None)
          PreResolutionPhase.Reference(resolvedIdent)
        }

      case ParsingPhase.Select(qualifier, name) =>
        PreResolutionPhase.Select(
          preResolvePosExpression(qualifier, env),
          posIdent(PreResolutionInfo.FieldSite(name.value.name), name.begin, name.end)
        )

      case ParsingPhase.Typed(expression, ascription) =>
        PreResolutionPhase.Typed(
          preResolvePosExpression(expression, env),
          ascription
        )

      case ParsingPhase.Tuple(elements) =>
        PreResolutionPhase.Tuple(
          elements.foldLeft(Vector.empty[Pos[PreResolutionPhase.Expression[Pos]]]) { (acc, el) =>
            acc :+ preResolvePosExpression(el, env)
          }
        )

      case ParsingPhase.Conditional(condition, thenBranch, elseBranch) =>
        PreResolutionPhase.Conditional(
          preResolvePosExpression(condition, env),
          preResolvePosExpression(thenBranch, env),
          preResolvePosExpression(elseBranch, env)
        )

      case ParsingPhase.Call.Infix(left, operation, typeArgs, right) =>
        PreResolutionPhase.Call.Infix(
          preResolvePosExpression(left, env),
          posIdent(PreResolutionInfo.MethodSite(operation.value.name, Vector(1)), operation.begin, operation.end),
          preResolveTypeArgs(typeArgs),
          preResolvePosExpression(right, env)
        )

      case ParsingPhase.Call.Standard(target, typeArgs, args) =>
        val arities = args.foldLeft(Vector.empty[Int]) { (acc, ag) =>
          acc :+ ag.value.arguments.size
        }
        val newArgs = args.foldLeft(Vector.empty[Pos[PreResolutionPhase.ArgumentGroup[Pos]]]) { (acc, ag) =>
          acc :+ preResolveArgumentGroup(ag, env)
        }
        val newTarget = preResolveCallTarget(target, arities, env)
        PreResolutionPhase.Call.Standard(newTarget, preResolveTypeArgs(typeArgs), newArgs)

      case ParsingPhase.Lambda(params, body) =>
        val paramNames = params.foldLeft(Set.empty[String]) { (acc, p) =>
          acc + p.value.name.value.name
        }
        val newParams = params.foldLeft(Vector.empty[Pos[PreResolutionPhase.LambdaParameter[Pos]]]) { (acc, p) =>
          val newIdent = posIdent(PreResolutionInfo.Unresolved(p.value.name.value.name), p.value.name.begin, p.value.name.end)
          acc :+ Pos(PreResolutionPhase.LambdaParameter(newIdent, p.value.typ), p.begin, p.end)
        }
        val bodyEnv = env ++ paramNames
        val newBody = preResolvePosExpression(body, bodyEnv)
        PreResolutionPhase.Lambda(newParams, newBody)

      case ParsingPhase.InterpolatedString(interpolator, initial, segments) =>
        val newSegments = segments.foldLeft(Vector.empty[(Pos[PreResolutionPhase.Expression[Pos]], String)]) {
          case (acc, (exprPart, textPart)) =>
            acc :+ (preResolvePosExpression(exprPart, env), textPart)
        }
        PreResolutionPhase.InterpolatedString(interpolator, initial, newSegments)

      case ParsingPhase.Match(expression, cases) =>
        val newExpr = preResolvePosExpression(expression, env)
        val newCases = cases.foldLeft(Vector.empty[Pos[PreResolutionPhase.Case[Pos]]]) { (acc, c) =>
          acc :+ preResolveCase(c, env)
        }
        PreResolutionPhase.Match(newExpr, newCases)

      case ParsingPhase.Block(declarations, result) =>
        val (finalEnv, newDecls) = declarations.foldLeft((env, Vector.empty[Pos[PreResolutionPhase.Declaration[Pos]]])) {
          case ((currentEnv, accDecls), decl) =>
            val (updatedEnv, newDecl: Pos[PreResolutionPhase.Declaration[Pos]]) = decl.value match {
              case ParsingPhase.Declaration.Val(pattern, rhs) =>
                val newRhs = preResolvePosExpression(rhs, currentEnv)
                val newPattern = preResolvePattern(pattern)
                val bindings = collectPatternBindings(pattern.value)
                val newEnv = currentEnv ++ bindings
                val d: Pos[PreResolutionPhase.Declaration[Pos]] = Pos(PreResolutionPhase.Declaration.Val(newPattern, newRhs), decl.begin, decl.end)
                (newEnv, d)

              case ParsingPhase.Declaration.LazyVal(pattern, rhs) =>
                val newRhs = preResolvePosExpression(rhs, currentEnv)
                val newPattern = preResolvePattern(pattern)
                val bindings = collectPatternBindings(pattern.value)
                val newEnv = currentEnv ++ bindings
                val d: Pos[PreResolutionPhase.Declaration[Pos]] = Pos(PreResolutionPhase.Declaration.LazyVal(newPattern, newRhs), decl.begin, decl.end)
                (newEnv, d)

              case ParsingPhase.Declaration.Def(name, params, returnType, body) =>
                val newName = posIdent(PreResolutionInfo.Unresolved(name.value.name), name.begin, name.end)
                val paramNames = params.foldLeft(Set.empty[String]) { (acc, g) =>
                  val regularNames = g.value.parameters.foldLeft(Set.empty[String]) { (pAcc, p) =>
                    pAcc + p.value.name.value.name
                  }
                  val variadicName = g.value.variadic.fold(Set.empty[String])(v => Set(v.value.name.value.name))
                  acc ++ regularNames ++ variadicName
                }
                val defBodyEnv = currentEnv ++ paramNames
                val newParams = preResolveFormalParameterGroups(params, currentEnv)
                val newBody = preResolvePosExpression(body, defBodyEnv)
                val d: Pos[PreResolutionPhase.Declaration[Pos]] = Pos(PreResolutionPhase.Declaration.Def(newName, newParams, returnType, newBody), decl.begin, decl.end)
                val newEnv = currentEnv + name.value.name
                (newEnv, d)

              case ParsingPhase.Declaration.Error(err) =>
                val d: Pos[PreResolutionPhase.Declaration[Pos]] = Pos(PreResolutionPhase.Declaration.Error[Pos](err), decl.begin, decl.end)
                (currentEnv, d)
            }
            (updatedEnv, accDecls :+ newDecl)
        }
        val newResult = preResolvePosExpression(result, finalEnv)
        PreResolutionPhase.Block(newDecls, newResult)
    }
  }

  private def preResolveCallTarget(target: Pos[ParsingPhase.Expression[Pos]],
                                   arities: Vector[Int],
                                   env: Set[String]): Pos[PreResolutionPhase.Expression[Pos]] = {
    target.value match {
      case ParsingPhase.Reference(id) =>
        val name = id.value.name
        if (env.contains(name)) {
          Pos(
            PreResolutionPhase.Reference(posIdent(PreResolutionInfo.Unresolved(name), id.begin, id.end)),
            target.begin,
            target.end
          )
        } else {
          val resolvedIdent = resolveUnqualifiedTerm(name, id.begin, id.end, Some(arities))
          Pos(PreResolutionPhase.Reference(resolvedIdent), target.begin, target.end)
        }

      case ParsingPhase.Select(qualifier, name) =>
        extractQualifiedName(qualifier.value, env) match {
          case Some(segments) =>
            val fullPathStr = (segments :+ name.value.name).mkString(".")
            val tryParsed = QualifiedName.tryParsePartial(fullPathStr).fold(
              _ => QualifiedName.tryParseFull(fullPathStr).toOption,
              p => Some(p)
            )
            tryParsed match {
              case Some(qName) =>
                val matches = symbolTable.resolveStaticFunction(qName, importScope)
                if (matches.nonEmpty) {
                  val resolvedIdent = resolveStaticMatches(name.value.name, matches, name.begin, name.end, Some(arities))
                  Pos(
                    PreResolutionPhase.Select(preResolvePosExpression(qualifier, env), resolvedIdent),
                    target.begin,
                    target.end
                  )
                } else {
                  Pos(
                    PreResolutionPhase.Select(
                      preResolvePosExpression(qualifier, env),
                      posIdent(PreResolutionInfo.MethodSite(name.value.name, arities), name.begin, name.end)
                    ),
                    target.begin,
                    target.end
                  )
                }
              case None =>
                Pos(
                  PreResolutionPhase.Select(
                    preResolvePosExpression(qualifier, env),
                    posIdent(PreResolutionInfo.MethodSite(name.value.name, arities), name.begin, name.end)
                  ),
                  target.begin,
                  target.end
                )
            }

          case None =>
            Pos(
              PreResolutionPhase.Select(
                preResolvePosExpression(qualifier, env),
                posIdent(PreResolutionInfo.MethodSite(name.value.name, arities), name.begin, name.end)
              ),
              target.begin,
              target.end
            )
        }

      case _ =>
        preResolvePosExpression(target, env)
    }
  }

  private def preResolveTypeArgs(typeArgs: Vector[Pos[ParsingPhase.TypeArgument[Pos]]]): Vector[Pos[PreResolutionPhase.TypeArgument[Pos]]] =
    typeArgs.foldLeft(Vector.empty[Pos[PreResolutionPhase.TypeArgument[Pos]]]) { (acc, ta) =>
      acc :+ Pos(PreResolutionPhase.TypeArgument(ta.value.typ), ta.begin, ta.end)
    }

  private def extractQualifiedName(expr: ParsingPhase.Expression[Pos],
                                   env: Set[String]): Option[Vector[String]] = {
    expr match {
      case ParsingPhase.Reference(id) if !env.contains(id.value.name) =>
        Some(Vector(id.value.name))
      case ParsingPhase.Select(q, n) =>
        extractQualifiedName(q.value, env) match {
          case Some(segments) => Some(segments :+ n.value.name)
          case None => None
        }
      case _ =>
        None
    }
  }

  private def resolveUnqualifiedTerm(name: String,
                                     begin: CharIndex,
                                     end: CharIndex,
                                     callArity: Option[Vector[Int]]): Pos[PreResolutionPhase.Ident[Pos]] = {
    val qName = QualifiedName.tryParsePartial(name).fold(_ => QualifiedName.local(Name(name)), identity)
    val matches = symbolTable.resolveStaticFunction(qName, importScope)
    resolveStaticMatches(name, matches, begin, end, callArity)
  }

  private def resolveStaticMatches(name: String,
                                   matches: List[SymbolEntry[OverloadTable]],
                                   begin: CharIndex,
                                   end: CharIndex,
                                   callArity: Option[Vector[Int]]): Pos[PreResolutionPhase.Ident[Pos]] = {
    val info: PreResolutionInfo = matches match {
      case Nil =>
        PreResolutionInfo.Unresolved(name)

      case entry :: Nil =>
        val overloads = entry.value.variations
        callArity match {
          case Some(arities) =>
            val matchingArity = overloads.foldLeft(List.empty[NativeFunctionDefinition]) { (acc, v) =>
              val matchesArity = v.paramGroups.size == arities.size &&
                v.paramGroups.zip(arities).forall { case (pg, arity) => pg.params.size == arity }
              if (matchesArity) acc :+ v else acc
            }
            matchingArity match {
              case single :: Nil if overloads.size == 1 =>
                PreResolutionInfo.Bound(software.kes.scaletta.internal.ast.NativeFunctionId(single.nativeFunctionId.value.toLong))
              case _ =>
                val qualifiedNameStr = entry.container.fold(entry.name.value)(c => s"${c}.${entry.name.value}")
                PreResolutionInfo.StaticFunctionSite(qualifiedNameStr, arities)
            }

          case None =>
            if (overloads.size == 1) {
              PreResolutionInfo.Bound(software.kes.scaletta.internal.ast.NativeFunctionId(overloads.head.nativeFunctionId.value.toLong))
            } else {
              val qualifiedNameStr = entry.container.fold(entry.name.value)(c => s"${c}.${entry.name.value}")
              PreResolutionInfo.StaticFunctionSite(qualifiedNameStr, Vector.empty)
            }
        }

      case multiple =>
        PreResolutionInfo.AmbiguousName(name, multiple.size)
    }
    posIdent(info, begin, end)
  }

  private def preResolveArgumentGroup(group: Pos[ParsingPhase.ArgumentGroup[Pos]],
                                      env: Set[String]): Pos[PreResolutionPhase.ArgumentGroup[Pos]] = {
    val newArgs = group.value.arguments.foldLeft(Vector.empty[Pos[PreResolutionPhase.Argument[Pos]]]) { (acc, arg) =>
      val newValue = preResolvePosExpression(arg.value.value, env)
      val newName = arg.value.name.fold[Option[Pos[PreResolutionPhase.Ident[Pos]]]](None) { n =>
        Some(posIdent(PreResolutionInfo.Unresolved(n.value.name), n.begin, n.end))
      }
      acc :+ Pos(PreResolutionPhase.Argument(newValue, newName), arg.begin, arg.end)
    }
    val newSplat = group.value.splat.fold[Option[Pos[PreResolutionPhase.Argument[Pos]]]](None) { splat =>
      val newValue = preResolvePosExpression(splat.value.value, env)
      val newName = splat.value.name.fold[Option[Pos[PreResolutionPhase.Ident[Pos]]]](None) { n =>
        Some(posIdent(PreResolutionInfo.Unresolved(n.value.name), n.begin, n.end))
      }
      Some(Pos(PreResolutionPhase.Argument(newValue, newName), splat.begin, splat.end))
    }
    Pos(PreResolutionPhase.ArgumentGroup(newArgs, newSplat), group.begin, group.end)
  }

  private def preResolveFormalParameterGroups(groups: Vector[Pos[ParsingPhase.FormalParameterGroup[Pos]]],
                                              env: Set[String]): Vector[Pos[PreResolutionPhase.FormalParameterGroup[Pos]]] = {
    groups.foldLeft(Vector.empty[Pos[PreResolutionPhase.FormalParameterGroup[Pos]]]) { (acc, g) =>
      val newParams = g.value.parameters.foldLeft(Vector.empty[Pos[PreResolutionPhase.FormalParameter[Pos]]]) { (pAcc, p) =>
        val newName = posIdent(PreResolutionInfo.Unresolved(p.value.name.value.name), p.value.name.begin, p.value.name.end)
        val newDefault = p.value.default.fold[Option[Pos[PreResolutionPhase.Expression[Pos]]]](None) { d =>
          Some(preResolvePosExpression(d, env))
        }
        pAcc :+ Pos(PreResolutionPhase.FormalParameter(newName, p.value.typ, newDefault), p.begin, p.end)
      }
      val newVariadic = g.value.variadic.fold[Option[Pos[PreResolutionPhase.FormalParameter[Pos]]]](None) { v =>
        val newName = posIdent(PreResolutionInfo.Unresolved(v.value.name.value.name), v.value.name.begin, v.value.name.end)
        val newDefault = v.value.default.fold[Option[Pos[PreResolutionPhase.Expression[Pos]]]](None) { d =>
          Some(preResolvePosExpression(d, env))
        }
        Some(Pos(PreResolutionPhase.FormalParameter(newName, v.value.typ, newDefault), v.begin, v.end))
      }
      acc :+ Pos(PreResolutionPhase.FormalParameterGroup(newParams, newVariadic), g.begin, g.end)
    }
  }

  private def preResolveCase(c: Pos[ParsingPhase.Case[Pos]],
                             env: Set[String]): Pos[PreResolutionPhase.Case[Pos]] = {
    val newPattern = preResolvePattern(c.value.pattern)
    val bindings = collectPatternBindings(c.value.pattern.value)
    val caseEnv = env ++ bindings
    val newGuard = c.value.guard.fold[Option[Pos[PreResolutionPhase.Expression[Pos]]]](None) { g =>
      Some(preResolvePosExpression(g, caseEnv))
    }
    val newBody = preResolvePosExpression(c.value.body, caseEnv)
    Pos(PreResolutionPhase.Case(newPattern, newGuard, newBody), c.begin, c.end)
  }

  private def preResolvePattern(pattern: Pos[ParsingPhase.Pattern[Pos]]): Pos[PreResolutionPhase.Pattern[Pos]] = {
    val resolved: PreResolutionPhase.Pattern[Pos] = pattern.value match {
      case ParsingPhase.Pattern.Identifier(name) =>
        PreResolutionPhase.Pattern.Identifier(posIdent(PreResolutionInfo.Unresolved(name.value.name), name.begin, name.end))

      case ParsingPhase.Pattern.Wildcard() =>
        PreResolutionPhase.Pattern.Wildcard()

      case ParsingPhase.Pattern.Literal(value) =>
        val lit = preResolveLiteral(value.value)
        PreResolutionPhase.Pattern.Literal(Pos(lit, value.begin, value.end))

      case ParsingPhase.Pattern.As(name, pat) =>
        PreResolutionPhase.Pattern.As(
          posIdent(PreResolutionInfo.Unresolved(name.value.name), name.begin, name.end),
          preResolvePattern(pat)
        )

      case ParsingPhase.Pattern.Typed(pat, ascription) =>
        PreResolutionPhase.Pattern.Typed(preResolvePattern(pat), ascription)

      case ParsingPhase.Pattern.Tuple(elements) =>
        PreResolutionPhase.Pattern.Tuple(
          elements.foldLeft(Vector.empty[Pos[PreResolutionPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ preResolvePattern(el)
          }
        )

      case ParsingPhase.Pattern.Product(typeId, args) =>
        PreResolutionPhase.Pattern.Product(
          typeId,
          args.foldLeft(Vector.empty[Pos[PreResolutionPhase.Pattern[Pos]]]) { (acc, arg) =>
            acc :+ preResolvePattern(arg)
          }
        )

      case ParsingPhase.Pattern.Error(error) =>
        PreResolutionPhase.Pattern.Error(error)
    }
    Pos(resolved, pattern.begin, pattern.end)
  }

  private def preResolveLiteral(lit: ParsingPhase.Literal[Pos]): PreResolutionPhase.Literal[Pos] = {
    lit match {
      case ParsingPhase.Literal.IntLiteral(v) => PreResolutionPhase.Literal.IntLiteral(v)
      case ParsingPhase.Literal.LongLiteral(v) => PreResolutionPhase.Literal.LongLiteral(v)
      case ParsingPhase.Literal.FloatLiteral(v) => PreResolutionPhase.Literal.FloatLiteral(v)
      case ParsingPhase.Literal.DoubleLiteral(v) => PreResolutionPhase.Literal.DoubleLiteral(v)
      case ParsingPhase.Literal.True() => PreResolutionPhase.Literal.True()
      case ParsingPhase.Literal.False() => PreResolutionPhase.Literal.False()
      case ParsingPhase.Literal.Null() => PreResolutionPhase.Literal.Null()
      case ParsingPhase.Literal.CharLiteral(v) => PreResolutionPhase.Literal.CharLiteral(v)
      case ParsingPhase.Literal.StringLiteral(v) => PreResolutionPhase.Literal.StringLiteral(v)
      case ParsingPhase.Literal.UnitLiteral() => PreResolutionPhase.Literal.UnitLiteral()
    }
  }

  private def collectPatternBindings(pat: ParsingPhase.Pattern[Pos]): Set[String] = {
    pat match {
      case ParsingPhase.Pattern.Identifier(name) =>
        Set(name.value.name)

      case ParsingPhase.Pattern.As(name, inner) =>
        collectPatternBindings(inner.value) + name.value.name

      case ParsingPhase.Pattern.Typed(inner, _) =>
        collectPatternBindings(inner.value)

      case ParsingPhase.Pattern.Tuple(elements) =>
        elements.foldLeft(Set.empty[String]) { (acc, el) =>
          acc ++ collectPatternBindings(el.value)
        }

      case ParsingPhase.Pattern.Product(_, args) =>
        args.foldLeft(Set.empty[String]) { (acc, arg) =>
          acc ++ collectPatternBindings(arg.value)
        }

      case ParsingPhase.Pattern.Wildcard() =>
        Set.empty

      case ParsingPhase.Pattern.Literal(_) =>
        Set.empty

      case ParsingPhase.Pattern.Error(_) =>
        Set.empty
    }
  }

  private def posIdent(info: PreResolutionInfo,
                       begin: CharIndex,
                       end: CharIndex): Pos[PreResolutionPhase.Ident[Pos]] =
    Pos(Pos[PreResolutionInfo](info, begin, end), begin, end)
}
