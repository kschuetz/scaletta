package software.kes.scaletta.internal.semantics

import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.semantics.Linker.{Input, Output}

object Linker {
  def create(): Linker = new Linker()

  type Input = TypeAscriptionPhase.Expression[Pos]
  type Output = LinkedPhase.Expression[Pos]
}

/**
 * Traverses the AST in a single linear pass to resolve lexical scoping and coordinate
 * variable slot assignments. Enforces strict forward-reference checks within blocks,
 * binds local variables to (depth, slot) coordinates, and detects semantic scoping errors.
 */
final class Linker private() {

  def link(input: Input): Output = linkExpression(input, LexicalScope.empty, Set.empty)

  private def linkPosExpression(posExpr: Pos[TypeAscriptionPhase.Expression[Pos]],
                                env: LexicalScope,
                                futureInBlock: Set[String]): Pos[LinkedPhase.Expression[Pos]] =
    Pos(linkExpression(posExpr.value, env, futureInBlock), posExpr.begin, posExpr.end)

  private def linkExpression(expr: TypeAscriptionPhase.Expression[Pos],
                             env: LexicalScope,
                             futureInBlock: Set[String]): LinkedPhase.Expression[Pos] = {
    expr match {
      case TypeAscriptionPhase.Expression.Error(err) =>
        LinkedPhase.Expression.Error(err)

      case lit: TypeAscriptionPhase.Literal[Pos @unchecked] =>
        linkLiteral(lit)

      case TypeAscriptionPhase.Reference(id) =>
        LinkedPhase.Reference(linkIdentifier(id, env, futureInBlock))

      case TypeAscriptionPhase.Select(qualifier, name) =>
        LinkedPhase.Select(linkPosExpression(qualifier, env, futureInBlock), linkFieldSite(name))

      case TypeAscriptionPhase.Typed(expression, ascription) =>
        LinkedPhase.Typed(linkPosExpression(expression, env, futureInBlock), ascription)

      case TypeAscriptionPhase.Tuple(elements) =>
        LinkedPhase.Tuple(
          elements.foldLeft(Vector.empty[Pos[LinkedPhase.Expression[Pos]]]) { (acc, el) =>
            acc :+ linkPosExpression(el, env, futureInBlock)
          }
        )

      case TypeAscriptionPhase.Conditional(condition, thenBranch, elseBranch) =>
        LinkedPhase.Conditional(
          linkPosExpression(condition, env, futureInBlock),
          linkPosExpression(thenBranch, env, futureInBlock),
          linkPosExpression(elseBranch, env, futureInBlock)
        )

      case TypeAscriptionPhase.Call.Infix(left, operation, typeArgs, right) =>
        LinkedPhase.Call.Infix(
          linkPosExpression(left, env, futureInBlock),
          linkMethodSite(operation),
          linkTypeArgs(typeArgs),
          linkPosExpression(right, env, futureInBlock)
        )

      case TypeAscriptionPhase.Call.Standard(target, typeArgs, args) =>
        LinkedPhase.Call.Standard(
          linkPosExpression(target, env, futureInBlock),
          linkTypeArgs(typeArgs),
          linkArgumentGroups(args, env, futureInBlock)
        )

      case TypeAscriptionPhase.Lambda(params, body) =>
        linkLambda(params, body, env)

      case TypeAscriptionPhase.InterpolatedString(interpolator, initial, segments) =>
        val newSegments = segments.foldLeft(Vector.empty[(Pos[LinkedPhase.Expression[Pos]], String)]) {
          case (acc, (exprPart, textPart)) =>
            acc :+ (linkPosExpression(exprPart, env, futureInBlock), textPart)
        }
        LinkedPhase.InterpolatedString(interpolator, initial, newSegments)

      case TypeAscriptionPhase.Match(expression, cases) =>
        LinkedPhase.Match(linkPosExpression(expression, env, futureInBlock), linkCases(cases, env))

      case TypeAscriptionPhase.Block(declarations, result) =>
        linkBlock(declarations, result, env)
    }
  }

  private def linkIdentifier(id: Pos[TypeAscriptionPhase.Ident[Pos]],
                             env: LexicalScope,
                             futureInBlock: Set[String]): Pos[LinkedPhase.Ident[Pos]] = {
    val info = id.value.value match {
      case PreResolutionInfo.Unresolved(name) =>
        env.lookup(name) match {
          case Some((depth, binding)) =>
            LinkedInfo.Local(depth, binding.slot, name, binding.kind)
          case None =>
            if (futureInBlock.contains(name)) {
              LinkedInfo.Error(SemanticError.ForwardReference(name))
            } else {
              LinkedInfo.Error(SemanticError.UnknownIdentifier(name))
            }
        }

      case PreResolutionInfo.Bound(symbol) =>
        LinkedInfo.Bound(symbol)

      case PreResolutionInfo.MethodSite(name, arity) =>
        LinkedInfo.MethodSite(name, arity)

      case PreResolutionInfo.StaticFunctionSite(qname, arity) =>
        LinkedInfo.StaticFunctionSite(qname, arity)

      case PreResolutionInfo.FieldSite(name) =>
        LinkedInfo.FieldSite(name)

      case PreResolutionInfo.AmbiguousName(name, count) =>
        LinkedInfo.AmbiguousName(name, count)
    }
    posIdent(info, id.begin, id.end)
  }

  private def linkMethodSite(op: Pos[TypeAscriptionPhase.Ident[Pos]]): Pos[LinkedPhase.Ident[Pos]] = {
    val info = op.value.value match {
      case PreResolutionInfo.MethodSite(name, arity) => LinkedInfo.MethodSite(name, arity)
      case PreResolutionInfo.StaticFunctionSite(qname, arity) => LinkedInfo.StaticFunctionSite(qname, arity)
      case PreResolutionInfo.Bound(symbol) => LinkedInfo.Bound(symbol)
      case PreResolutionInfo.Unresolved(name) => LinkedInfo.MethodSite(name, Vector(1))
      case PreResolutionInfo.FieldSite(name) => LinkedInfo.FieldSite(name)
      case PreResolutionInfo.AmbiguousName(name, count) => LinkedInfo.AmbiguousName(name, count)
    }
    posIdent(info, op.begin, op.end)
  }

  private def linkFieldSite(name: Pos[TypeAscriptionPhase.Ident[Pos]]): Pos[LinkedPhase.Ident[Pos]] = {
    val info = name.value.value match {
      case PreResolutionInfo.FieldSite(n) => LinkedInfo.FieldSite(n)
      case PreResolutionInfo.Unresolved(n) => LinkedInfo.FieldSite(n)
      case PreResolutionInfo.Bound(symbol) => LinkedInfo.Bound(symbol)
      case PreResolutionInfo.MethodSite(n, arity) => LinkedInfo.MethodSite(n, arity)
      case PreResolutionInfo.StaticFunctionSite(qname, arity) => LinkedInfo.StaticFunctionSite(qname, arity)
      case PreResolutionInfo.AmbiguousName(n, count) => LinkedInfo.AmbiguousName(n, count)
    }
    posIdent(info, name.begin, name.end)
  }

  private def linkLiteral(lit: TypeAscriptionPhase.Literal[Pos]): LinkedPhase.Literal[Pos] = {
    lit match {
      case TypeAscriptionPhase.Literal.IntLiteral(v) => LinkedPhase.Literal.IntLiteral(v)
      case TypeAscriptionPhase.Literal.LongLiteral(v) => LinkedPhase.Literal.LongLiteral(v)
      case TypeAscriptionPhase.Literal.FloatLiteral(v) => LinkedPhase.Literal.FloatLiteral(v)
      case TypeAscriptionPhase.Literal.DoubleLiteral(v) => LinkedPhase.Literal.DoubleLiteral(v)
      case TypeAscriptionPhase.Literal.True() => LinkedPhase.Literal.True()
      case TypeAscriptionPhase.Literal.False() => LinkedPhase.Literal.False()
      case TypeAscriptionPhase.Literal.Null() => LinkedPhase.Literal.Null()
      case TypeAscriptionPhase.Literal.CharLiteral(v) => LinkedPhase.Literal.CharLiteral(v)
      case TypeAscriptionPhase.Literal.StringLiteral(v) => LinkedPhase.Literal.StringLiteral(v)
      case TypeAscriptionPhase.Literal.UnitLiteral() => LinkedPhase.Literal.UnitLiteral()
    }
  }

  private def linkTypeArgs(typeArgs: Vector[Pos[TypeAscriptionPhase.TypeArgument[Pos]]]): Vector[Pos[LinkedPhase.TypeArgument[Pos]]] =
    typeArgs.foldLeft(Vector.empty[Pos[LinkedPhase.TypeArgument[Pos]]]) { (acc, ta) =>
      acc :+ Pos(LinkedPhase.TypeArgument[Pos](ta.value.typ), ta.begin, ta.end)
    }

  private def linkArgumentGroups(groups: Vector[Pos[TypeAscriptionPhase.ArgumentGroup[Pos]]],
                                 env: LexicalScope,
                                 futureInBlock: Set[String]): Vector[Pos[LinkedPhase.ArgumentGroup[Pos]]] = {
    groups.foldLeft(Vector.empty[Pos[LinkedPhase.ArgumentGroup[Pos]]]) { (acc, ag) =>
      val newArgs = ag.value.arguments.foldLeft(Vector.empty[Pos[LinkedPhase.Argument[Pos]]]) { (aAcc, arg) =>
        val linkedValue = linkPosExpression(arg.value.value, env, futureInBlock)
        val linkedName = arg.value.name.fold[Option[Pos[LinkedPhase.Ident[Pos]]]](None) { n =>
          Some(linkIdentifier(n, env, futureInBlock))
        }
        aAcc :+ Pos(LinkedPhase.Argument(linkedValue, linkedName), arg.begin, arg.end)
      }
      val newSplat = ag.value.splat.fold[Option[Pos[LinkedPhase.Argument[Pos]]]](None) { s =>
        val linkedValue = linkPosExpression(s.value.value, env, futureInBlock)
        val linkedName = s.value.name.fold[Option[Pos[LinkedPhase.Ident[Pos]]]](None) { n =>
          Some(linkIdentifier(n, env, futureInBlock))
        }
        Some(Pos(LinkedPhase.Argument(linkedValue, linkedName), s.begin, s.end))
      }
      acc :+ Pos(LinkedPhase.ArgumentGroup(newArgs, newSplat), ag.begin, ag.end)
    }
  }

  private def linkLambda(params: Vector[Pos[TypeAscriptionPhase.LambdaParameter[Pos]]],
                         body: Pos[TypeAscriptionPhase.Expression[Pos]],
                         env: LexicalScope): LinkedPhase.Lambda[Pos] = {
    case class Acc(seen: Set[String],
                   bindings: Vector[ScopeBinding],
                   linkedParams: Vector[Pos[LinkedPhase.LambdaParameter[Pos]]])

    val initial = Acc(Set.empty, Vector.empty, Vector.empty)
    val result = params.zipWithIndex.foldLeft(initial) { case (acc, (p, idx)) =>
      val paramName = extractIdentName(p.value.name)
      if (acc.seen.contains(paramName)) {
        val linkedIdent = posIdent(LinkedInfo.Error(SemanticError.DuplicateDefinition(paramName)), p.value.name.begin, p.value.name.end)
        val linkedParam = Pos(LinkedPhase.LambdaParameter(linkedIdent, p.value.typ), p.begin, p.end)
        acc.copy(linkedParams = acc.linkedParams :+ linkedParam)
      } else {
        val binding = ScopeBinding(paramName, idx, LocalBindingKind.Parameter)
        val linkedIdent = posIdent(LinkedInfo.Local(0, idx, paramName, LocalBindingKind.Parameter), p.value.name.begin, p.value.name.end)
        val linkedParam = Pos(LinkedPhase.LambdaParameter(linkedIdent, p.value.typ), p.begin, p.end)
        Acc(acc.seen + paramName, acc.bindings :+ binding, acc.linkedParams :+ linkedParam)
      }
    }

    val lambdaEnv = env.pushLayer(result.bindings)
    val linkedBody = linkPosExpression(body, lambdaEnv, Set.empty)
    LinkedPhase.Lambda(result.linkedParams, linkedBody)
  }

  private def linkCases(cases: Vector[Pos[TypeAscriptionPhase.Case[Pos]]],
                        env: LexicalScope): Vector[Pos[LinkedPhase.Case[Pos]]] = {
    cases.foldLeft(Vector.empty[Pos[LinkedPhase.Case[Pos]]]) { (acc, c) =>
      acc :+ linkCase(c, env)
    }
  }

  private def linkCase(c: Pos[TypeAscriptionPhase.Case[Pos]],
                       env: LexicalScope): Pos[LinkedPhase.Case[Pos]] = {
    val varNames = extractPatternVarNames(c.value.pattern)

    val (bindings, bindingMap) = varNames.foldLeft((Vector.empty[ScopeBinding], Map.empty[String, ScopeBinding])) {
      case ((bList, bMap), name) =>
        if (bMap.contains(name)) {
          (bList, bMap)
        } else {
          val slot = bList.length
          val binding = ScopeBinding(name, slot, LocalBindingKind.PatternBinding)
          (bList :+ binding, bMap + (name -> binding))
        }
    }

    val caseEnv = env.pushLayer(bindings)
    val linkedPattern = linkPattern(c.value.pattern, 0, bindingMap)
    val linkedGuard = c.value.guard.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { g =>
      Some(linkPosExpression(g, caseEnv, Set.empty))
    }
    val linkedBody = linkPosExpression(c.value.body, caseEnv, Set.empty)
    Pos(LinkedPhase.Case(linkedPattern, linkedGuard, linkedBody), c.begin, c.end)
  }

  private def linkBlock(declarations: Vector[Pos[TypeAscriptionPhase.Declaration[Pos]]],
                        result: Pos[TypeAscriptionPhase.Expression[Pos]],
                        env: LexicalScope): LinkedPhase.Block[Pos] = {
    val allFutureNames: Set[String] = declarations.foldLeft(Set.empty[String]) { (acc, d) =>
      acc ++ declaredNamesIn(d)
    }

    case class BlockAcc(currentBindings: Vector[ScopeBinding],
                        futureNames: Set[String],
                        linkedDecls: Vector[Pos[LinkedPhase.Declaration[Pos]]])

    val initialAcc = BlockAcc(Vector.empty, allFutureNames, Vector.empty)
    val blockResult = declarations.foldLeft(initialAcc) { (acc, decl) =>
      val declNames = declaredNamesIn(decl)
      val remainingFuture = acc.futureNames -- declNames
      val currentEnv = env.pushLayer(acc.currentBindings)

      decl.value match {
        case TypeAscriptionPhase.Declaration.Val(pattern, rhs) =>
          val linkedRhs = linkPosExpression(rhs, currentEnv, remainingFuture)
          val varNames = extractPatternVarNames(pattern)

          val (newBindings, bindingMap) = varNames.foldLeft((Vector.empty[ScopeBinding], Map.empty[String, ScopeBinding])) {
            case ((bList, bMap), name) =>
              val isDuplicate = acc.currentBindings.exists(_.name == name) || bMap.contains(name)
              if (isDuplicate) {
                (bList, bMap)
              } else {
                val slot = acc.currentBindings.length + bList.length
                val binding = ScopeBinding(name, slot, LocalBindingKind.Val)
                (bList :+ binding, bMap + (name -> binding))
              }
          }

          val linkedPattern = linkPattern(pattern, 0, bindingMap)
          val newDecl: LinkedPhase.Declaration[Pos] = LinkedPhase.Declaration.Val(linkedPattern, linkedRhs)
          val linkedDecl = Pos(newDecl, decl.begin, decl.end)
          BlockAcc(acc.currentBindings ++ newBindings, remainingFuture, acc.linkedDecls :+ linkedDecl)

        case TypeAscriptionPhase.Declaration.LazyVal(pattern, rhs) =>
          val linkedRhs = linkPosExpression(rhs, currentEnv, remainingFuture)
          val varNames = extractPatternVarNames(pattern)

          val (newBindings, bindingMap) = varNames.foldLeft((Vector.empty[ScopeBinding], Map.empty[String, ScopeBinding])) {
            case ((bList, bMap), name) =>
              val isDuplicate = acc.currentBindings.exists(_.name == name) || bMap.contains(name)
              if (isDuplicate) {
                (bList, bMap)
              } else {
                val slot = acc.currentBindings.length + bList.length
                val binding = ScopeBinding(name, slot, LocalBindingKind.LazyVal)
                (bList :+ binding, bMap + (name -> binding))
              }
          }

          val linkedPattern = linkPattern(pattern, 0, bindingMap)
          val newDecl: LinkedPhase.Declaration[Pos] = LinkedPhase.Declaration.LazyVal(linkedPattern, linkedRhs)
          val linkedDecl = Pos(newDecl, decl.begin, decl.end)
          BlockAcc(acc.currentBindings ++ newBindings, remainingFuture, acc.linkedDecls :+ linkedDecl)

        case TypeAscriptionPhase.Declaration.Def(name, paramGroups, returnType, body) =>
          val defName = extractIdentName(name)
          val isDuplicate = acc.currentBindings.exists(_.name == defName)
          val defSlot = acc.currentBindings.length
          val defBinding = ScopeBinding(defName, defSlot, LocalBindingKind.Def)
          val linkedName = if (isDuplicate) {
            posIdent(LinkedInfo.Error(SemanticError.DuplicateDefinition(defName)), name.begin, name.end)
          } else {
            posIdent(LinkedInfo.Local(0, defSlot, defName, LocalBindingKind.Def), name.begin, name.end)
          }

          val (paramBindings, linkedParamGroups) = linkFormalParamGroups(paramGroups)
          val defEnv = currentEnv.pushLayer(paramBindings)
          val linkedBody = linkPosExpression(body, defEnv, Set.empty)

          val newDecl: LinkedPhase.Declaration[Pos] = LinkedPhase.Declaration.Def(linkedName, linkedParamGroups, returnType, linkedBody)
          val linkedDecl = Pos(newDecl, decl.begin, decl.end)
          val nextBindings = if (isDuplicate) acc.currentBindings else acc.currentBindings :+ defBinding
          BlockAcc(nextBindings, remainingFuture, acc.linkedDecls :+ linkedDecl)

        case TypeAscriptionPhase.Declaration.Error(err) =>
          val newDecl: LinkedPhase.Declaration[Pos] = LinkedPhase.Declaration.Error(err)
          val linkedDecl = Pos(newDecl, decl.begin, decl.end)
          BlockAcc(acc.currentBindings, remainingFuture, acc.linkedDecls :+ linkedDecl)
      }
    }

    val finalBlockEnv = env.pushLayer(blockResult.currentBindings)
    val linkedResult = linkPosExpression(result, finalBlockEnv, Set.empty)
    LinkedPhase.Block(blockResult.linkedDecls, linkedResult)
  }

  private def linkPattern(pattern: Pos[TypeAscriptionPhase.Pattern[Pos]],
                          depth: Int,
                          scopeBindings: Map[String, ScopeBinding]): Pos[LinkedPhase.Pattern[Pos]] = {
    val linkedVal: LinkedPhase.Pattern[Pos] = pattern.value match {
      case TypeAscriptionPhase.Pattern.Identifier(name) =>
        val n = extractIdentName(name)
        val info = scopeBindings.get(n) match {
          case Some(sb) => LinkedInfo.Local(depth, sb.slot, n, sb.kind)
          case None => LinkedInfo.Error(SemanticError.DuplicateDefinition(n))
        }
        LinkedPhase.Pattern.Identifier(posIdent(info, name.begin, name.end))

      case TypeAscriptionPhase.Pattern.Wildcard() =>
        LinkedPhase.Pattern.Wildcard()

      case TypeAscriptionPhase.Pattern.Literal(value) =>
        LinkedPhase.Pattern.Literal(Pos(linkLiteral(value.value), value.begin, value.end))

      case TypeAscriptionPhase.Pattern.As(name, inner) =>
        val n = extractIdentName(name)
        val info = scopeBindings.get(n) match {
          case Some(sb) => LinkedInfo.Local(depth, sb.slot, n, sb.kind)
          case None => LinkedInfo.Error(SemanticError.DuplicateDefinition(n))
        }
        val linkedIdent = posIdent(info, name.begin, name.end)
        val linkedInner = linkPattern(inner, depth, scopeBindings)
        LinkedPhase.Pattern.As(linkedIdent, linkedInner)

      case TypeAscriptionPhase.Pattern.Typed(inner, ascription) =>
        LinkedPhase.Pattern.Typed(linkPattern(inner, depth, scopeBindings), ascription)

      case TypeAscriptionPhase.Pattern.Tuple(elements) =>
        LinkedPhase.Pattern.Tuple(
          elements.foldLeft(Vector.empty[Pos[LinkedPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ linkPattern(el, depth, scopeBindings)
          }
        )

      case TypeAscriptionPhase.Pattern.Product(typeId, args) =>
        LinkedPhase.Pattern.Product(
          typeId,
          args.foldLeft(Vector.empty[Pos[LinkedPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ linkPattern(el, depth, scopeBindings)
          }
        )

      case TypeAscriptionPhase.Pattern.Error(err) =>
        LinkedPhase.Pattern.Error(err)
    }
    Pos(linkedVal, pattern.begin, pattern.end)
  }

  private def extractPatternVarNames(pattern: Pos[TypeAscriptionPhase.Pattern[Pos]]): Vector[String] = {
    pattern.value match {
      case TypeAscriptionPhase.Pattern.Identifier(name) =>
        Vector(extractIdentName(name))
      case TypeAscriptionPhase.Pattern.Wildcard() =>
        Vector.empty
      case TypeAscriptionPhase.Pattern.Literal(_) =>
        Vector.empty
      case TypeAscriptionPhase.Pattern.As(name, inner) =>
        extractIdentName(name) +: extractPatternVarNames(inner)
      case TypeAscriptionPhase.Pattern.Typed(inner, _) =>
        extractPatternVarNames(inner)
      case TypeAscriptionPhase.Pattern.Tuple(elements) =>
        elements.foldLeft(Vector.empty[String]) { (acc, el) =>
          acc ++ extractPatternVarNames(el)
        }
      case TypeAscriptionPhase.Pattern.Product(_, args) =>
        args.foldLeft(Vector.empty[String]) { (acc, arg) =>
          acc ++ extractPatternVarNames(arg)
        }
      case TypeAscriptionPhase.Pattern.Error(_) =>
        Vector.empty
    }
  }

  private def declaredNamesIn(decl: Pos[TypeAscriptionPhase.Declaration[Pos]]): Set[String] = {
    decl.value match {
      case TypeAscriptionPhase.Declaration.Val(pattern, _) =>
        extractPatternVarNames(pattern).toSet
      case TypeAscriptionPhase.Declaration.LazyVal(pattern, _) =>
        extractPatternVarNames(pattern).toSet
      case TypeAscriptionPhase.Declaration.Def(name, _, _, _) =>
        Set(extractIdentName(name))
      case TypeAscriptionPhase.Declaration.Error(_) =>
        Set.empty
    }
  }

  private def linkFormalParamGroups(paramGroups: Vector[Pos[TypeAscriptionPhase.FormalParameterGroup[Pos]]]): (Vector[ScopeBinding], Vector[Pos[LinkedPhase.FormalParameterGroup[Pos]]]) = {
    case class ParamAcc(bindings: Vector[ScopeBinding],
                        seen: Set[String],
                        linkedGroups: Vector[Pos[LinkedPhase.FormalParameterGroup[Pos]]])

    val initial = ParamAcc(Vector.empty, Set.empty, Vector.empty)
    val res = paramGroups.foldLeft(initial) { (acc, group) =>
      case class GroupAcc(bindings: Vector[ScopeBinding],
                          seen: Set[String],
                          linkedParams: Vector[Pos[LinkedPhase.FormalParameter[Pos]]])

      val gInitial = GroupAcc(acc.bindings, acc.seen, Vector.empty)
      val gRes = group.value.parameters.foldLeft(gInitial) { (pAcc, p) =>
        val paramName = extractIdentName(p.value.name)
        val slot = pAcc.bindings.length
        if (pAcc.seen.contains(paramName)) {
          val linkedIdent = posIdent(LinkedInfo.Error(SemanticError.DuplicateDefinition(paramName)), p.value.name.begin, p.value.name.end)
          val linkedDefault = p.value.default.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { d =>
            Some(linkPosExpression(d, LexicalScope.empty, Set.empty))
          }
          val linkedP = Pos(LinkedPhase.FormalParameter(linkedIdent, p.value.typ, linkedDefault), p.begin, p.end)
          pAcc.copy(linkedParams = pAcc.linkedParams :+ linkedP)
        } else {
          val binding = ScopeBinding(paramName, slot, LocalBindingKind.Parameter)
          val linkedIdent = posIdent(LinkedInfo.Local(0, slot, paramName, LocalBindingKind.Parameter), p.value.name.begin, p.value.name.end)
          val linkedDefault = p.value.default.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { d =>
            Some(linkPosExpression(d, LexicalScope.empty, Set.empty))
          }
          val linkedP = Pos(LinkedPhase.FormalParameter(linkedIdent, p.value.typ, linkedDefault), p.begin, p.end)
          GroupAcc(pAcc.bindings :+ binding, pAcc.seen + paramName, pAcc.linkedParams :+ linkedP)
        }
      }

      val linkedVariadic = group.value.variadic.fold[Option[Pos[LinkedPhase.FormalParameter[Pos]]]](None) { v =>
        val vName = extractIdentName(v.value.name)
        val vSlot = gRes.bindings.length
        val vIdent = posIdent(LinkedInfo.Local(0, vSlot, vName, LocalBindingKind.Parameter), v.value.name.begin, v.value.name.end)
        val vDefault = v.value.default.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { d =>
          Some(linkPosExpression(d, LexicalScope.empty, Set.empty))
        }
        Some(Pos(LinkedPhase.FormalParameter(vIdent, v.value.typ, vDefault), v.begin, v.end))
      }

      val variadicBinding = group.value.variadic.fold(Vector.empty[ScopeBinding]) { v =>
        val vName = extractIdentName(v.value.name)
        val vSlot = gRes.bindings.length
        Vector(ScopeBinding(vName, vSlot, LocalBindingKind.Parameter))
      }

      val linkedGroup = Pos(LinkedPhase.FormalParameterGroup(gRes.linkedParams, linkedVariadic), group.begin, group.end)
      ParamAcc(gRes.bindings ++ variadicBinding, gRes.seen, acc.linkedGroups :+ linkedGroup)
    }
    (res.bindings, res.linkedGroups)
  }

  private def posIdent(info: LinkedInfo, begin: CharIndex, end: CharIndex): Pos[LinkedPhase.Ident[Pos]] =
    Pos(Pos(info, begin, end), begin, end)

  private def extractIdentName(id: Pos[TypeAscriptionPhase.Ident[Pos]]): String = {
    id.value.value match {
      case PreResolutionInfo.Unresolved(name) => name
      case PreResolutionInfo.Bound(symbol) => symbol.toString
      case PreResolutionInfo.MethodSite(name, _) => name
      case PreResolutionInfo.StaticFunctionSite(qname, _) => qname
      case PreResolutionInfo.FieldSite(name) => name
      case PreResolutionInfo.AmbiguousName(name, _) => name
    }
  }
}
