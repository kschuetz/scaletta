package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api._
import software.kes.scaletta.internal.Universe
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.semantics.Linker.{Input, Output}
import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}
import software.kes.scaletta.internal.types.{ConjunctionType, TypeNameIndex, TypeResolutionError}

object Linker {
  type Input = DesugaredPhase.Expression[Pos]
  type Output = LinkedPhase.Expression[Pos]

  def create(methodResolver: MethodResolver,
             typeNameIndex: TypeNameIndex,
             importScope: ImportScope = ImportScope.default): Linker =
    new Linker(methodResolver, typeNameIndex, importScope)

  def create(universe: Universe,
             importScope: ImportScope): Linker =
    new Linker(universe, universe.typeUniverse.nameIndex, importScope)

  def create(universe: Universe): Linker =
    new Linker(universe, universe.typeUniverse.nameIndex, ImportScope.default)
}

/**
 * Traverses the AST in a single linear pass to resolve type annotations, lexical scoping,
 * and coordinate variable slot assignments. Enforces strict forward-reference checks within blocks,
 * binds local variables to (depth, slot) coordinates, detects semantic scoping and type resolution errors,
 * and enriches call sites with candidate overload collections.
 */
final class Linker private(methodResolver: MethodResolver,
                           typeNameIndex: TypeNameIndex,
                           importScope: ImportScope) {

  def link(input: Input): Output = linkExpression(input, LexicalScope.empty, Set.empty, Set.empty)

  private def linkPosExpression(posExpr: Pos[DesugaredPhase.Expression[Pos]],
                                env: LexicalScope,
                                futureInBlock: Set[String],
                                disallowedSelfRefs: Set[String]): Pos[LinkedPhase.Expression[Pos]] =
    Pos(linkExpression(posExpr.value, env, futureInBlock, disallowedSelfRefs), posExpr.begin, posExpr.end)

  private def linkExpression(expr: DesugaredPhase.Expression[Pos],
                             env: LexicalScope,
                             futureInBlock: Set[String],
                             disallowedSelfRefs: Set[String]): LinkedPhase.Expression[Pos] = {
    expr match {
      case DesugaredPhase.Expression.Error(err) =>
        LinkedPhase.Expression.Error(err)

      case lit: DesugaredPhase.Literal[Pos] @unchecked =>
        linkLiteral(lit)

      case DesugaredPhase.Reference(id) =>
        LinkedPhase.Reference(linkIdentifier(id, env, futureInBlock, disallowedSelfRefs))

      case DesugaredPhase.Select(qualifier, name) =>
        LinkedPhase.Select(linkPosExpression(qualifier, env, futureInBlock, disallowedSelfRefs), linkFieldSite(name))

      case DesugaredPhase.Typed(expression, ascription) =>
        LinkedPhase.Typed(linkPosExpression(expression, env, futureInBlock, disallowedSelfRefs), ascribeTypeIdent(ascription))

      case DesugaredPhase.Tuple(elements) =>
        LinkedPhase.Tuple(
          elements.foldLeft(Vector.empty[Pos[LinkedPhase.Expression[Pos]]]) { (acc, el) =>
            acc :+ linkPosExpression(el, env, futureInBlock, disallowedSelfRefs)
          }
        )

      case DesugaredPhase.Conditional(condition, thenBranch, elseBranch) =>
        LinkedPhase.Conditional(
          linkPosExpression(condition, env, futureInBlock, disallowedSelfRefs),
          linkPosExpression(thenBranch, env, futureInBlock, disallowedSelfRefs),
          linkPosExpression(elseBranch, env, futureInBlock, disallowedSelfRefs)
        )

      case DesugaredPhase.Call.Infix(left, operation, typeArgs, right) =>
        val receiverTypeOpt = extractAscribedType(left.value)
        val rightParam = extractAscribedType(right.value) match {
          case Some(t) => SignatureQueryParameter.OfType(t)
          case None => SignatureQueryParameter.Unknown
        }
        val query = SignatureQuery.of(rightParam)
        val opName = operation.value.name
        val linkedOpInfo = if (disallowedSelfRefs.contains(opName)) {
          LinkedInfo.Error(SemanticError.RecursiveReference(opName))
        } else {
          val candidates = receiverTypeOpt match {
            case Some(recType) =>
              methodResolver.getMethodCandidates(recType, Name(opName), query)
            case None =>
              methodResolver.getMethodCandidates(Name(opName), query)
          }
          if (candidates.nonEmpty) {
            LinkedInfo.MethodCandidates(opName, Vector(1), candidates.toVector)
          } else {
            val ascribedArgs = extractAscribedType(right.value).toVector
            val err = receiverTypeOpt match {
              case Some(recType) => SemanticError.NoSuchMethod(recType, opName, ascribedArgs)
              case None => SemanticError.NoSuchFunction(opName, ascribedArgs)
            }
            LinkedInfo.Error(err)
          }
        }
        val linkedOp = posIdent(linkedOpInfo, operation.begin, operation.end)
        LinkedPhase.Call.Infix(
          linkPosExpression(left, env, futureInBlock, disallowedSelfRefs),
          linkedOp,
          linkTypeArgs(typeArgs),
          linkPosExpression(right, env, futureInBlock, disallowedSelfRefs)
        )

      case DesugaredPhase.Call.Standard(target, typeArgs, args) =>
        val (argArities, query, ascribedArgs) = buildSignatureQueryForArgs(args)
        val linkedTypeArgs = linkTypeArgs(typeArgs)
        val linkedArgs = linkArgumentGroups(args, env, futureInBlock, disallowedSelfRefs)
        val linkedTarget = linkCallTarget(target, argArities, query, ascribedArgs, env, futureInBlock, disallowedSelfRefs)
        LinkedPhase.Call.Standard(
          linkedTarget,
          linkedTypeArgs,
          linkedArgs
        )

      case DesugaredPhase.Lambda(params, body) =>
        linkLambda(params, body, env, futureInBlock, disallowedSelfRefs)

      case DesugaredPhase.InterpolatedString(interpolator, initial, segments) =>
        val newSegments = segments.foldLeft(Vector.empty[(Pos[LinkedPhase.Expression[Pos]], String)]) {
          case (acc, (exprPart, textPart)) =>
            acc :+ (linkPosExpression(exprPart, env, futureInBlock, disallowedSelfRefs), textPart)
        }
        LinkedPhase.InterpolatedString(interpolator, initial, newSegments)

      case DesugaredPhase.Match(expression, cases) =>
        LinkedPhase.Match(linkPosExpression(expression, env, futureInBlock, disallowedSelfRefs), linkCases(cases, env, futureInBlock, disallowedSelfRefs))

      case DesugaredPhase.Block(declarations, result) =>
        linkBlock(declarations, result, env, futureInBlock, disallowedSelfRefs)
    }
  }

  private def linkCallTarget(target: Pos[DesugaredPhase.Expression[Pos]],
                             argArities: Vector[Int],
                             query: SignatureQuery,
                             ascribedArgs: Vector[ProperType[TypeId]],
                             env: LexicalScope,
                             futureInBlock: Set[String],
                             disallowedSelfRefs: Set[String]): Pos[LinkedPhase.Expression[Pos]] = {
    target.value match {
      case DesugaredPhase.Reference(id) =>
        val name = id.value.name
        val linkedInfo = if (disallowedSelfRefs.contains(name)) {
          LinkedInfo.Error(SemanticError.RecursiveReference(name))
        } else if (futureInBlock.contains(name)) {
          LinkedInfo.Error(SemanticError.ForwardReference(name))
        } else {
          env.lookup(name) match {
            case Some((depth, binding)) =>
              LinkedInfo.Local(depth, binding.slot, name, binding.kind)
            case None =>
              val qName = QualifiedName.tryParsePartial(name).fold(_ => QualifiedName.local(Name(name)), identity)
              val candidates = methodResolver.getStaticFunctionCandidates(qName, importScope, query)
              if (candidates.nonEmpty) {
                LinkedInfo.StaticFunctionCandidates(name, argArities, candidates.toVector)
              } else {
                LinkedInfo.Error(SemanticError.NoSuchFunction(name, ascribedArgs))
              }
          }
        }
        Pos(LinkedPhase.Reference(posIdent(linkedInfo, id.begin, id.end)), target.begin, target.end)

      case DesugaredPhase.Select(qualifier, nameIdent) =>
        val nameStr = nameIdent.value.name
        val qualifiedPathOpt = extractQualifiedName(qualifier.value, env)
        val linkedInfo = qualifiedPathOpt match {
          case Some(segments) =>
            val fullPathStr = (segments :+ nameStr).mkString(".")
            val tryParsed = QualifiedName.tryParsePartial(fullPathStr).fold(
              _ => QualifiedName.tryParseFull(fullPathStr).toOption,
              p => Some(p)
            )
            tryParsed match {
              case Some(qName) =>
                val staticCandidates = methodResolver.getStaticFunctionCandidates(qName, importScope, query)
                if (staticCandidates.nonEmpty) {
                  LinkedInfo.StaticFunctionCandidates(nameStr, argArities, staticCandidates.toVector)
                } else {
                  resolveMethodOnSelect(qualifier.value, nameStr, argArities, query, ascribedArgs)
                }
              case None =>
                resolveMethodOnSelect(qualifier.value, nameStr, argArities, query, ascribedArgs)
            }
          case None =>
            resolveMethodOnSelect(qualifier.value, nameStr, argArities, query, ascribedArgs)
        }
        Pos(
          LinkedPhase.Select(
            linkPosExpression(qualifier, env, futureInBlock, disallowedSelfRefs),
            posIdent(linkedInfo, nameIdent.begin, nameIdent.end)
          ),
          target.begin,
          target.end
        )

      case _ =>
        linkPosExpression(target, env, futureInBlock, disallowedSelfRefs)
    }
  }

  private def resolveMethodOnSelect(qualifier: DesugaredPhase.Expression[Pos],
                                    nameStr: String,
                                    argArities: Vector[Int],
                                    query: SignatureQuery,
                                    ascribedArgs: Vector[ProperType[TypeId]]): LinkedInfo = {
    val receiverTypeOpt = extractAscribedType(qualifier)
    val candidates = receiverTypeOpt match {
      case Some(recType) =>
        methodResolver.getMethodCandidates(recType, Name(nameStr), query)
      case None =>
        methodResolver.getMethodCandidates(Name(nameStr), query)
    }
    if (candidates.nonEmpty) {
      LinkedInfo.MethodCandidates(nameStr, argArities, candidates.toVector)
    } else {
      val err = receiverTypeOpt match {
        case Some(recType) => SemanticError.NoSuchMethod(recType, nameStr, ascribedArgs)
        case None => SemanticError.NoSuchFunction(nameStr, ascribedArgs)
      }
      LinkedInfo.Error(err)
    }
  }

  private def extractQualifiedName(expr: DesugaredPhase.Expression[Pos],
                                   env: LexicalScope): Option[Vector[String]] = {
    expr match {
      case DesugaredPhase.Reference(id) if env.lookup(id.value.name).isEmpty =>
        Some(Vector(id.value.name))
      case DesugaredPhase.Select(q, n) =>
        extractQualifiedName(q.value, env) match {
          case Some(segments) => Some(segments :+ n.value.name)
          case None => None
        }
      case _ =>
        None
    }
  }

  private def buildSignatureQueryForArgs(groups: Vector[Pos[DesugaredPhase.ArgumentGroup[Pos]]]): (Vector[Int], SignatureQuery, Vector[ProperType[TypeId]]) = {
    case class Acc(arities: Vector[Int],
                   queryGroups: Vector[SignatureQuery.Group],
                   ascribedTypes: Vector[ProperType[TypeId]])

    val initial = Acc(Vector.empty, Vector.empty, Vector.empty)
    val res = groups.foldLeft(initial) { (acc, group) =>
      case class GroupAcc(params: Vector[SignatureQueryParameter],
                          ascribed: Vector[ProperType[TypeId]])
      val gInitial = GroupAcc(Vector.empty, Vector.empty)
      val gRes = group.value.arguments.foldLeft(gInitial) { (gAcc, arg) =>
        extractAscribedType(arg.value.value.value) match {
          case Some(t) =>
            GroupAcc(gAcc.params :+ SignatureQueryParameter.OfType(t), gAcc.ascribed :+ t)
          case None =>
            GroupAcc(gAcc.params :+ SignatureQueryParameter.Unknown, gAcc.ascribed)
        }
      }
      Acc(
        acc.arities :+ group.value.arguments.size,
        acc.queryGroups :+ SignatureQuery.Group(gRes.params),
        acc.ascribedTypes ++ gRes.ascribed
      )
    }
    (res.arities, SignatureQuery.ofGroups(res.queryGroups: _*), res.ascribedTypes)
  }

  private def extractAscribedType(expr: DesugaredPhase.Expression[Pos]): Option[ProperType[TypeId]] =
    expr match {
      case DesugaredPhase.Typed(_, ascription) =>
        resolveTypeIdentifier(ascription.value) match {
          case Right(t) => Some(t)
          case Left(_) => None
        }
      case _ => None
    }

  private def linkIdentifier(id: Pos[Identifier],
                             env: LexicalScope,
                             futureInBlock: Set[String],
                             disallowedSelfRefs: Set[String]): Pos[LinkedPhase.Ident] = {
    val name = id.value.name
    val info = if (disallowedSelfRefs.contains(name)) {
      LinkedInfo.Error(SemanticError.RecursiveReference(name))
    } else if (futureInBlock.contains(name)) {
      LinkedInfo.Error(SemanticError.ForwardReference(name))
    } else {
      env.lookup(name) match {
        case Some((depth, binding)) =>
          LinkedInfo.Local(depth, binding.slot, name, binding.kind)
        case None =>
          val qName = QualifiedName.tryParsePartial(name).fold(_ => QualifiedName.local(Name(name)), identity)
          val candidates = methodResolver.getStaticFunctionCandidates(qName, importScope, SignatureQuery.empty)
          if (candidates.nonEmpty) {
            LinkedInfo.StaticFunctionCandidates(name, Vector.empty, candidates.toVector)
          } else {
            LinkedInfo.Error(SemanticError.UnknownIdentifier(name))
          }
      }
    }
    posIdent(info, id.begin, id.end)
  }

  private def linkFieldSite(name: Pos[Identifier]): Pos[LinkedPhase.Ident] = {
    val info = LinkedInfo.FieldSite(name.value.name)
    posIdent(info, name.begin, name.end)
  }

  private def linkLiteral(lit: DesugaredPhase.Literal[Pos]): LinkedPhase.Literal[Pos] = {
    lit match {
      case DesugaredPhase.Literal.IntLiteral(v) => LinkedPhase.Literal.IntLiteral(v)
      case DesugaredPhase.Literal.LongLiteral(v) => LinkedPhase.Literal.LongLiteral(v)
      case DesugaredPhase.Literal.FloatLiteral(v) => LinkedPhase.Literal.FloatLiteral(v)
      case DesugaredPhase.Literal.DoubleLiteral(v) => LinkedPhase.Literal.DoubleLiteral(v)
      case DesugaredPhase.Literal.True() => LinkedPhase.Literal.True()
      case DesugaredPhase.Literal.False() => LinkedPhase.Literal.False()
      case DesugaredPhase.Literal.Null() => LinkedPhase.Literal.Null()
      case DesugaredPhase.Literal.CharLiteral(v) => LinkedPhase.Literal.CharLiteral(v)
      case DesugaredPhase.Literal.StringLiteral(v) => LinkedPhase.Literal.StringLiteral(v)
      case DesugaredPhase.Literal.UnitLiteral() => LinkedPhase.Literal.UnitLiteral()
    }
  }

  private def linkTypeArgs(typeArgs: Vector[Pos[DesugaredPhase.TypeArgument[Pos]]]): Vector[Pos[LinkedPhase.TypeArgument[Pos]]] =
    typeArgs.foldLeft(Vector.empty[Pos[LinkedPhase.TypeArgument[Pos]]]) { (acc, ta) =>
      acc :+ Pos(LinkedPhase.TypeArgument[Pos](ascribeTypeIdent(ta.value.typ)), ta.begin, ta.end)
    }

  private def linkArgumentGroups(groups: Vector[Pos[DesugaredPhase.ArgumentGroup[Pos]]],
                                 env: LexicalScope,
                                 futureInBlock: Set[String],
                                 disallowedSelfRefs: Set[String]): Vector[Pos[LinkedPhase.ArgumentGroup[Pos]]] = {
    groups.foldLeft(Vector.empty[Pos[LinkedPhase.ArgumentGroup[Pos]]]) { (acc, ag) =>
      val newArgs = ag.value.arguments.foldLeft(Vector.empty[Pos[LinkedPhase.Argument[Pos]]]) { (aAcc, arg) =>
        val linkedValue = linkPosExpression(arg.value.value, env, futureInBlock, disallowedSelfRefs)
        val linkedName = arg.value.name.fold[Option[Pos[LinkedPhase.Ident]]](None) { n =>
          Some(linkIdentifier(n, env, futureInBlock, disallowedSelfRefs))
        }
        aAcc :+ Pos(LinkedPhase.Argument(linkedValue, linkedName), arg.begin, arg.end)
      }
      val newSplat = ag.value.splat.fold[Option[Pos[LinkedPhase.Argument[Pos]]]](None) { s =>
        val linkedValue = linkPosExpression(s.value.value, env, futureInBlock, disallowedSelfRefs)
        val linkedName = s.value.name.fold[Option[Pos[LinkedPhase.Ident]]](None) { n =>
          Some(linkIdentifier(n, env, futureInBlock, disallowedSelfRefs))
        }
        Some(Pos(LinkedPhase.Argument(linkedValue, linkedName), s.begin, s.end))
      }
      acc :+ Pos(LinkedPhase.ArgumentGroup(newArgs, newSplat), ag.begin, ag.end)
    }
  }

  private def linkLambda(params: Vector[Pos[DesugaredPhase.LambdaParameter[Pos]]],
                         body: Pos[DesugaredPhase.Expression[Pos]],
                         env: LexicalScope,
                         futureInBlock: Set[String],
                         disallowedSelfRefs: Set[String]): LinkedPhase.Lambda[Pos] = {
    case class Acc(seen: Set[String],
                   bindings: Vector[ScopeBinding],
                   linkedParams: Vector[Pos[LinkedPhase.LambdaParameter[Pos]]])

    val initial = Acc(Set.empty, Vector.empty, Vector.empty)
    val result = params.zipWithIndex.foldLeft(initial) { case (acc, (p, idx)) =>
      val paramName = p.value.name.value.name
      val linkedTyp = p.value.typ.fold[Option[Pos[LinkedPhase.TypeIdent[Pos]]]](None) { t =>
        Some(ascribeTypeIdent(t))
      }
      if (acc.seen.contains(paramName)) {
        val linkedIdent = posIdent(LinkedInfo.Error(SemanticError.DuplicateDefinition(paramName)), p.value.name.begin, p.value.name.end)
        val linkedParam = Pos(LinkedPhase.LambdaParameter(linkedIdent, linkedTyp), p.begin, p.end)
        acc.copy(linkedParams = acc.linkedParams :+ linkedParam)
      } else {
        val binding = ScopeBinding(paramName, idx, LocalBindingKind.Parameter)
        val linkedIdent = posIdent(LinkedInfo.Local(0, idx, paramName, LocalBindingKind.Parameter), p.value.name.begin, p.value.name.end)
        val linkedParam = Pos(LinkedPhase.LambdaParameter(linkedIdent, linkedTyp), p.begin, p.end)
        Acc(acc.seen + paramName, acc.bindings :+ binding, acc.linkedParams :+ linkedParam)
      }
    }

    val lambdaEnv = env.pushLayer(result.bindings)
    val paramNames = result.bindings.map(_.name).toSet
    val lambdaFuture = futureInBlock -- paramNames
    val lambdaDisallowed = disallowedSelfRefs -- paramNames
    val linkedBody = linkPosExpression(body, lambdaEnv, lambdaFuture, lambdaDisallowed)
    LinkedPhase.Lambda(result.linkedParams, linkedBody)
  }

  private def linkCases(cases: Vector[Pos[DesugaredPhase.Case[Pos]]],
                        env: LexicalScope,
                        futureInBlock: Set[String],
                        disallowedSelfRefs: Set[String]): Vector[Pos[LinkedPhase.Case[Pos]]] = {
    cases.foldLeft(Vector.empty[Pos[LinkedPhase.Case[Pos]]]) { (acc, c) =>
      acc :+ linkCase(c, env, futureInBlock, disallowedSelfRefs)
    }
  }

  private def linkCase(c: Pos[DesugaredPhase.Case[Pos]],
                       env: LexicalScope,
                       futureInBlock: Set[String],
                       disallowedSelfRefs: Set[String]): Pos[LinkedPhase.Case[Pos]] = {
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
    val caseNames = bindings.map(_.name).toSet
    val caseFuture = futureInBlock -- caseNames
    val caseDisallowed = disallowedSelfRefs -- caseNames
    val linkedPattern = linkPattern(c.value.pattern, 0, bindingMap)
    val linkedGuard = c.value.guard.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { g =>
      Some(linkPosExpression(g, caseEnv, caseFuture, caseDisallowed))
    }
    val linkedBody = linkPosExpression(c.value.body, caseEnv, caseFuture, caseDisallowed)
    Pos(LinkedPhase.Case(linkedPattern, linkedGuard, linkedBody), c.begin, c.end)
  }

  private def linkBlock(declarations: Vector[Pos[DesugaredPhase.Declaration[Pos]]],
                        result: Pos[DesugaredPhase.Expression[Pos]],
                        env: LexicalScope,
                        futureInBlock: Set[String],
                        disallowedSelfRefs: Set[String]): LinkedPhase.Block[Pos] = {
    val allFutureNames: Set[String] = futureInBlock ++ declarations.foldLeft(Set.empty[String]) { (acc, d) =>
      acc ++ declaredNamesIn(d)
    }

    case class BlockAcc(currentBindings: Vector[ScopeBinding],
                        futureNames: Set[String],
                        disallowed: Set[String],
                        linkedDecls: Vector[Pos[LinkedPhase.Declaration[Pos]]])

    val initialAcc = BlockAcc(Vector.empty, allFutureNames, disallowedSelfRefs, Vector.empty)
    val blockResult = declarations.foldLeft(initialAcc) { (acc, decl) =>
      val declNames = declaredNamesIn(decl)
      val remainingFuture = acc.futureNames -- declNames
      val currentEnv = env.pushLayer(acc.currentBindings)

      decl.value match {
        case DesugaredPhase.Declaration.Val(pattern, rhs) =>
          val linkedRhs = linkPosExpression(rhs, currentEnv, acc.futureNames, acc.disallowed)
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
          val nextDisallowed = acc.disallowed -- newBindings.map(_.name).toSet
          BlockAcc(acc.currentBindings ++ newBindings, remainingFuture, nextDisallowed, acc.linkedDecls :+ linkedDecl)

        case DesugaredPhase.Declaration.LazyVal(pattern, rhs) =>
          val linkedRhs = linkPosExpression(rhs, currentEnv, acc.futureNames, acc.disallowed)
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
          val nextDisallowed = acc.disallowed -- newBindings.map(_.name).toSet
          BlockAcc(acc.currentBindings ++ newBindings, remainingFuture, nextDisallowed, acc.linkedDecls :+ linkedDecl)

        case DesugaredPhase.Declaration.Def(name, paramGroups, returnType, body) =>
          val defName = name.value.name
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
          val defDisallowed = (acc.disallowed + defName) -- paramBindings.map(_.name).toSet
          val linkedBody = linkPosExpression(body, defEnv, remainingFuture, defDisallowed)
          val linkedReturnType = returnType.fold[Option[Pos[LinkedPhase.TypeIdent[Pos]]]](None) { rt =>
            Some(ascribeTypeIdent(rt))
          }

          val newDecl: LinkedPhase.Declaration[Pos] = LinkedPhase.Declaration.Def(linkedName, linkedParamGroups, linkedReturnType, linkedBody)
          val linkedDecl = Pos(newDecl, decl.begin, decl.end)
          val nextBindings = if (isDuplicate) acc.currentBindings else acc.currentBindings :+ defBinding
          val nextDisallowed = if (isDuplicate) acc.disallowed else acc.disallowed - defName
          BlockAcc(nextBindings, remainingFuture, nextDisallowed, acc.linkedDecls :+ linkedDecl)

        case DesugaredPhase.Declaration.Error(err) =>
          val newDecl: LinkedPhase.Declaration[Pos] = LinkedPhase.Declaration.Error(err)
          val linkedDecl = Pos(newDecl, decl.begin, decl.end)
          BlockAcc(acc.currentBindings, remainingFuture, acc.disallowed, acc.linkedDecls :+ linkedDecl)
      }
    }

    val finalBlockEnv = env.pushLayer(blockResult.currentBindings)
    val linkedResult = linkPosExpression(result, finalBlockEnv, blockResult.futureNames, blockResult.disallowed)
    LinkedPhase.Block(blockResult.linkedDecls, linkedResult)
  }

  private def linkPattern(pattern: Pos[DesugaredPhase.Pattern[Pos]],
                          depth: Int,
                          scopeBindings: Map[String, ScopeBinding]): Pos[LinkedPhase.Pattern[Pos]] = {
    val linkedVal: LinkedPhase.Pattern[Pos] = pattern.value match {
      case DesugaredPhase.Pattern.Identifier(name) =>
        val n = name.value.name
        val info = scopeBindings.get(n) match {
          case Some(sb) => LinkedInfo.Local(depth, sb.slot, n, sb.kind)
          case None => LinkedInfo.Error(SemanticError.DuplicateDefinition(n))
        }
        LinkedPhase.Pattern.Identifier(posIdent(info, name.begin, name.end))

      case DesugaredPhase.Pattern.Wildcard() =>
        LinkedPhase.Pattern.Wildcard()

      case DesugaredPhase.Pattern.Literal(value) =>
        LinkedPhase.Pattern.Literal(Pos(linkLiteral(value.value), value.begin, value.end))

      case DesugaredPhase.Pattern.As(name, inner) =>
        val n = name.value.name
        val info = scopeBindings.get(n) match {
          case Some(sb) => LinkedInfo.Local(depth, sb.slot, n, sb.kind)
          case None => LinkedInfo.Error(SemanticError.DuplicateDefinition(n))
        }
        val linkedIdent = posIdent(info, name.begin, name.end)
        val linkedInner = linkPattern(inner, depth, scopeBindings)
        LinkedPhase.Pattern.As(linkedIdent, linkedInner)

      case DesugaredPhase.Pattern.Typed(inner, ascription) =>
        LinkedPhase.Pattern.Typed(linkPattern(inner, depth, scopeBindings), ascribeTypeIdent(ascription))

      case DesugaredPhase.Pattern.Tuple(elements) =>
        LinkedPhase.Pattern.Tuple(
          elements.foldLeft(Vector.empty[Pos[LinkedPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ linkPattern(el, depth, scopeBindings)
          }
        )

      case DesugaredPhase.Pattern.Product(typeId, args) =>
        LinkedPhase.Pattern.Product(
          ascribeTypeIdent(typeId),
          args.foldLeft(Vector.empty[Pos[LinkedPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ linkPattern(el, depth, scopeBindings)
          }
        )

      case DesugaredPhase.Pattern.Error(err) =>
        LinkedPhase.Pattern.Error(err)
    }
    Pos(linkedVal, pattern.begin, pattern.end)
  }

  private def extractPatternVarNames(pattern: Pos[DesugaredPhase.Pattern[Pos]]): Vector[String] = {
    pattern.value match {
      case DesugaredPhase.Pattern.Identifier(name) =>
        Vector(name.value.name)
      case DesugaredPhase.Pattern.Wildcard() =>
        Vector.empty
      case DesugaredPhase.Pattern.Literal(_) =>
        Vector.empty
      case DesugaredPhase.Pattern.As(name, inner) =>
        name.value.name +: extractPatternVarNames(inner)
      case DesugaredPhase.Pattern.Typed(inner, _) =>
        extractPatternVarNames(inner)
      case DesugaredPhase.Pattern.Tuple(elements) =>
        elements.foldLeft(Vector.empty[String]) { (acc, el) =>
          acc ++ extractPatternVarNames(el)
        }
      case DesugaredPhase.Pattern.Product(_, args) =>
        args.foldLeft(Vector.empty[String]) { (acc, arg) =>
          acc ++ extractPatternVarNames(arg)
        }
      case DesugaredPhase.Pattern.Error(_) =>
        Vector.empty
    }
  }

  private def declaredNamesIn(decl: Pos[DesugaredPhase.Declaration[Pos]]): Set[String] = {
    decl.value match {
      case DesugaredPhase.Declaration.Val(pattern, _) =>
        extractPatternVarNames(pattern).toSet
      case DesugaredPhase.Declaration.LazyVal(pattern, _) =>
        extractPatternVarNames(pattern).toSet
      case DesugaredPhase.Declaration.Def(name, _, _, _) =>
        Set(name.value.name)
      case DesugaredPhase.Declaration.Error(_) =>
        Set.empty
    }
  }

  private def linkFormalParamGroups(paramGroups: Vector[Pos[DesugaredPhase.FormalParameterGroup[Pos]]]): (Vector[ScopeBinding], Vector[Pos[LinkedPhase.FormalParameterGroup[Pos]]]) = {
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
        val paramName = p.value.name.value.name
        val slot = pAcc.bindings.length
        val linkedTyp = ascribeTypeIdent(p.value.typ)
        if (pAcc.seen.contains(paramName)) {
          val linkedIdent = posIdent(LinkedInfo.Error(SemanticError.DuplicateDefinition(paramName)), p.value.name.begin, p.value.name.end)
          val linkedDefault = p.value.default.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { d =>
            Some(linkPosExpression(d, LexicalScope.empty, Set.empty, Set.empty))
          }
          val linkedP = Pos(LinkedPhase.FormalParameter(linkedIdent, linkedTyp, linkedDefault), p.begin, p.end)
          pAcc.copy(linkedParams = pAcc.linkedParams :+ linkedP)
        } else {
          val binding = ScopeBinding(paramName, slot, LocalBindingKind.Parameter)
          val linkedIdent = posIdent(LinkedInfo.Local(0, slot, paramName, LocalBindingKind.Parameter), p.value.name.begin, p.value.name.end)
          val linkedDefault = p.value.default.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { d =>
            Some(linkPosExpression(d, LexicalScope.empty, Set.empty, Set.empty))
          }
          val linkedP = Pos(LinkedPhase.FormalParameter(linkedIdent, linkedTyp, linkedDefault), p.begin, p.end)
          GroupAcc(pAcc.bindings :+ binding, pAcc.seen + paramName, pAcc.linkedParams :+ linkedP)
        }
      }

      val linkedVariadic = group.value.variadic.fold[Option[Pos[LinkedPhase.FormalParameter[Pos]]]](None) { v =>
        val vName = v.value.name.value.name
        val vSlot = gRes.bindings.length
        val vIdent = posIdent(LinkedInfo.Local(0, vSlot, vName, LocalBindingKind.Parameter), v.value.name.begin, v.value.name.end)
        val linkedVTyp = ascribeTypeIdent(v.value.typ)
        val vDefault = v.value.default.fold[Option[Pos[LinkedPhase.Expression[Pos]]]](None) { d =>
          Some(linkPosExpression(d, LexicalScope.empty, Set.empty, Set.empty))
        }
        Some(Pos(LinkedPhase.FormalParameter(vIdent, linkedVTyp, vDefault), v.begin, v.end))
      }

      val variadicBinding = group.value.variadic.fold(Vector.empty[ScopeBinding]) { v =>
        val vName = v.value.name.value.name
        val vSlot = gRes.bindings.length
        Vector(ScopeBinding(vName, vSlot, LocalBindingKind.Parameter))
      }

      val linkedGroup = Pos(LinkedPhase.FormalParameterGroup(gRes.linkedParams, linkedVariadic), group.begin, group.end)
      ParamAcc(gRes.bindings ++ variadicBinding, gRes.seen, acc.linkedGroups :+ linkedGroup)
    }
    (res.bindings, res.linkedGroups)
  }

  private def ascribeTypeIdent(ti: Pos[TypeIdentifier[Pos]]): Pos[LinkedPhase.TypeIdent[Pos]] = {
    val resolved = resolveTypeIdentifier(ti.value)
    Pos(resolved, ti.begin, ti.end)
  }

  private def resolveTypeIdentifier(ti: TypeIdentifier[Pos]): Either[TypeResolutionError, ProperType[TypeId]] = {
    ti match {
      case TypeIdentifier.Name(name) =>
        resolveNamedType(Vector(name.value.name))

      case TypeIdentifier.Select(qualifier, name) =>
        extractTypeNameSegments(qualifier.value) match {
          case Some(segments) => resolveNamedType(segments :+ name.value.name)
          case None => Left(TypeResolutionError.UnknownType(name.value.name))
        }

      case TypeIdentifier.Applied(qualifier, args) =>
        resolveAppliedType(qualifier.value, args)

      case TypeIdentifier.Function(params, result) =>
        resolveFunctionType(params, result)

      case TypeIdentifier.Conjunction(conjunctionType, components) =>
        resolveConjunctionType(conjunctionType, components)

      case TypeIdentifier.Tuple(elements) =>
        resolveTupleType(elements)
    }
  }

  private def extractTypeNameSegments(ti: TypeIdentifier[Pos]): Option[Vector[String]] = {
    ti match {
      case TypeIdentifier.Name(name) =>
        Some(Vector(name.value.name))

      case TypeIdentifier.Select(qualifier, name) =>
        extractTypeNameSegments(qualifier.value) match {
          case Some(segments) => Some(segments :+ name.value.name)
          case None => None
        }

      case _ =>
        None
    }
  }

  private def lookupType(segments: Vector[String]): Either[TypeResolutionError, Type[TypeId]] = {
    val fullName = segments.mkString(".")
    val parsedName: Option[QualifiedName] =
      if (segments.size == 1) {
        Some(QualifiedName.tryParsePartial(segments.head).fold(_ => QualifiedName.local(Name(segments.head)), identity))
      } else {
        QualifiedName.tryParsePartial(fullName).fold(_ => QualifiedName.tryParseFull(fullName).toOption, p => Some(p))
      }
    parsedName match {
      case None =>
        Left(TypeResolutionError.UnknownType(fullName))
      case Some(qName) =>
        typeNameIndex.resolve(qName, importScope) match {
          case Nil => Left(TypeResolutionError.UnknownType(fullName))
          case entry :: Nil => Right(entry.value)
          case multiple => Left(TypeResolutionError.AmbiguousType(fullName, multiple.size))
        }
    }
  }

  private def resolveNamedType(segments: Vector[String]): Either[TypeResolutionError, ProperType[TypeId]] = {
    lookupType(segments) match {
      case Left(err) => Left(err)
      case Right(pt: ProperType[TypeId]) => Right(pt)
      case Right(_) => Left(TypeResolutionError.NotAProperType(segments.mkString(".")))
    }
  }

  private def resolveAppliedType(qualifier: TypeIdentifier[Pos],
                                 args: ::[Pos[TypeIdentifier[Pos]]]): Either[TypeResolutionError, ProperType[TypeId]] = {
    extractTypeNameSegments(qualifier) match {
      case None =>
        Left(TypeResolutionError.UnknownType("<applied type>"))

      case Some(segments) =>
        val fullName = segments.mkString(".")
        lookupType(segments) match {
          case Left(err) => Left(err)
          case Right(ctor: Type.Constructor[TypeId]) =>
            sequenceTypeIdents(args.toVector) match {
              case Left(err) => Left(err)
              case Right(argTypes) =>
                if (argTypes.length == ctor.parameters.length) {
                  Right(TypeApplier.fromNode(ctor).applyAllFromSeq(argTypes))
                } else {
                  Left(TypeResolutionError.WrongArity(fullName, ctor.parameters.length, argTypes.length))
                }
            }
          case Right(_) =>
            Left(TypeResolutionError.NotApplicable(fullName))
        }
    }
  }

  private def resolveFunctionType(params: Vector[Pos[TypeIdentifier[Pos]]],
                                  result: Pos[TypeIdentifier[Pos]]): Either[TypeResolutionError, ProperType[TypeId]] = {
    sequenceTypeIdents(params) match {
      case Left(err) => Left(err)
      case Right(paramTypes) =>
        resolveTypeIdentifier(result.value) match {
          case Left(err) => Left(err)
          case Right(resultType) => Right(Type.function(paramTypes: _*)(resultType))
        }
    }
  }

  private def resolveConjunctionType(conjunctionType: ConjunctionType,
                                     components: Vector[Pos[TypeIdentifier[Pos]]]): Either[TypeResolutionError, ProperType[TypeId]] = {
    sequenceTypeIdents(components) match {
      case Left(err) => Left(err)
      case Right(types) =>
        types match {
          case Vector() => Left(TypeResolutionError.UnknownType("<empty conjunction>"))
          case Vector(single) => Right(single)
          case first +: second +: rest =>
            conjunctionType match {
              case ConjunctionType.Union => Right(Type.union(first, second, rest: _*))
              case ConjunctionType.Intersection => Right(Type.intersection(first, second, rest: _*))
            }
          case _ => Left(TypeResolutionError.UnknownType("<invalid conjunction>"))
        }
    }
  }

  private def resolveTupleType(elements: Vector[Pos[TypeIdentifier[Pos]]]): Either[TypeResolutionError, ProperType[TypeId]] = {
    sequenceTypeIdents(elements) match {
      case Left(err) => Left(err)
      case Right(types) =>
        types match {
          case Vector() => Right(Type.unit)
          case Vector(single) => Right(single)
          case first +: second +: rest => Right(Type.tuple(first, second, rest: _*))
          case _ => Left(TypeResolutionError.UnknownType("<invalid tuple>"))
        }
    }
  }

  private def sequenceTypeIdents(items: Vector[Pos[TypeIdentifier[Pos]]]): Either[TypeResolutionError, Vector[ProperType[TypeId]]] =
    items.foldLeft(Right(Vector.empty[ProperType[TypeId]]): Either[TypeResolutionError, Vector[ProperType[TypeId]]]) { (acc, item) =>
      acc match {
        case Left(err) => Left(err)
        case Right(types) =>
          resolveTypeIdentifier(item.value) match {
            case Left(err) => Left(err)
            case Right(t) => Right(types :+ t)
          }
      }
    }

  private def posIdent(info: LinkedInfo, begin: CharIndex, end: CharIndex): Pos[LinkedPhase.Ident] =
    Pos(info, begin, end)
}
