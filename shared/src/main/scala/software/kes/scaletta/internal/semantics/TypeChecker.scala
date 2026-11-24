package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api._
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.builtins.{MethodResolver, OverloadResolver, ResolutionError}
import software.kes.scaletta.internal.interpolators.LinkedInterpolator
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.symbols.SignatureQuery
import software.kes.scaletta.internal.types.{TypeResolutionError, TypeUniverse}
import software.kes.scaletta.util.VectorTwoPlus

object TypeChecker {
  def create(typeUniverse: TypeUniverse, methodResolver: MethodResolver): TypeChecker =
    new TypeChecker(typeUniverse, methodResolver)

  def create(universe: software.kes.scaletta.internal.Universe): TypeChecker =
    new TypeChecker(universe.typeUniverse, universe)
}

final class TypeChecker private(typeUniverse: TypeUniverse, methodResolver: MethodResolver) {
  private val patternChecker: PatternChecker = new PatternChecker(typeUniverse)

  def typeCheck(input: Pos[LinkedPhase.Expression[Pos]],
                initialEnv: TypeEnvironment = TypeEnvironment.empty): TypeCheckingResult = {
    val (attr, state) = checkPosExpression(input, initialEnv, State.empty)
    TypeCheckingResult(attr, state.errors, state.requiredContexts)
  }

  def typeCheck(input: LinkedPhase.Expression[Pos]): TypeCheckingResult =
    typeCheck(Pos(input, CharIndex(0), CharIndex(0)))

  private case class State(errors: Vector[Pos[SemanticError]],
                           requiredContexts: Set[RuntimeContextId]) {
    def addError(err: Pos[SemanticError]): State =
      copy(errors = errors :+ err)

    def addErrors(errs: Vector[Pos[SemanticError]]): State =
      copy(errors = errors ++ errs)

    def addContexts(contexts: Set[RuntimeContextId]): State =
      copy(requiredContexts = requiredContexts ++ contexts)
  }

  private object State {
    val empty: State = State(Vector.empty, Set.empty)
  }

  private def attrIdent(info: TypedInfo,
                        begin: CharIndex,
                        end: CharIndex,
                        tpe: ProperType[TypeId]): Attributed[TypedPhase.Ident] =
    Attributed[TypedPhase.Ident](info, begin, end, tpe)

  private def attrTypeIdent(typ: Either[TypeResolutionError, ProperType[TypeId]],
                            begin: CharIndex,
                            end: CharIndex,
                            tpe: ProperType[TypeId]): Attributed[TypedPhase.TypeIdent[Attributed]] =
    Attributed[TypedPhase.TypeIdent[Attributed]](typ, begin, end, tpe)

  private def checkPosExpression(posExpr: Pos[LinkedPhase.Expression[Pos]],
                                 env: TypeEnvironment,
                                 state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    posExpr.value match {
      case lit: LinkedPhase.Literal[Pos] @unchecked =>
        val (attrLit, nextSt) = checkLiteral(lit, posExpr.begin, posExpr.end, state)
        (Attributed[TypedPhase.Expression[Attributed]](attrLit.value, posExpr.begin, posExpr.end, attrLit.properType), nextSt)

      case LinkedPhase.Expression.Error(err) =>
        (Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Expression.Error[Attributed](err), posExpr.begin, posExpr.end, CoreTypes.NothingT), state)

      case LinkedPhase.Reference(id) =>
        checkReference(id, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Select(qualifier, name) =>
        checkSelect(qualifier, name, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Typed(expression, ascription) =>
        checkTyped(expression, ascription, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Tuple(elements) =>
        checkTuple(elements, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Conditional(condition, thenBranch, elseBranch) =>
        checkConditional(condition, thenBranch, elseBranch, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Lambda(params, body) =>
        checkLambda(params, body, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.InterpolatedString(interpolator, initial, segments) =>
        checkInterpolatedString(interpolator, initial, segments, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Call.Infix(left, operation, typeArgs, right) =>
        checkCallInfix(left, operation, typeArgs, right, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Call.Standard(target, typeArgs, args) =>
        checkCallStandard(target, typeArgs, args, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Block(declarations, result) =>
        checkBlock(declarations, result, posExpr.begin, posExpr.end, env, state)

      case LinkedPhase.Match(expression, cases) =>
        checkMatch(expression, cases, posExpr.begin, posExpr.end, env, state)
    }
  }

  private def checkLiteral(lit: LinkedPhase.Literal[Pos],
                           begin: CharIndex,
                           end: CharIndex,
                           state: State): (Attributed[TypedPhase.Literal[Attributed]], State) = {
    val (typedLit, litType) = lit match {
      case LinkedPhase.Literal.IntLiteral(v) => (TypedPhase.Literal.IntLiteral[Attributed](v), CoreTypes.IntT)
      case LinkedPhase.Literal.LongLiteral(v) => (TypedPhase.Literal.LongLiteral[Attributed](v), CoreTypes.LongT)
      case LinkedPhase.Literal.FloatLiteral(v) => (TypedPhase.Literal.FloatLiteral[Attributed](v), CoreTypes.FloatT)
      case LinkedPhase.Literal.DoubleLiteral(v) => (TypedPhase.Literal.DoubleLiteral[Attributed](v), CoreTypes.DoubleT)
      case LinkedPhase.Literal.True() => (TypedPhase.Literal.True[Attributed](), CoreTypes.BooleanT)
      case LinkedPhase.Literal.False() => (TypedPhase.Literal.False[Attributed](), CoreTypes.BooleanT)
      case LinkedPhase.Literal.Null() => (TypedPhase.Literal.Null[Attributed](), CoreTypes.NullT)
      case LinkedPhase.Literal.CharLiteral(v) => (TypedPhase.Literal.CharLiteral[Attributed](v), CoreTypes.CharT)
      case LinkedPhase.Literal.StringLiteral(v) => (TypedPhase.Literal.StringLiteral[Attributed](v), CoreTypes.StringT)
      case LinkedPhase.Literal.UnitLiteral() => (TypedPhase.Literal.UnitLiteral[Attributed](), CoreTypes.UnitT)
    }
    val attr: Attributed[TypedPhase.Literal[Attributed]] = Attributed[TypedPhase.Literal[Attributed]](typedLit, begin, end, litType)
    (attr, state)
  }

  private def checkReference(id: Pos[LinkedPhase.Ident],
                             begin: CharIndex,
                             end: CharIndex,
                             env: TypeEnvironment,
                             state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    id.value match {
      case LinkedInfo.Local(depth, slot, name, kind) =>
        env.lookup(name) match {
          case Some((foundDepth, binding)) =>
            val typedInfo = TypedInfo.Local(foundDepth, binding.slot, name, binding.kind, binding.tpe)
            val attrId = attrIdent(typedInfo, id.begin, id.end, binding.tpe)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, binding.tpe)
            (attr, state)
          case None =>
            val err = SemanticError.UnknownIdentifier(name)
            val s1 = state.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
            (attr, s1)
        }

      case LinkedInfo.Bound(symbol) =>
        val tpe = symbol match {
          case NativeFunctionSymbolId(nfId) =>
            methodResolver.getNativeFunctionDefinition(nfId).fold[ProperType[TypeId]](Type.singleton(symbol)) { fnDef =>
              fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            }
          case _ => Type.singleton(symbol)
        }
        val typedInfo = TypedInfo.Bound(symbol, tpe)
        val attrId = attrIdent(typedInfo, id.begin, id.end, tpe)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, tpe)
        (attr, state)

      case LinkedInfo.StaticFunctionCandidates(name, _, candidates) =>
        OverloadResolver.resolveFunctionCall(typeUniverse, candidates, Vector.empty) match {
          case Right(fnDef) =>
            val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s1 = state.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.StaticFunctionCall(fnDef, retType)
            val attrId = attrIdent(typedInfo, id.begin, id.end, retType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, retType)
            (attr, s1)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchFunction(name, Vector.empty)
            val s1 = state.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
            (attr, s1)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(name, candidates.size)
            val s1 = state.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
            (attr, s1)
        }

      case LinkedInfo.MethodCandidates(name, _, candidates) =>
        OverloadResolver.resolveFunctionCall(typeUniverse, candidates, Vector.empty) match {
          case Right(fnDef) =>
            val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s1 = state.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.MethodCall(fnDef, retType)
            val attrId = attrIdent(typedInfo, id.begin, id.end, retType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, retType)
            (attr, s1)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchFunction(name, Vector.empty)
            val s1 = state.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
            (attr, s1)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(name, candidates.size)
            val s1 = state.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
            (attr, s1)
        }

      case LinkedInfo.AmbiguousName(name, candidateCount) =>
        val err = SemanticError.AmbiguousCall(name, candidateCount)
        val s1 = state.addError(Pos(err, id.begin, id.end))
        val typedInfo = TypedInfo.Error(err)
        val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
        (attr, s1)

      case LinkedInfo.FieldSite(name) =>
        val err = SemanticError.UnknownIdentifier(name)
        val s1 = state.addError(Pos(err, id.begin, id.end))
        val typedInfo = TypedInfo.Error(err)
        val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
        (attr, s1)

      case LinkedInfo.Error(err) =>
        val s1 = state.addError(Pos(err, id.begin, id.end))
        val typedInfo = TypedInfo.Error(err)
        val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), begin, end, CoreTypes.NothingT)
        (attr, s1)
    }
  }

  private def checkSelect(qualifier: Pos[LinkedPhase.Expression[Pos]],
                          name: Pos[LinkedPhase.Ident],
                          begin: CharIndex,
                          end: CharIndex,
                          env: TypeEnvironment,
                          state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val (qualAttr, s1) = checkPosExpression(qualifier, env, state)
    val qualType = qualAttr.properType
    name.value match {
      case LinkedInfo.FieldSite(fieldName) =>
        methodResolver.resolveBestMethod(qualType, Name(fieldName), SignatureQuery.empty) match {
          case Right(fnDef) =>
            val fieldType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s2 = s1.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.FieldAccess(fieldName, fieldType)
            val attrName = attrIdent(typedInfo, name.begin, name.end, fieldType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, fieldType)
            (attr, s2)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchField(qualType, fieldName)
            val s2 = s1.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
            (attr, s2)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(fieldName, 2)
            val s2 = s1.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
            (attr, s2)
        }

      case LinkedInfo.MethodCandidates(nameStr, _, candidates) =>
        OverloadResolver.resolveMethodCall(typeUniverse, candidates, qualType, Vector.empty) match {
          case Right(fnDef) =>
            val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s2 = s1.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.MethodCall(fnDef, retType)
            val attrName = attrIdent(typedInfo, name.begin, name.end, retType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, retType)
            (attr, s2)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchMethod(qualType, nameStr, Vector.empty)
            val s2 = s1.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
            (attr, s2)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
            val s2 = s1.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
            (attr, s2)
        }

      case LinkedInfo.StaticFunctionCandidates(nameStr, _, candidates) =>
        OverloadResolver.resolveFunctionCall(typeUniverse, candidates, Vector.empty) match {
          case Right(fnDef) =>
            val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s2 = s1.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.StaticFunctionCall(fnDef, retType)
            val attrName = attrIdent(typedInfo, name.begin, name.end, retType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, retType)
            (attr, s2)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchFunction(nameStr, Vector.empty)
            val s2 = s1.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
            (attr, s2)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
            val s2 = s1.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
            (attr, s2)
        }

      case LinkedInfo.Error(err) =>
        val s2 = s1.addError(Pos(err, name.begin, name.end))
        val typedInfo = TypedInfo.Error(err)
        val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
        (attr, s2)

      case other =>
        val err = SemanticError.UnknownIdentifier(other.toString)
        val s2 = s1.addError(Pos(err, name.begin, name.end))
        val typedInfo = TypedInfo.Error(err)
        val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), begin, end, CoreTypes.NothingT)
        (attr, s2)
    }
  }

  private def checkTyped(expression: Pos[LinkedPhase.Expression[Pos]],
                         ascription: Pos[LinkedPhase.TypeIdent[Pos]],
                         begin: CharIndex,
                         end: CharIndex,
                         env: TypeEnvironment,
                         state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val (exprAttr, s1) = checkPosExpression(expression, env, state)
    val exprType = exprAttr.properType
    ascription.value match {
      case Right(targetType) =>
        val isSub = typeUniverse.hierarchy.isSubtype(exprType, targetType)
        val s2 = if (isSub) s1 else s1.addError(Pos(SemanticError.TypeMismatch(targetType, exprType), begin, end))
        val attrAscription = attrTypeIdent(Right(targetType), ascription.begin, ascription.end, targetType)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Typed(exprAttr, attrAscription), begin, end, targetType)
        (attr, s2)
      case Left(typeErr) =>
        val attrAscription = attrTypeIdent(Left(typeErr), ascription.begin, ascription.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Typed(exprAttr, attrAscription), begin, end, CoreTypes.NothingT)
        (attr, s1)
    }
  }

  private def checkTuple(elements: Vector[Pos[LinkedPhase.Expression[Pos]]],
                         begin: CharIndex,
                         end: CharIndex,
                         env: TypeEnvironment,
                         state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    case class TupleAcc(attrs: Vector[Attributed[TypedPhase.Expression[Attributed]]], st: State)
    val initial = TupleAcc(Vector.empty, state)
    val res = elements.foldLeft(initial) { (acc, el) =>
      val (attrEl, nextSt) = checkPosExpression(el, env, acc.st)
      TupleAcc(acc.attrs :+ attrEl, nextSt)
    }
    val types = res.attrs.map(_.properType)
    val tupleType = if (types.size >= 2) {
      VectorTwoPlus.tryFrom(types).fold[ProperType[TypeId]](CoreTypes.UnitT)(Type.Tuple(_))
    } else if (types.size == 1) {
      types.head
    } else {
      CoreTypes.UnitT
    }
    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Tuple(res.attrs), begin, end, tupleType)
    (attr, res.st)
  }

  private def checkConditional(condition: Pos[LinkedPhase.Expression[Pos]],
                               thenBranch: Pos[LinkedPhase.Expression[Pos]],
                               elseBranch: Pos[LinkedPhase.Expression[Pos]],
                               begin: CharIndex,
                               end: CharIndex,
                               env: TypeEnvironment,
                               state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val (condAttr, s1) = checkPosExpression(condition, env, state)
    val isCondBool = typeUniverse.hierarchy.isSubtype(condAttr.properType, CoreTypes.BooleanT)
    val s2 = if (isCondBool) s1 else s1.addError(Pos(SemanticError.ConditionNotBoolean(condAttr.properType), condition.begin, condition.end))
    val (thenAttr, s3) = checkPosExpression(thenBranch, env, s2)
    val (elseAttr, s4) = checkPosExpression(elseBranch, env, s3)
    val lubType = typeUniverse.hierarchy.leastUpperBound(thenAttr.properType, elseAttr.properType).asInstanceOf[ProperType[TypeId]]
    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Conditional(condAttr, thenAttr, elseAttr), begin, end, lubType)
    (attr, s4)
  }

  private def checkLambda(params: Vector[Pos[LinkedPhase.LambdaParameter[Pos]]],
                          body: Pos[LinkedPhase.Expression[Pos]],
                          begin: CharIndex,
                          end: CharIndex,
                          env: TypeEnvironment,
                          state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    case class ParamAcc(bindings: Vector[TypedScopeBinding],
                        attrParams: Vector[Attributed[TypedPhase.LambdaParameter[Attributed]]],
                        paramTypes: Vector[ProperType[TypeId]],
                        st: State)
    val pInitial = ParamAcc(Vector.empty, Vector.empty, Vector.empty, state)
    val pRes = params.zipWithIndex.foldLeft(pInitial) { case (acc, (p, idx)) =>
      val paramName = p.value.name.value match {
        case LinkedInfo.Local(_, _, n, _) => n
        case _ => ""
      }
      val (paramType, attrTyp) = p.value.typ match {
        case Some(t) =>
          t.value match {
            case Right(tpe) => (tpe, Some(attrTypeIdent(Right(tpe), t.begin, t.end, tpe)))
            case Left(err) => (CoreTypes.NothingT, Some(attrTypeIdent(Left(err), t.begin, t.end, CoreTypes.NothingT)))
          }
        case None =>
          (CoreTypes.AnyT, None)
      }
      val typedInfo = TypedInfo.Local(0, idx, paramName, LocalBindingKind.Parameter, paramType)
      val attrName = attrIdent(typedInfo, p.value.name.begin, p.value.name.end, paramType)
      val attrParam = Attributed[TypedPhase.LambdaParameter[Attributed]](TypedPhase.LambdaParameter(attrName, attrTyp), p.begin, p.end, paramType)
      val binding = TypedScopeBinding(paramName, idx, LocalBindingKind.Parameter, paramType)
      ParamAcc(acc.bindings :+ binding, acc.attrParams :+ attrParam, acc.paramTypes :+ paramType, acc.st)
    }
    val lambdaEnv = env.pushLayer(pRes.bindings)
    val (bodyAttr, s1) = checkPosExpression(body, lambdaEnv, pRes.st)
    val fnType = Type.Function(pRes.paramTypes, bodyAttr.properType)
    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Lambda(pRes.attrParams, bodyAttr), begin, end, fnType)
    (attr, s1)
  }

  private def checkInterpolatedString(interpolator: LinkedInterpolator,
                                      initial: String,
                                      segments: Vector[(Pos[LinkedPhase.Expression[Pos]], String)],
                                      begin: CharIndex,
                                      end: CharIndex,
                                      env: TypeEnvironment,
                                      state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val s1 = interpolator match {
      case LinkedInterpolator.Error(err) => state.addError(Pos(err, begin, end))
      case _ => state
    }
    case class InterpAcc(attrSegments: Vector[(Attributed[TypedPhase.Expression[Attributed]], String)], st: State)
    val interpInit = InterpAcc(Vector.empty, s1)
    val interpRes = segments.foldLeft(interpInit) { case (acc, (expr, text)) =>
      val (attrExpr, nextSt) = checkPosExpression(expr, env, acc.st)
      InterpAcc(acc.attrSegments :+ (attrExpr, text), nextSt)
    }
    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.InterpolatedString(interpolator, initial, interpRes.attrSegments), begin, end, CoreTypes.StringT)
    (attr, interpRes.st)
  }

  private def checkCallInfix(left: Pos[LinkedPhase.Expression[Pos]],
                             operation: Pos[LinkedPhase.Ident],
                             typeArgs: Vector[Pos[LinkedPhase.TypeArgument[Pos]]],
                             right: Pos[LinkedPhase.Expression[Pos]],
                             begin: CharIndex,
                             end: CharIndex,
                             env: TypeEnvironment,
                             state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val (leftAttr, s1) = checkPosExpression(left, env, state)
    val (rightAttr, s2) = checkPosExpression(right, env, s1)
    val (attrTypeArgs, s3) = checkTypeArgs(typeArgs, s2)
    val leftType = leftAttr.properType
    val rightType = rightAttr.properType

    operation.value match {
      case LinkedInfo.MethodCandidates(nameStr, _, candidates) =>
        OverloadResolver.resolveMethodCall(typeUniverse, candidates, leftType, Vector(Vector(rightType))) match {
          case Right(fnDef) =>
            val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s4 = s3.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.MethodCall(fnDef, retType)
            val attrOp = attrIdent(typedInfo, operation.begin, operation.end, retType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, retType)
            (attr, s4)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchMethod(leftType, nameStr, Vector(rightType))
            val s4 = s3.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrOp = attrIdent(typedInfo, operation.begin, operation.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, CoreTypes.NothingT)
            (attr, s4)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
            val s4 = s3.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrOp = attrIdent(typedInfo, operation.begin, operation.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, CoreTypes.NothingT)
            (attr, s4)
        }

      case LinkedInfo.StaticFunctionCandidates(nameStr, _, candidates) =>
        OverloadResolver.resolveFunctionCall(typeUniverse, candidates, Vector(Vector(leftType, rightType))) match {
          case Right(fnDef) =>
            val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
            val s4 = s3.addContexts(fnDef.requireRuntimeContexts)
            val typedInfo = TypedInfo.StaticFunctionCall(fnDef, retType)
            val attrOp = attrIdent(typedInfo, operation.begin, operation.end, retType)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, retType)
            (attr, s4)
          case Left(ResolutionError.NotFound) =>
            val err = SemanticError.NoSuchFunction(nameStr, Vector(leftType, rightType))
            val s4 = s3.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrOp = attrIdent(typedInfo, operation.begin, operation.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, CoreTypes.NothingT)
            (attr, s4)
          case Left(ResolutionError.Ambiguous) =>
            val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
            val s4 = s3.addError(Pos(err, begin, end))
            val typedInfo = TypedInfo.Error(err)
            val attrOp = attrIdent(typedInfo, operation.begin, operation.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, CoreTypes.NothingT)
            (attr, s4)
        }

      case LinkedInfo.Error(err) =>
        val s4 = s3.addError(Pos(err, operation.begin, operation.end))
        val typedInfo = TypedInfo.Error(err)
        val attrOp = attrIdent(typedInfo, operation.begin, operation.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, CoreTypes.NothingT)
        (attr, s4)

      case other =>
        val err = SemanticError.UnknownIdentifier(other.toString)
        val s4 = s3.addError(Pos(err, operation.begin, operation.end))
        val typedInfo = TypedInfo.Error(err)
        val attrOp = attrIdent(typedInfo, operation.begin, operation.end, CoreTypes.NothingT)
        val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Infix(leftAttr, attrOp, attrTypeArgs, rightAttr), begin, end, CoreTypes.NothingT)
        (attr, s4)
    }
  }

  private def checkCallStandard(target: Pos[LinkedPhase.Expression[Pos]],
                                typeArgs: Vector[Pos[LinkedPhase.TypeArgument[Pos]]],
                                args: Vector[Pos[LinkedPhase.ArgumentGroup[Pos]]],
                                begin: CharIndex,
                                end: CharIndex,
                                env: TypeEnvironment,
                                state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val (attrTypeArgs, s1) = checkTypeArgs(typeArgs, state)
    val (attrArgs, argTypes, s2) = checkArgumentGroups(args, env, s1)
    val flatArgTypes = argTypes.flatten

    target.value match {
      case LinkedPhase.Reference(id) =>
        id.value match {
          case LinkedInfo.StaticFunctionCandidates(name, _, candidates) =>
            OverloadResolver.resolveFunctionCall(typeUniverse, candidates, argTypes) match {
              case Right(fnDef) =>
                val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
                val s3 = s2.addContexts(fnDef.requireRuntimeContexts)
                val typedInfo = TypedInfo.StaticFunctionCall(fnDef, retType)
                val attrId = attrIdent(typedInfo, id.begin, id.end, retType)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, retType)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, retType)
                (attr, s3)
              case Left(ResolutionError.NotFound) =>
                val err = SemanticError.NoSuchFunction(name, flatArgTypes)
                val s3 = s2.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s3)
              case Left(ResolutionError.Ambiguous) =>
                val err = SemanticError.AmbiguousCall(name, candidates.size)
                val s3 = s2.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s3)
            }

          case LinkedInfo.MethodCandidates(name, _, candidates) =>
            OverloadResolver.resolveFunctionCall(typeUniverse, candidates, argTypes) match {
              case Right(fnDef) =>
                val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
                val s3 = s2.addContexts(fnDef.requireRuntimeContexts)
                val typedInfo = TypedInfo.MethodCall(fnDef, retType)
                val attrId = attrIdent(typedInfo, id.begin, id.end, retType)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, retType)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, retType)
                (attr, s3)
              case Left(ResolutionError.NotFound) =>
                val err = SemanticError.NoSuchFunction(name, flatArgTypes)
                val s3 = s2.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s3)
              case Left(ResolutionError.Ambiguous) =>
                val err = SemanticError.AmbiguousCall(name, candidates.size)
                val s3 = s2.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s3)
            }

          case LinkedInfo.Local(depth, slot, name, kind) =>
            env.lookup(name) match {
              case Some((foundDepth, binding)) =>
                val tpe = binding.tpe
                val typedInfo = TypedInfo.Local(foundDepth, binding.slot, name, binding.kind, tpe)
                val attrId = attrIdent(typedInfo, id.begin, id.end, tpe)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, tpe)
                tpe match {
                  case Type.Function(paramTypes, resultType) =>
                    val s3 = if (paramTypes.size != flatArgTypes.size) {
                      s2.addError(Pos(SemanticError.NoSuchFunction(name, flatArgTypes), begin, end))
                    } else {
                      paramTypes.zip(flatArgTypes).foldLeft(s2) { case (accSt, (expected, actual)) =>
                        if (!typeUniverse.hierarchy.isSubtype(actual, expected)) {
                          accSt.addError(Pos(SemanticError.TypeMismatch(expected, actual), begin, end))
                        } else accSt
                      }
                    }
                    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, resultType)
                    (attr, s3)
                  case other =>
                    val s3 = s2.addError(Pos(SemanticError.NotCallable(other), begin, end))
                    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                    (attr, s3)
                }
              case None =>
                val err = SemanticError.UnknownIdentifier(name)
                val s3 = s2.addError(Pos(err, id.begin, id.end))
                val typedInfo = TypedInfo.Error(err)
                val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s3)
            }

          case LinkedInfo.Error(err) =>
            val s3 = s2.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
            (attr, s3)

          case other =>
            val err = SemanticError.UnknownIdentifier(other.toString)
            val s3 = s2.addError(Pos(err, id.begin, id.end))
            val typedInfo = TypedInfo.Error(err)
            val attrId = attrIdent(typedInfo, id.begin, id.end, CoreTypes.NothingT)
            val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Reference(attrId), target.begin, target.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
            (attr, s3)
        }

      case LinkedPhase.Select(qualifier, nameIdent) =>
        val (qualAttr, s3) = checkPosExpression(qualifier, env, s2)
        val qualType = qualAttr.properType
        nameIdent.value match {
          case LinkedInfo.MethodCandidates(nameStr, _, candidates) =>
            OverloadResolver.resolveMethodCall(typeUniverse, candidates, qualType, argTypes) match {
              case Right(fnDef) =>
                val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
                val s4 = s3.addContexts(fnDef.requireRuntimeContexts)
                val typedInfo = TypedInfo.MethodCall(fnDef, retType)
                val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, retType)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, retType)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, retType)
                (attr, s4)
              case Left(ResolutionError.NotFound) =>
                val err = SemanticError.NoSuchMethod(qualType, nameStr, flatArgTypes)
                val s4 = s3.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s4)
              case Left(ResolutionError.Ambiguous) =>
                val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
                val s4 = s3.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s4)
            }

          case LinkedInfo.FieldSite(nameStr) =>
            val candidates = methodResolver.getMethodCandidates(qualType, Name(nameStr), SignatureQuery.empty)
            if (candidates.nonEmpty) {
              OverloadResolver.resolveMethodCall(typeUniverse, candidates.toVector, qualType, argTypes) match {
                case Right(fnDef) =>
                  val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
                  val s4 = s3.addContexts(fnDef.requireRuntimeContexts)
                  val typedInfo = TypedInfo.MethodCall(fnDef, retType)
                  val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, retType)
                  val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, retType)
                  val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, retType)
                  (attr, s4)
                case Left(ResolutionError.NotFound) =>
                  val err = SemanticError.NoSuchMethod(qualType, nameStr, flatArgTypes)
                  val s4 = s3.addError(Pos(err, begin, end))
                  val typedInfo = TypedInfo.Error(err)
                  val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
                  val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
                  val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                  (attr, s4)
                case Left(ResolutionError.Ambiguous) =>
                  val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
                  val s4 = s3.addError(Pos(err, begin, end))
                  val typedInfo = TypedInfo.Error(err)
                  val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
                  val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
                  val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                  (attr, s4)
              }
            } else {
              val err = SemanticError.NoSuchMethod(qualType, nameStr, flatArgTypes)
              val s4 = s3.addError(Pos(err, begin, end))
              val typedInfo = TypedInfo.Error(err)
              val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
              val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
              val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
              (attr, s4)
            }

          case LinkedInfo.StaticFunctionCandidates(nameStr, _, candidates) =>
            OverloadResolver.resolveFunctionCall(typeUniverse, candidates, argTypes) match {
              case Right(fnDef) =>
                val retType = fnDef.returnType.asInstanceOf[ProperType[TypeId]]
                val s4 = s3.addContexts(fnDef.requireRuntimeContexts)
                val typedInfo = TypedInfo.StaticFunctionCall(fnDef, retType)
                val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, retType)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, retType)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, retType)
                (attr, s4)
              case Left(ResolutionError.NotFound) =>
                val err = SemanticError.NoSuchFunction(nameStr, flatArgTypes)
                val s4 = s3.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s4)
              case Left(ResolutionError.Ambiguous) =>
                val err = SemanticError.AmbiguousCall(nameStr, candidates.size)
                val s4 = s3.addError(Pos(err, begin, end))
                val typedInfo = TypedInfo.Error(err)
                val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
                val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
                val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
                (attr, s4)
            }

          case LinkedInfo.Error(err) =>
            val s4 = s3.addError(Pos(err, nameIdent.begin, nameIdent.end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
            val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
            (attr, s4)

          case other =>
            val err = SemanticError.UnknownIdentifier(other.toString)
            val s4 = s3.addError(Pos(err, nameIdent.begin, nameIdent.end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, nameIdent.begin, nameIdent.end, CoreTypes.NothingT)
            val attrTarget = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Select(qualAttr, attrName), target.begin, target.end, CoreTypes.NothingT)
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
            (attr, s4)
        }

      case other =>
        val (attrTarget, s3) = checkPosExpression(target, env, s2)
        val targetType = attrTarget.properType
        targetType match {
          case Type.Function(paramTypes, resultType) =>
            val s4 = if (paramTypes.size != flatArgTypes.size) {
              s3.addError(Pos(SemanticError.NoSuchFunction("<function>", flatArgTypes), begin, end))
            } else {
              paramTypes.zip(flatArgTypes).foldLeft(s3) { case (accSt, (expected, actual)) =>
                if (!typeUniverse.hierarchy.isSubtype(actual, expected)) {
                  accSt.addError(Pos(SemanticError.TypeMismatch(expected, actual), begin, end))
                } else accSt
              }
            }
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, resultType)
            (attr, s4)
          case nonCallable =>
            val s4 = s3.addError(Pos(SemanticError.NotCallable(nonCallable), begin, end))
            val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Call.Standard(attrTarget, attrTypeArgs, attrArgs), begin, end, CoreTypes.NothingT)
            (attr, s4)
        }
    }
  }

  private def checkBlock(declarations: Vector[Pos[LinkedPhase.Declaration[Pos]]],
                         result: Pos[LinkedPhase.Expression[Pos]],
                         begin: CharIndex,
                         end: CharIndex,
                         env: TypeEnvironment,
                         state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    case class BlockAcc(currentEnv: TypeEnvironment,
                        attrDecls: Vector[Attributed[TypedPhase.Declaration[Attributed]]],
                        st: State)
    val blockInit = BlockAcc(env.pushLayer(Vector.empty), Vector.empty, state)
    val blockRes = declarations.foldLeft(blockInit) { (acc, decl) =>
      decl.value match {
        case LinkedPhase.Declaration.Val(pattern, rhs) =>
          val (rhsAttr, s1) = checkPosExpression(rhs, acc.currentEnv, acc.st)
          val rhsType = rhsAttr.properType
          val irrefutableErrOpt = patternChecker.checkValPatternIrrefutable(pattern, rhsType)
          val s2 = irrefutableErrOpt.fold(s1)(err => s1.addError(Pos(err, pattern.begin, pattern.end)))
          val (attrPattern, newBindings, s3) = checkPattern(pattern, rhsType, s2)
          val nextEnv = acc.currentEnv.extendCurrentLayer(newBindings)
          val attrDecl: Attributed[TypedPhase.Declaration[Attributed]] = Attributed[TypedPhase.Declaration[Attributed]](TypedPhase.Declaration.Val(attrPattern, rhsAttr), decl.begin, decl.end, CoreTypes.UnitT)
          BlockAcc(nextEnv, acc.attrDecls :+ attrDecl, s3)

        case LinkedPhase.Declaration.LazyVal(pattern, rhs) =>
          val (rhsAttr, s1) = checkPosExpression(rhs, acc.currentEnv, acc.st)
          val rhsType = rhsAttr.properType
          val irrefutableErrOpt = patternChecker.checkValPatternIrrefutable(pattern, rhsType)
          val s2 = irrefutableErrOpt.fold(s1)(err => s1.addError(Pos(err, pattern.begin, pattern.end)))
          val (attrPattern, newBindings, s3) = checkPattern(pattern, rhsType, s2)
          val nextEnv = acc.currentEnv.extendCurrentLayer(newBindings)
          val attrDecl: Attributed[TypedPhase.Declaration[Attributed]] = Attributed[TypedPhase.Declaration[Attributed]](TypedPhase.Declaration.LazyVal(attrPattern, rhsAttr), decl.begin, decl.end, CoreTypes.UnitT)
          BlockAcc(nextEnv, acc.attrDecls :+ attrDecl, s3)

        case LinkedPhase.Declaration.Def(name, paramGroups, returnType, body) =>
          val (defSlot, defName) = name.value match {
            case LinkedInfo.Local(_, slot, n, _) => (slot, n)
            case _ => (acc.currentEnv.currentLayerSize, "")
          }
          val (attrParamGroups, paramBindings, paramTypes, s1) = checkFormalParamGroups(paramGroups, acc.currentEnv, acc.st)
          val defEnv = acc.currentEnv.pushLayer(paramBindings)
          val (bodyAttr, s2) = checkPosExpression(body, defEnv, s1)
          val bodyType = bodyAttr.properType
          val (declaredRetTypeOpt, attrRetType, s3) = returnType match {
            case Some(rt) =>
              rt.value match {
                case Right(tpe) =>
                  val isSub = typeUniverse.hierarchy.isSubtype(bodyType, tpe)
                  val sNext = if (isSub) s2 else s2.addError(Pos(SemanticError.TypeMismatch(tpe, bodyType), body.begin, body.end))
                  (Some(tpe), Some(attrTypeIdent(Right(tpe), rt.begin, rt.end, tpe)), sNext)
                case Left(err) =>
                  (None, Some(attrTypeIdent(Left(err), rt.begin, rt.end, CoreTypes.NothingT)), s2)
              }
            case None =>
              (None, None, s2)
          }
          val finalRetType = declaredRetTypeOpt.getOrElse(bodyType)
          val fnType = Type.Function(paramTypes, finalRetType)
          val typedInfo = TypedInfo.Local(0, defSlot, defName, LocalBindingKind.Def, fnType)
          val attrName = attrIdent(typedInfo, name.begin, name.end, fnType)
          val defBinding = TypedScopeBinding(defName, defSlot, LocalBindingKind.Def, fnType)
          val nextEnv = acc.currentEnv.extendCurrentLayer(Vector(defBinding))
          val attrDecl: Attributed[TypedPhase.Declaration[Attributed]] = Attributed[TypedPhase.Declaration[Attributed]](TypedPhase.Declaration.Def(attrName, attrParamGroups, attrRetType, bodyAttr), decl.begin, decl.end, CoreTypes.UnitT)
          BlockAcc(nextEnv, acc.attrDecls :+ attrDecl, s3)

        case LinkedPhase.Declaration.Error(err) =>
          val attrDecl: Attributed[TypedPhase.Declaration[Attributed]] = Attributed[TypedPhase.Declaration[Attributed]](TypedPhase.Declaration.Error(err), decl.begin, decl.end, CoreTypes.UnitT)
          BlockAcc(acc.currentEnv, acc.attrDecls :+ attrDecl, acc.st)
      }
    }
    val (resultAttr, sFinal) = checkPosExpression(result, blockRes.currentEnv, blockRes.st)
    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Block(blockRes.attrDecls, resultAttr), begin, end, resultAttr.properType)
    (attr, sFinal)
  }

  private def checkMatch(expression: Pos[LinkedPhase.Expression[Pos]],
                         cases: Vector[Pos[LinkedPhase.Case[Pos]]],
                         begin: CharIndex,
                         end: CharIndex,
                         env: TypeEnvironment,
                         state: State): (Attributed[TypedPhase.Expression[Attributed]], State) = {
    val (scrutAttr, s1) = checkPosExpression(expression, env, state)
    val scrutType = scrutAttr.properType
    val patternAnalysisResult = patternChecker.analyzeMatchExhaustiveness(scrutType, cases)
    val s2 = s1.addErrors(patternAnalysisResult.diagnostics)

    case class CaseAcc(attrCases: Vector[Attributed[TypedPhase.Case[Attributed]]],
                       bodyTypes: Vector[ProperType[TypeId]],
                       st: State)
    val caseInit = CaseAcc(Vector.empty, Vector.empty, s2)
    val caseRes = cases.foldLeft(caseInit) { (acc, c) =>
      val (attrPattern, patternBindings, sNext1) = checkPattern(c.value.pattern, scrutType, acc.st)
      val caseEnv = env.pushLayer(patternBindings)
      val (attrGuard, sNext2) = c.value.guard match {
        case Some(g) =>
          val (gAttr, gSt) = checkPosExpression(g, caseEnv, sNext1)
          val isBool = typeUniverse.hierarchy.isSubtype(gAttr.properType, CoreTypes.BooleanT)
          val gSt2 = if (isBool) gSt else gSt.addError(Pos(SemanticError.ConditionNotBoolean(gAttr.properType), g.begin, g.end))
          (Some(gAttr), gSt2)
        case None =>
          (None, sNext1)
      }
      val (bodyAttr, sNext3) = checkPosExpression(c.value.body, caseEnv, sNext2)
      val attrCase = Attributed[TypedPhase.Case[Attributed]](TypedPhase.Case(attrPattern, attrGuard, bodyAttr), c.begin, c.end, bodyAttr.properType)
      CaseAcc(acc.attrCases :+ attrCase, acc.bodyTypes :+ bodyAttr.properType, sNext3)
    }

    val matchType = if (caseRes.bodyTypes.nonEmpty) {
      caseRes.bodyTypes.tail.foldLeft(caseRes.bodyTypes.head) { (acc, t) =>
        typeUniverse.hierarchy.leastUpperBound(acc, t).asInstanceOf[ProperType[TypeId]]
      }
    } else {
      CoreTypes.NothingT
    }
    val attr = Attributed[TypedPhase.Expression[Attributed]](TypedPhase.Match(scrutAttr, caseRes.attrCases), begin, end, matchType)
    (attr, caseRes.st)
  }

  private def checkPattern(pattern: Pos[LinkedPhase.Pattern[Pos]],
                           targetType: ProperType[TypeId],
                           state: State): (Attributed[TypedPhase.Pattern[Attributed]], Vector[TypedScopeBinding], State) = {
    patternChecker.typePattern(pattern, targetType) match {
      case Right(bindingsMap) =>
        checkPatternNode(pattern, bindingsMap, targetType, state)
      case Left(err) =>
        val s1 = state.addError(Pos(err, pattern.begin, pattern.end))
        checkPatternNode(pattern, Map.empty, targetType, s1)
    }
  }

  private def checkPatternNode(pattern: Pos[LinkedPhase.Pattern[Pos]],
                               bindingsMap: Map[String, ProperType[TypeId]],
                               fallbackType: ProperType[TypeId],
                               state: State): (Attributed[TypedPhase.Pattern[Attributed]], Vector[TypedScopeBinding], State) = {
    pattern.value match {
      case LinkedPhase.Pattern.Identifier(name) =>
        name.value match {
          case LinkedInfo.Local(depth, slot, n, kind) =>
            val tpe = bindingsMap.getOrElse(n, fallbackType)
            val typedInfo = TypedInfo.Local(depth, slot, n, kind, tpe)
            val attrName = attrIdent(typedInfo, name.begin, name.end, tpe)
            val binding = TypedScopeBinding(n, slot, kind, tpe)
            val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Identifier(attrName), pattern.begin, pattern.end, tpe)
            (attrPat, Vector(binding), state)
          case LinkedInfo.Error(err) =>
            val s1 = state.addError(Pos(err, name.begin, name.end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Identifier(attrName), pattern.begin, pattern.end, CoreTypes.NothingT)
            (attrPat, Vector.empty, s1)
          case other =>
            val err = SemanticError.UnknownIdentifier(other.toString)
            val s1 = state.addError(Pos(err, name.begin, name.end))
            val typedInfo = TypedInfo.Error(err)
            val attrName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Identifier(attrName), pattern.begin, pattern.end, CoreTypes.NothingT)
            (attrPat, Vector.empty, s1)
        }

      case LinkedPhase.Pattern.Wildcard() =>
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Wildcard[Attributed](), pattern.begin, pattern.end, fallbackType)
        (attrPat, Vector.empty, state)

      case LinkedPhase.Pattern.Literal(value) =>
        val (attrLit, s1) = checkLiteral(value.value, value.begin, value.end, state)
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Literal(attrLit), pattern.begin, pattern.end, attrLit.properType)
        (attrPat, Vector.empty, s1)

      case LinkedPhase.Pattern.As(name, inner) =>
        val (attrInner, innerBindings, s1) = checkPatternNode(inner, bindingsMap, fallbackType, state)
        val (attrName, nameBinding, s2) = name.value match {
          case LinkedInfo.Local(depth, slot, n, kind) =>
            val tpe = bindingsMap.getOrElse(n, fallbackType)
            val typedInfo = TypedInfo.Local(depth, slot, n, kind, tpe)
            val aName = attrIdent(typedInfo, name.begin, name.end, tpe)
            val binding = TypedScopeBinding(n, slot, kind, tpe)
            (aName, Some(binding), s1)
          case LinkedInfo.Error(err) =>
            val sNext = s1.addError(Pos(err, name.begin, name.end))
            val typedInfo = TypedInfo.Error(err)
            val aName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            (aName, None, sNext)
          case other =>
            val err = SemanticError.UnknownIdentifier(other.toString)
            val sNext = s1.addError(Pos(err, name.begin, name.end))
            val typedInfo = TypedInfo.Error(err)
            val aName = attrIdent(typedInfo, name.begin, name.end, CoreTypes.NothingT)
            (aName, None, sNext)
        }
        val allBindings = nameBinding.fold(innerBindings)(b => innerBindings :+ b)
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.As(attrName, attrInner), pattern.begin, pattern.end, fallbackType)
        (attrPat, allBindings, s2)

      case LinkedPhase.Pattern.Typed(inner, ascription) =>
        val (ascribedType, attrAscription, s1) = ascription.value match {
          case Right(tpe) => (tpe, attrTypeIdent(Right(tpe), ascription.begin, ascription.end, tpe), state)
          case Left(err) => (fallbackType, attrTypeIdent(Left(err), ascription.begin, ascription.end, CoreTypes.NothingT), state)
        }
        val (attrInner, innerBindings, s2) = checkPatternNode(inner, bindingsMap, ascribedType, s1)
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Typed(attrInner, attrAscription), pattern.begin, pattern.end, ascribedType)
        (attrPat, innerBindings, s2)

      case LinkedPhase.Pattern.Tuple(elements) =>
        case class PatTupleAcc(attrElems: Vector[Attributed[TypedPhase.Pattern[Attributed]]],
                               bindings: Vector[TypedScopeBinding],
                               types: Vector[ProperType[TypeId]],
                               st: State)
        val (elemExpectedTypes, hasExpected) = fallbackType match {
          case Type.Tuple(elems) if elems.size == elements.size => (elems.toVector, true)
          case _ => (Vector.fill(elements.size)(CoreTypes.AnyT), false)
        }
        val initial = PatTupleAcc(Vector.empty, Vector.empty, Vector.empty, state)
        val res = elements.zipWithIndex.foldLeft(initial) { case (acc, (el, idx)) =>
          val expectedElemType = if (hasExpected) elemExpectedTypes(idx) else CoreTypes.AnyT
          val (attrEl, elBindings, nextSt) = checkPatternNode(el, bindingsMap, expectedElemType, acc.st)
          PatTupleAcc(acc.attrElems :+ attrEl, acc.bindings ++ elBindings, acc.types :+ attrEl.properType, nextSt)
        }
        val tupType = if (res.types.size >= 2) VectorTwoPlus.tryFrom(res.types).fold[ProperType[TypeId]](CoreTypes.UnitT)(Type.Tuple(_)) else CoreTypes.UnitT
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Tuple(res.attrElems), pattern.begin, pattern.end, tupType)
        (attrPat, res.bindings, res.st)

      case LinkedPhase.Pattern.Product(typeId, args) =>
        val (prodType, attrTypeId, s1) = typeId.value match {
          case Right(tpe) => (tpe, attrTypeIdent(Right(tpe), typeId.begin, typeId.end, tpe), state)
          case Left(err) => (fallbackType, attrTypeIdent(Left(err), typeId.begin, typeId.end, CoreTypes.NothingT), state)
        }
        case class PatProdAcc(attrArgs: Vector[Attributed[TypedPhase.Pattern[Attributed]]],
                              bindings: Vector[TypedScopeBinding],
                              st: State)
        val prodInit = PatProdAcc(Vector.empty, Vector.empty, s1)
        val prodRes = args.foldLeft(prodInit) { (acc, arg) =>
          val (attrArg, argBindings, nextSt) = checkPatternNode(arg, bindingsMap, CoreTypes.AnyT, acc.st)
          PatProdAcc(acc.attrArgs :+ attrArg, acc.bindings ++ argBindings, nextSt)
        }
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Product(attrTypeId, prodRes.attrArgs), pattern.begin, pattern.end, prodType)
        (attrPat, prodRes.bindings, prodRes.st)

      case LinkedPhase.Pattern.Error(err) =>
        val attrPat = Attributed[TypedPhase.Pattern[Attributed]](TypedPhase.Pattern.Error[Attributed](err), pattern.begin, pattern.end, CoreTypes.NothingT)
        (attrPat, Vector.empty, state)
    }
  }

  private def checkFormalParamGroups(groups: Vector[Pos[LinkedPhase.FormalParameterGroup[Pos]]],
                                     env: TypeEnvironment,
                                     state: State): (Vector[Attributed[TypedPhase.FormalParameterGroup[Attributed]]], Vector[TypedScopeBinding], Vector[ProperType[TypeId]], State) = {
    case class GroupAcc(attrGroups: Vector[Attributed[TypedPhase.FormalParameterGroup[Attributed]]],
                        bindings: Vector[TypedScopeBinding],
                        types: Vector[ProperType[TypeId]],
                        st: State)
    val initial = GroupAcc(Vector.empty, Vector.empty, Vector.empty, state)
    val res = groups.foldLeft(initial) { (acc, group) =>
      case class ParamAcc(attrParams: Vector[Attributed[TypedPhase.FormalParameter[Attributed]]],
                          bindings: Vector[TypedScopeBinding],
                          types: Vector[ProperType[TypeId]],
                          st: State)
      val pInit = ParamAcc(Vector.empty, Vector.empty, Vector.empty, acc.st)
      val pRes = group.value.parameters.foldLeft(pInit) { (pAcc, p) =>
        val (slot, pName) = p.value.name.value match {
          case LinkedInfo.Local(_, s, n, _) => (s, n)
          case _ => (acc.bindings.size + pAcc.bindings.size, "")
        }
        val (pType, attrTyp) = p.value.typ.value match {
          case Right(tpe) => (tpe, attrTypeIdent(Right(tpe), p.value.typ.begin, p.value.typ.end, tpe))
          case Left(err) => (CoreTypes.NothingT, attrTypeIdent(Left(err), p.value.typ.begin, p.value.typ.end, CoreTypes.NothingT))
        }
        val (attrDefault, st2) = p.value.default match {
          case Some(d) =>
            val (dAttr, dSt) = checkPosExpression(d, env, pAcc.st)
            val isSub = typeUniverse.hierarchy.isSubtype(dAttr.properType, pType)
            val dSt2 = if (isSub) dSt else dSt.addError(Pos(SemanticError.TypeMismatch(pType, dAttr.properType), d.begin, d.end))
            (Some(dAttr), dSt2)
          case None =>
            (None, pAcc.st)
        }
        val typedInfo = TypedInfo.Local(0, slot, pName, LocalBindingKind.Parameter, pType)
        val attrName = attrIdent(typedInfo, p.value.name.begin, p.value.name.end, pType)
        val attrParam = Attributed[TypedPhase.FormalParameter[Attributed]](TypedPhase.FormalParameter(attrName, attrTyp, attrDefault), p.begin, p.end, pType)
        val binding = TypedScopeBinding(pName, slot, LocalBindingKind.Parameter, pType)
        ParamAcc(pAcc.attrParams :+ attrParam, pAcc.bindings :+ binding, pAcc.types :+ pType, st2)
      }

      val (attrVariadic, vBinding, vType, st3) = group.value.variadic match {
        case Some(v) =>
          val (vSlot, vName) = v.value.name.value match {
            case LinkedInfo.Local(_, s, n, _) => (s, n)
            case _ => (acc.bindings.size + pRes.bindings.size, "")
          }
          val (vType, attrTyp) = v.value.typ.value match {
            case Right(tpe) => (tpe, attrTypeIdent(Right(tpe), v.value.typ.begin, v.value.typ.end, tpe))
            case Left(err) => (CoreTypes.NothingT, attrTypeIdent(Left(err), v.value.typ.begin, v.value.typ.end, CoreTypes.NothingT))
          }
          val typedInfo = TypedInfo.Local(0, vSlot, vName, LocalBindingKind.Parameter, vType)
          val attrName = attrIdent(typedInfo, v.value.name.begin, v.value.name.end, vType)
          val attrVar = Attributed[TypedPhase.FormalParameter[Attributed]](TypedPhase.FormalParameter(attrName, attrTyp, None), v.begin, v.end, vType)
          val binding = TypedScopeBinding(vName, vSlot, LocalBindingKind.Parameter, vType)
          (Some(attrVar), Some(binding), Some(vType), pRes.st)
        case None =>
          (None, None, None, pRes.st)
      }

      val attrGroup = Attributed[TypedPhase.FormalParameterGroup[Attributed]](TypedPhase.FormalParameterGroup(pRes.attrParams, attrVariadic), group.begin, group.end, CoreTypes.UnitT)
      val allBindings = acc.bindings ++ pRes.bindings ++ vBinding.toVector
      val allTypes = acc.types ++ pRes.types ++ vType.toVector
      GroupAcc(acc.attrGroups :+ attrGroup, allBindings, allTypes, st3)
    }

    (res.attrGroups, res.bindings, res.types, res.st)
  }

  private def checkTypeArgs(typeArgs: Vector[Pos[LinkedPhase.TypeArgument[Pos]]],
                            state: State): (Vector[Attributed[TypedPhase.TypeArgument[Attributed]]], State) = {
    val attrs = typeArgs.foldLeft(Vector.empty[Attributed[TypedPhase.TypeArgument[Attributed]]]) { (acc, ta) =>
      val (attrTyp, tpe) = ta.value.typ.value match {
        case Right(t) => (attrTypeIdent(Right(t), ta.value.typ.begin, ta.value.typ.end, t), t)
        case Left(err) => (attrTypeIdent(Left(err), ta.value.typ.begin, ta.value.typ.end, CoreTypes.NothingT), CoreTypes.NothingT)
      }
      acc :+ Attributed[TypedPhase.TypeArgument[Attributed]](TypedPhase.TypeArgument(attrTyp), ta.begin, ta.end, tpe)
    }
    (attrs, state)
  }

  private def checkArgumentGroups(groups: Vector[Pos[LinkedPhase.ArgumentGroup[Pos]]],
                                  env: TypeEnvironment,
                                  state: State): (Vector[Attributed[TypedPhase.ArgumentGroup[Attributed]]], Vector[Vector[ProperType[TypeId]]], State) = {
    case class GroupsAcc(attrGroups: Vector[Attributed[TypedPhase.ArgumentGroup[Attributed]]],
                         groupTypes: Vector[Vector[ProperType[TypeId]]],
                         st: State)
    val initial = GroupsAcc(Vector.empty, Vector.empty, state)
    val res = groups.foldLeft(initial) { (acc, group) =>
      case class ArgGroupAcc(attrArgs: Vector[Attributed[TypedPhase.Argument[Attributed]]],
                             argTypes: Vector[ProperType[TypeId]],
                             st: State)
      val groupInit = ArgGroupAcc(Vector.empty, Vector.empty, acc.st)
      val groupRes = group.value.arguments.foldLeft(groupInit) { (gAcc, arg) =>
        val (attrVal, nextSt) = checkPosExpression(arg.value.value, env, gAcc.st)
        val attrName = arg.value.name.map { n =>
          attrIdent(TypedInfo.FieldAccess(n.value.toString, attrVal.properType), n.begin, n.end, attrVal.properType)
        }
        val attrArg = Attributed[TypedPhase.Argument[Attributed]](TypedPhase.Argument(attrVal, attrName), arg.begin, arg.end, attrVal.properType)
        ArgGroupAcc(gAcc.attrArgs :+ attrArg, gAcc.argTypes :+ attrVal.properType, nextSt)
      }
      val (attrSplat, finalSt) = group.value.splat match {
        case Some(splat) =>
          val (attrVal, nextSt) = checkPosExpression(splat.value.value, env, groupRes.st)
          val attrName = splat.value.name.map { n =>
            attrIdent(TypedInfo.FieldAccess(n.value.toString, attrVal.properType), n.begin, n.end, attrVal.properType)
          }
          val attrSplatArg = Attributed[TypedPhase.Argument[Attributed]](TypedPhase.Argument(attrVal, attrName), splat.begin, splat.end, attrVal.properType)
          (Some(attrSplatArg), nextSt)
        case None =>
          (None, groupRes.st)
      }
      val attrGroup = Attributed[TypedPhase.ArgumentGroup[Attributed]](TypedPhase.ArgumentGroup(groupRes.attrArgs, attrSplat), group.begin, group.end, CoreTypes.UnitT)
      GroupsAcc(acc.attrGroups :+ attrGroup, acc.groupTypes :+ groupRes.argTypes, finalSt)
    }
    (res.attrGroups, res.groupTypes, res.st)
  }
}
