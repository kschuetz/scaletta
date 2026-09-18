package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api._
import software.kes.scaletta.internal.ast.{PreResolutionPhase, TypeAscriptionPhase, TypeIdentifier}
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.internal.semantics.TypeAscriber.{Input, Output}
import software.kes.scaletta.internal.types.{ConjunctionType, TypeNameIndex, TypeResolutionError}

object TypeAscriber {
  def create(typeNameIndex: TypeNameIndex,
             importScope: ImportScope): TypeAscriber =
    new TypeAscriber(typeNameIndex, importScope)

  type Input = PreResolutionPhase.Expression[Pos]
  type Output = TypeAscriptionPhase.Expression[Pos]
}

/**
 * Resolves plain type identifiers (as produced by the parser) into concrete host types,
 * transitioning the AST from the PreResolutionPhase to the TypeAscriptionPhase. Every type
 * annotation is turned into an `Either[TypeResolutionError, ProperType[TypeId]]`, while
 * pre-resolved term/type identifiers are carried over unchanged.
 */
final class TypeAscriber private(typeNameIndex: TypeNameIndex,
                                 importScope: ImportScope) {

  def ascribeTypes(input: Input): Output = ascribeExpression(input)

  private def ascribePosExpression(posExpr: Pos[PreResolutionPhase.Expression[Pos]]): Pos[TypeAscriptionPhase.Expression[Pos]] =
    Pos(ascribeExpression(posExpr.value), posExpr.begin, posExpr.end)

  private def ascribeExpression(expr: PreResolutionPhase.Expression[Pos]): TypeAscriptionPhase.Expression[Pos] = {
    expr match {
      case PreResolutionPhase.Expression.Error(err) =>
        TypeAscriptionPhase.Expression.Error(err)

      case lit: PreResolutionPhase.Literal[Pos @unchecked] =>
        ascribeLiteral(lit)

      case PreResolutionPhase.Reference(id) =>
        TypeAscriptionPhase.Reference(id)

      case PreResolutionPhase.Select(qualifier, name) =>
        TypeAscriptionPhase.Select(ascribePosExpression(qualifier), name)

      case PreResolutionPhase.Typed(expression, ascription) =>
        TypeAscriptionPhase.Typed(ascribePosExpression(expression), ascribeTypeIdent(ascription))

      case PreResolutionPhase.Tuple(elements) =>
        TypeAscriptionPhase.Tuple(
          elements.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.Expression[Pos]]]) { (acc, el) =>
            acc :+ ascribePosExpression(el)
          }
        )

      case PreResolutionPhase.Conditional(condition, thenBranch, elseBranch) =>
        TypeAscriptionPhase.Conditional(
          ascribePosExpression(condition),
          ascribePosExpression(thenBranch),
          ascribePosExpression(elseBranch)
        )

      case PreResolutionPhase.Call.Infix(left, operation, typeArgs, right) =>
        TypeAscriptionPhase.Call.Infix(
          ascribePosExpression(left),
          operation,
          ascribeTypeArgs(typeArgs),
          ascribePosExpression(right)
        )

      case PreResolutionPhase.Call.Standard(target, typeArgs, args) =>
        TypeAscriptionPhase.Call.Standard(
          ascribePosExpression(target),
          ascribeTypeArgs(typeArgs),
          ascribeArgumentGroups(args)
        )

      case PreResolutionPhase.Lambda(params, body) =>
        TypeAscriptionPhase.Lambda(ascribeLambdaParameters(params), ascribePosExpression(body))

      case PreResolutionPhase.InterpolatedString(interpolator, initial, segments) =>
        val newSegments = segments.foldLeft(Vector.empty[(Pos[TypeAscriptionPhase.Expression[Pos]], String)]) {
          case (acc, (exprPart, textPart)) =>
            acc :+ (ascribePosExpression(exprPart), textPart)
        }
        TypeAscriptionPhase.InterpolatedString(interpolator, initial, newSegments)

      case PreResolutionPhase.Match(expression, cases) =>
        val newCases = cases.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.Case[Pos]]]) { (acc, c) =>
          acc :+ ascribeCase(c)
        }
        TypeAscriptionPhase.Match(ascribePosExpression(expression), newCases)

      case PreResolutionPhase.Block(declarations, result) =>
        val newDecls = declarations.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.Declaration[Pos]]]) { (acc, decl) =>
          acc :+ ascribeDeclaration(decl)
        }
        TypeAscriptionPhase.Block(newDecls, ascribePosExpression(result))
    }
  }

  private def ascribeLiteral(lit: PreResolutionPhase.Literal[Pos]): TypeAscriptionPhase.Literal[Pos] = {
    lit match {
      case PreResolutionPhase.Literal.IntLiteral(v) => TypeAscriptionPhase.Literal.IntLiteral(v)
      case PreResolutionPhase.Literal.LongLiteral(v) => TypeAscriptionPhase.Literal.LongLiteral(v)
      case PreResolutionPhase.Literal.FloatLiteral(v) => TypeAscriptionPhase.Literal.FloatLiteral(v)
      case PreResolutionPhase.Literal.DoubleLiteral(v) => TypeAscriptionPhase.Literal.DoubleLiteral(v)
      case PreResolutionPhase.Literal.True() => TypeAscriptionPhase.Literal.True()
      case PreResolutionPhase.Literal.False() => TypeAscriptionPhase.Literal.False()
      case PreResolutionPhase.Literal.Null() => TypeAscriptionPhase.Literal.Null()
      case PreResolutionPhase.Literal.CharLiteral(v) => TypeAscriptionPhase.Literal.CharLiteral(v)
      case PreResolutionPhase.Literal.StringLiteral(v) => TypeAscriptionPhase.Literal.StringLiteral(v)
      case PreResolutionPhase.Literal.UnitLiteral() => TypeAscriptionPhase.Literal.UnitLiteral()
    }
  }

  private def ascribeTypeArgs(typeArgs: Vector[Pos[PreResolutionPhase.TypeArgument[Pos]]]): Vector[Pos[TypeAscriptionPhase.TypeArgument[Pos]]] =
    typeArgs.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.TypeArgument[Pos]]]) { (acc, ta) =>
      acc :+ Pos(TypeAscriptionPhase.TypeArgument(ascribeTypeIdent(ta.value.typ)), ta.begin, ta.end)
    }

  private def ascribeArgumentGroups(groups: Vector[Pos[PreResolutionPhase.ArgumentGroup[Pos]]]): Vector[Pos[TypeAscriptionPhase.ArgumentGroup[Pos]]] =
    groups.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.ArgumentGroup[Pos]]]) { (acc, g) =>
      acc :+ ascribeArgumentGroup(g)
    }

  private def ascribeArgumentGroup(group: Pos[PreResolutionPhase.ArgumentGroup[Pos]]): Pos[TypeAscriptionPhase.ArgumentGroup[Pos]] = {
    val newArgs = group.value.arguments.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.Argument[Pos]]]) { (acc, arg) =>
      acc :+ ascribeArgument(arg)
    }
    val newSplat = group.value.splat.fold[Option[Pos[TypeAscriptionPhase.Argument[Pos]]]](None) { splat =>
      Some(ascribeArgument(splat))
    }
    Pos(TypeAscriptionPhase.ArgumentGroup(newArgs, newSplat), group.begin, group.end)
  }

  private def ascribeArgument(arg: Pos[PreResolutionPhase.Argument[Pos]]): Pos[TypeAscriptionPhase.Argument[Pos]] =
    Pos(TypeAscriptionPhase.Argument(ascribePosExpression(arg.value.value), arg.value.name), arg.begin, arg.end)

  private def ascribeLambdaParameters(params: Vector[Pos[PreResolutionPhase.LambdaParameter[Pos]]]): Vector[Pos[TypeAscriptionPhase.LambdaParameter[Pos]]] =
    params.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.LambdaParameter[Pos]]]) { (acc, p) =>
      val newTyp = p.value.typ.fold[Option[Pos[TypeAscriptionPhase.TypeIdent[Pos]]]](None) { t =>
        Some(ascribeTypeIdent(t))
      }
      acc :+ Pos(TypeAscriptionPhase.LambdaParameter(p.value.name, newTyp), p.begin, p.end)
    }

  private def ascribeFormalParameterGroups(groups: Vector[Pos[PreResolutionPhase.FormalParameterGroup[Pos]]]): Vector[Pos[TypeAscriptionPhase.FormalParameterGroup[Pos]]] =
    groups.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.FormalParameterGroup[Pos]]]) { (acc, g) =>
      val newParams = g.value.parameters.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.FormalParameter[Pos]]]) { (pAcc, p) =>
        pAcc :+ ascribeFormalParameter(p)
      }
      val newVariadic = g.value.variadic.fold[Option[Pos[TypeAscriptionPhase.FormalParameter[Pos]]]](None) { v =>
        Some(ascribeFormalParameter(v))
      }
      acc :+ Pos(TypeAscriptionPhase.FormalParameterGroup(newParams, newVariadic), g.begin, g.end)
    }

  private def ascribeFormalParameter(p: Pos[PreResolutionPhase.FormalParameter[Pos]]): Pos[TypeAscriptionPhase.FormalParameter[Pos]] = {
    val newDefault = p.value.default.fold[Option[Pos[TypeAscriptionPhase.Expression[Pos]]]](None) { d =>
      Some(ascribePosExpression(d))
    }
    Pos(TypeAscriptionPhase.FormalParameter(p.value.name, ascribeTypeIdent(p.value.typ), newDefault), p.begin, p.end)
  }

  private def ascribeCase(c: Pos[PreResolutionPhase.Case[Pos]]): Pos[TypeAscriptionPhase.Case[Pos]] = {
    val newGuard = c.value.guard.fold[Option[Pos[TypeAscriptionPhase.Expression[Pos]]]](None) { g =>
      Some(ascribePosExpression(g))
    }
    Pos(TypeAscriptionPhase.Case(ascribePattern(c.value.pattern), newGuard, ascribePosExpression(c.value.body)), c.begin, c.end)
  }

  private def ascribeDeclaration(decl: Pos[PreResolutionPhase.Declaration[Pos]]): Pos[TypeAscriptionPhase.Declaration[Pos]] = {
    val newDecl: TypeAscriptionPhase.Declaration[Pos] = decl.value match {
      case PreResolutionPhase.Declaration.Val(pattern, rhs) =>
        TypeAscriptionPhase.Declaration.Val(ascribePattern(pattern), ascribePosExpression(rhs))

      case PreResolutionPhase.Declaration.LazyVal(pattern, rhs) =>
        TypeAscriptionPhase.Declaration.LazyVal(ascribePattern(pattern), ascribePosExpression(rhs))

      case PreResolutionPhase.Declaration.Def(name, params, returnType, body) =>
        val newReturnType = returnType.fold[Option[Pos[TypeAscriptionPhase.TypeIdent[Pos]]]](None) { rt =>
          Some(ascribeTypeIdent(rt))
        }
        TypeAscriptionPhase.Declaration.Def(name, ascribeFormalParameterGroups(params), newReturnType, ascribePosExpression(body))

      case PreResolutionPhase.Declaration.Error(err) =>
        TypeAscriptionPhase.Declaration.Error(err)
    }
    Pos(newDecl, decl.begin, decl.end)
  }

  private def ascribePattern(pattern: Pos[PreResolutionPhase.Pattern[Pos]]): Pos[TypeAscriptionPhase.Pattern[Pos]] = {
    val resolved: TypeAscriptionPhase.Pattern[Pos] = pattern.value match {
      case PreResolutionPhase.Pattern.Identifier(name) =>
        TypeAscriptionPhase.Pattern.Identifier(name)

      case PreResolutionPhase.Pattern.Wildcard() =>
        TypeAscriptionPhase.Pattern.Wildcard()

      case PreResolutionPhase.Pattern.Literal(value) =>
        TypeAscriptionPhase.Pattern.Literal(Pos(ascribeLiteral(value.value), value.begin, value.end))

      case PreResolutionPhase.Pattern.As(name, pat) =>
        TypeAscriptionPhase.Pattern.As(name, ascribePattern(pat))

      case PreResolutionPhase.Pattern.Typed(pat, ascription) =>
        TypeAscriptionPhase.Pattern.Typed(ascribePattern(pat), ascribeTypeIdent(ascription))

      case PreResolutionPhase.Pattern.Tuple(elements) =>
        TypeAscriptionPhase.Pattern.Tuple(
          elements.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.Pattern[Pos]]]) { (acc, el) =>
            acc :+ ascribePattern(el)
          }
        )

      case PreResolutionPhase.Pattern.Product(typeId, args) =>
        TypeAscriptionPhase.Pattern.Product(
          ascribeTypeIdent(typeId),
          args.foldLeft(Vector.empty[Pos[TypeAscriptionPhase.Pattern[Pos]]]) { (acc, arg) =>
            acc :+ ascribePattern(arg)
          }
        )

      case PreResolutionPhase.Pattern.Error(error) =>
        TypeAscriptionPhase.Pattern.Error(error)
    }
    Pos(resolved, pattern.begin, pattern.end)
  }

  private def ascribeTypeIdent(ti: Pos[TypeIdentifier[Pos]]): Pos[TypeAscriptionPhase.TypeIdent[Pos]] = {
    val resolved = resolveTypeIdentifier(ti.value)
    Pos(Pos[Either[TypeResolutionError, ProperType[TypeId]]](resolved, ti.begin, ti.end), ti.begin, ti.end)
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
}
