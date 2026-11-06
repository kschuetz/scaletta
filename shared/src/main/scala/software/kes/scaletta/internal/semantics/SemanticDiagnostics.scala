package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.DiagnosticMessage
import software.kes.scaletta.internal.ast.LinkedPhase._
import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.reporting.{LineMap, Pos}
import software.kes.scaletta.internal.types.TypeResolutionError

object SemanticDiagnostics {

  /**
   * Traverses a linked AST expression to collect all semantic and type resolution
   * errors into diagnostic messages mapped to source positions.
   */
  def collect(expression: Expression[Pos], lineMap: LineMap): Vector[DiagnosticMessage] = {
    val collector = new SemanticDiagnostics(lineMap)
    collector.collectFromExpression(expression)
  }

  /**
   * Traverses a positioned linked AST expression to collect all semantic and type resolution
   * errors into diagnostic messages mapped to source positions.
   */
  def collect(posExpression: Pos[Expression[Pos]], lineMap: LineMap): Vector[DiagnosticMessage] = {
    val collector = new SemanticDiagnostics(lineMap)
    collector.collectFromPosExpression(posExpression)
  }
}

/**
 * Traverses an attributed AST in the LinkedPhase to extract semantic and type errors
 * and project them into line-and-column indexed DiagnosticMessage objects for compiler reporting.
 */
final class SemanticDiagnostics private(lineMap: LineMap) {

  def collectFromPosExpression(posExpr: Pos[LinkedPhase.Expression[Pos]]): Vector[DiagnosticMessage] =
    collectFromExpression(posExpr.value)

  def collectFromExpression(expr: LinkedPhase.Expression[Pos]): Vector[DiagnosticMessage] = {
    expr match {
      case Expression.Error(_) =>
        Vector.empty

      case Literal.IntLiteral(_) | Literal.LongLiteral(_) | Literal.FloatLiteral(_) |
           Literal.DoubleLiteral(_) | Literal.True() | Literal.False() | Literal.Null() |
           Literal.CharLiteral(_) | Literal.StringLiteral(_) | Literal.UnitLiteral() =>
        Vector.empty

      case Reference(id) =>
        collectFromIdent(id)

      case Select(qualifier, name) =>
        collectFromPosExpression(qualifier) ++ collectFromIdent(name)

      case Conditional(condition, thenBranch, elseBranch) =>
        collectFromPosExpression(condition) ++
          collectFromPosExpression(thenBranch) ++
          collectFromPosExpression(elseBranch)

      case Call.Standard(target, typeArgs, args) =>
        val targetDiag = collectFromPosExpression(target)
        val typeArgsDiag = typeArgs.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, ta) =>
          acc ++ collectFromTypeIdent(ta.value.typ)
        }
        val argsDiag = args.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, ag) =>
          acc ++ collectFromArgumentGroup(ag)
        }
        targetDiag ++ typeArgsDiag ++ argsDiag

      case Call.Infix(left, operator, typeArgs, right) =>
        val leftDiag = collectFromPosExpression(left)
        val opDiag = collectFromIdent(operator)
        val typeArgsDiag = typeArgs.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, ta) =>
          acc ++ collectFromTypeIdent(ta.value.typ)
        }
        val rightDiag = collectFromPosExpression(right)
        leftDiag ++ opDiag ++ typeArgsDiag ++ rightDiag

      case Lambda(parameters, body) =>
        val paramDiag = parameters.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, param) =>
          acc ++ collectFromIdent(param.value.name) ++
            param.value.typ.fold(Vector.empty[DiagnosticMessage])(collectFromTypeIdent)
        }
        paramDiag ++ collectFromPosExpression(body)

      case InterpolatedString(_, _, segments) =>
        segments.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, segment) =>
          acc ++ collectFromPosExpression(segment._1)
        }

      case Typed(target, ascription) =>
        collectFromPosExpression(target) ++ collectFromTypeIdent(ascription)

      case Match(target, cases) =>
        val targetDiag = collectFromPosExpression(target)
        val casesDiag = cases.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, matchCase) =>
          acc ++ collectFromCase(matchCase)
        }
        targetDiag ++ casesDiag

      case Tuple(elements) =>
        elements.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, elem) =>
          acc ++ collectFromPosExpression(elem)
        }

      case Block(declarations, result) =>
        val declDiagnostics = declarations.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, decl) =>
          acc ++ collectFromDeclaration(decl)
        }
        declDiagnostics ++ collectFromPosExpression(result)
    }
  }

  private def collectFromDeclaration(decl: Pos[LinkedPhase.Declaration[Pos]]): Vector[DiagnosticMessage] = {
    decl.value match {
      case Declaration.Val(pattern, rhs) =>
        collectFromPattern(pattern) ++ collectFromPosExpression(rhs)

      case Declaration.LazyVal(pattern, rhs) =>
        collectFromPattern(pattern) ++ collectFromPosExpression(rhs)

      case Declaration.Def(name, parameters, returnType, rhs) =>
        val nameDiagnostics = collectFromIdent(name)
        val paramDiagnostics = parameters.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, group) =>
          acc ++ collectFromFormalParameterGroup(group)
        }
        val returnDiagnostics = returnType.fold(Vector.empty[DiagnosticMessage])(collectFromTypeIdent)
        val rhsDiagnostics = collectFromPosExpression(rhs)
        nameDiagnostics ++ paramDiagnostics ++ returnDiagnostics ++ rhsDiagnostics

      case Declaration.Error(_) =>
        Vector.empty
    }
  }

  private def collectFromPattern(pattern: Pos[LinkedPhase.Pattern[Pos]]): Vector[DiagnosticMessage] = {
    pattern.value match {
      case Pattern.Wildcard() =>
        Vector.empty

      case Pattern.Identifier(id) =>
        collectFromIdent(id)

      case Pattern.Literal(_) =>
        Vector.empty

      case Pattern.As(name, pat) =>
        collectFromIdent(name) ++ collectFromPattern(pat)

      case Pattern.Typed(pat, ascription) =>
        collectFromPattern(pat) ++ collectFromTypeIdent(ascription)

      case Pattern.Tuple(elements) =>
        elements.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, elem) =>
          acc ++ collectFromPattern(elem)
        }

      case Pattern.Product(typeId, args) =>
        val typeDiag = collectFromTypeIdent(typeId)
        val argsDiag = args.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, arg) =>
          acc ++ collectFromPattern(arg)
        }
        typeDiag ++ argsDiag

      case Pattern.Error(_) =>
        Vector.empty
    }
  }

  private def collectFromFormalParameterGroup(group: Pos[LinkedPhase.FormalParameterGroup[Pos]]): Vector[DiagnosticMessage] = {
    val paramDiagnostics = group.value.parameters.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, param) =>
      acc ++ collectFromFormalParameter(param)
    }
    val variadicDiagnostics = group.value.variadic.fold(Vector.empty[DiagnosticMessage])(collectFromFormalParameter)
    paramDiagnostics ++ variadicDiagnostics
  }

  private def collectFromFormalParameter(param: Pos[LinkedPhase.FormalParameter[Pos]]): Vector[DiagnosticMessage] = {
    val nameDiagnostics = collectFromIdent(param.value.name)
    val typeDiagnostics = collectFromTypeIdent(param.value.typ)
    val defaultDiagnostics = param.value.default.fold(Vector.empty[DiagnosticMessage])(collectFromPosExpression)
    nameDiagnostics ++ typeDiagnostics ++ defaultDiagnostics
  }

  private def collectFromArgumentGroup(group: Pos[LinkedPhase.ArgumentGroup[Pos]]): Vector[DiagnosticMessage] = {
    val argDiagnostics = group.value.arguments.foldLeft(Vector.empty[DiagnosticMessage]) { (acc, arg) =>
      acc ++ collectFromArgument(arg)
    }
    val splatDiagnostics = group.value.splat.fold(Vector.empty[DiagnosticMessage])(collectFromArgument)
    argDiagnostics ++ splatDiagnostics
  }

  private def collectFromArgument(arg: Pos[LinkedPhase.Argument[Pos]]): Vector[DiagnosticMessage] = {
    val nameDiagnostics = arg.value.name.fold(Vector.empty[DiagnosticMessage])(collectFromIdent)
    val exprDiagnostics = collectFromPosExpression(arg.value.value)
    nameDiagnostics ++ exprDiagnostics
  }

  private def collectFromCase(c: Pos[LinkedPhase.Case[Pos]]): Vector[DiagnosticMessage] = {
    val patternDiagnostics = collectFromPattern(c.value.pattern)
    val guardDiagnostics = c.value.guard.fold(Vector.empty[DiagnosticMessage])(collectFromPosExpression)
    val bodyDiagnostics = collectFromPosExpression(c.value.body)
    patternDiagnostics ++ guardDiagnostics ++ bodyDiagnostics
  }

  private def collectFromIdent(id: Pos[LinkedPhase.Ident[Pos]]): Vector[DiagnosticMessage] = {
    id.value.value match {
      case LinkedInfo.Error(err) =>
        Vector(toDiagnostic(id.value, formatSemanticError(err)))
      case _ =>
        Vector.empty
    }
  }

  private def collectFromTypeIdent(typeIdent: Pos[LinkedPhase.TypeIdent[Pos]]): Vector[DiagnosticMessage] = {
    typeIdent.value.value match {
      case Left(err) =>
        Vector(toDiagnostic(typeIdent.value, formatTypeResolutionError(err)))
      case Right(_) =>
        Vector.empty
    }
  }

  private def toDiagnostic(pos: Pos[_], message: String): DiagnosticMessage = {
    val begin = lineMap.indexToPosition(pos.begin)
    val end = lineMap.indexToPosition(pos.end)
    DiagnosticMessage(message, begin, end)
  }

  private def formatSemanticError(error: SemanticError): String = error match {
    case SemanticError.ForwardReference(name) =>
      s"Forward reference to '$name' is not allowed"
    case SemanticError.UnknownIdentifier(name) =>
      s"Unknown identifier '$name'"
    case SemanticError.DuplicateDefinition(name) =>
      s"Duplicate definition of '$name'"
  }

  private def formatTypeResolutionError(error: TypeResolutionError): String = error match {
    case TypeResolutionError.UnknownType(name) =>
      s"Unknown type '$name'"
    case TypeResolutionError.AmbiguousType(name, candidateCount) =>
      s"Ambiguous type '$name' ($candidateCount candidates)"
    case TypeResolutionError.NotAProperType(name) =>
      s"Type '$name' is not a proper type"
    case TypeResolutionError.NotApplicable(name) =>
      s"Type '$name' is not applicable"
    case TypeResolutionError.WrongArity(name, expected, actual) =>
      s"Type '$name' expects $expected type arguments, but got $actual"
  }
}
