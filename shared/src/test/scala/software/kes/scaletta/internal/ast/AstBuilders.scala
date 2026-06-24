package software.kes.scaletta.internal.ast

import software.kes.scaletta.internal.ast.Declaration.Def
import software.kes.scaletta.util.functional.Id._

import scala.language.implicitConversions

object AstBuilders {

  import software.kes.scaletta.internal.parser.ParseError
  import software.kes.scaletta.internal.scanner.Token

  def lit(n: Int): Expression[Id] = Literal.int(n)

  def lit(s: String): Expression[Id] = Literal.string(s)

  def lit(b: Boolean): Expression[Id] = Literal.boolean(b)

  def litNull: Expression[Id] = Literal.null_()

  def litUnit: Expression[Id] = Literal.unit()

  def errExpr(error: ParseError): Expression[Id] = Expression.Error[Id](error)

  def errMissing(context: String): Expression[Id] = errExpr(ParseError.MissingExpression(context))

  def errUnexpected(token: Token): Expression[Id] = errExpr(ParseError.UnexpectedToken(token))

  def errPat(error: ParseError): Pattern[Id] = Pattern.Error[Id](error)

  def ref(name: Identifier[Id]): Expression[Id] = Reference[Id](name)

  def select(qualifier: Expression[Id], name: Identifier[Id]): Expression[Id] =
    Select[Id](qualifier, name)

  def infix(left: Expression[Id], op: Identifier[Id], right: Expression[Id]): Expression[Id] =
    Call.infix[Id](left, op, Vector.empty, right)

  def call(target: Expression[Id]): CallBuilder =
    new CallBuilder(target, Vector.empty, Vector.empty)

  /**
   * Calls with only single argument group, and no type arguments.
   */
  def callSimple(target: Expression[Id], args: Argument[Id]*): Expression[Id] =
    call(target).group(ArgumentGroup[Id](args.toVector)).build()

  def typed(expr: Expression[Id], ascription: TypeIdentifier[Id]): Expression[Id] =
    Typed[Id](expr, ascription)

  implicit def tName(name: Identifier[Id]): TypeIdentifier[Id] = TypeIdentifier.name[Id](name)

  implicit def tName(name: String): TypeIdentifier[Id] = TypeIdentifier.name[Id](Identifier[Id](name))

  def tApplied(name: TypeIdentifier[Id], args: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.applied[Id](name, args: _*)

  def tFunc(params: Vector[TypeIdentifier[Id]], result: TypeIdentifier[Id]): TypeIdentifier[Id] =
    TypeIdentifier.function[Id](params, result)

  def tTuple(elements: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.tuple[Id](elements.toVector)

  def block(declarations: Vector[Declaration[Id]], result: Expression[Id]): Expression[Id] =
    Block[Id](declarations, result)

  def block(result: Expression[Id], decls: Declaration[Id]*): Expression[Id] =
    Block[Id](decls.toVector, result)

  def tuple(elements: Expression[Id]*): Expression[Id] =
    Tuple[Id](elements.toVector)

  def cond(condition: Expression[Id], thenBranch: Expression[Id], elseBranch: Expression[Id]): Expression[Id] =
    Conditional[Id](condition, thenBranch, elseBranch)

  def valDecl(pattern: Pattern[Id], rhs: Expression[Id]): Declaration[Id] =
    Declaration.val_[Id](pattern, rhs)

  def valId(name: String, rhs: Expression[Id]): Declaration[Id] =
    valDecl(pId(name), rhs)

  def valTypedId(name: String, typeName: String, rhs: Expression[Id]): Declaration[Id] =
    valDecl(pTypedId(name, typeName), rhs)

  def lazyValDecl(pattern: Pattern[Id], rhs: Expression[Id]): Declaration[Id] =
    Declaration.lazyVal[Id](pattern, rhs)

  def defDecl(name: String): DefDeclBuilder = {
    val initial = Def[Id](Identifier(name), Vector.empty, None, Literal.null_())
    new DefDeclBuilder(initial)
  }

  def defSimple(name: String, body: Expression[Id]): Declaration[Id] =
    defDecl(name).body(body)

  def pWild: Pattern[Id] = Pattern.Wildcard[Id]()

  def pId(name: Identifier[Id]): Pattern[Id] = Pattern.Identifier[Id](name)

  def pTypedId(name: String, typeId: TypeIdentifier[Id]): Pattern[Id] =
    pTyped(pId(name), typeId)

  def pWildTyped(typeId: TypeIdentifier[Id]): Pattern[Id] =
    pTyped(pWild, typeId)

  def pLit(expr: Literal[Id]): Pattern[Id] = Pattern.Literal[Id](expr)

  def pAs(name: Identifier[Id], pattern: Pattern[Id]): Pattern[Id] = Pattern.As[Id](name, pattern)

  def pAsTyped(name: String, pattern: Pattern[Id], typeName: String): Pattern[Id] =
    pTyped(pAs(name, pattern), typeName)

  def pTyped(pattern: Pattern[Id], typeId: TypeIdentifier[Id]): Pattern[Id] =
    Pattern.Typed[Id](pattern, typeId)

  def pTuple(elements: Pattern[Id]*): Pattern[Id] = Pattern.Tuple[Id](elements.toVector)

  def pProduct(typeId: TypeIdentifier[Id], args: Pattern[Id]*): Pattern[Id] =
    Pattern.Product[Id](typeId, args.toVector)

  def matchExpr(expr: Expression[Id], cases: Case[Id]*): Expression[Id] =
    Match[Id](expr, cases.toVector)

  def caseExpr(pat: Pattern[Id], result: Expression[Id]): Case[Id] =
    Case[Id](pat, None, result)

  def param(name: Identifier[Id], typ: TypeIdentifier[Id]): FormalParameter[Id] =
    FormalParameter[Id](name, typ, None)

  def param(name: Identifier[Id], typ: TypeIdentifier[Id], default: Expression[Id]): FormalParameter[Id] =
    FormalParameter[Id](name, typ, Some(default))

  def paramGroup(params: FormalParameter[Id]*): FormalParameterGroup[Id] =
    FormalParameterGroup[Id](params.toVector)

  implicit def arg(expression: Expression[Id]): Argument[Id] = Argument[Id](expression)

  def namedArg(name: String, expression: Expression[Id]): Argument[Id] =
    Argument[Id](expression, Some(Identifier[Id](name)))

  implicit def identifier(name: String): Identifier[Id] = Identifier[Id](name)

  final class DefDeclBuilder(private val result: Def[Id]) {
    def group(params: FormalParameter[Id]*): DefDeclBuilder =
      modify(_.copy(params = result.params :+ paramGroup(params: _*)))

    def returnType(typ: TypeIdentifier[Id]): DefDeclBuilder =
      modify(_.copy[Id](returnType = Some(typ)))

    def body(expression: Expression[Id]): Declaration[Id] =
      result.copy[Id](body = expression)

    private def modify(fn: Def[Id] => Def[Id]): DefDeclBuilder =
      new DefDeclBuilder(fn(result))
  }

  final class CallBuilder(private val target: Expression[Id],
                          private val typeArgs: Vector[TypeArgument[Id]],
                          private val argGroups: Vector[ArgumentGroup[Id]]) {

    def typeArg(args: TypeIdentifier[Id]*): CallBuilder =
      new CallBuilder(target, typeArgs ++ args.map(t => TypeArgument[Id](t)), argGroups)

    def group(args: Argument[Id]*): CallBuilder = {
      val newGroup = ArgumentGroup[Id](args.toVector)
      new CallBuilder(target, typeArgs, argGroups :+ newGroup)
    }

    def group(group: ArgumentGroup[Id]): CallBuilder =
      new CallBuilder(target, typeArgs, argGroups :+ group)

    def build(): Expression[Id] = {
      val finalGroups = if (argGroups.isEmpty) Vector(ArgumentGroup[Id](Vector.empty)) else argGroups
      Call.standard[Id](target, typeArgs, finalGroups)
    }
  }
}
