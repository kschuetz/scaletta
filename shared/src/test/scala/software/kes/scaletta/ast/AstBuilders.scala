package software.kes.scaletta.ast

import software.kes.scaletta.ast.Declaration.Def
import software.kes.scaletta.util.functional.Id._

import scala.language.implicitConversions

object AstBuilders {
  def lit(n: Int): Expression[Id] = Literal.int(n)

  def lit(s: String): Expression[Id] = Literal.string(s)

  def lit(b: Boolean): Expression[Id] = Literal.boolean(b)

  def litNull: Expression[Id] = Literal.null_()

  def ref(name: String): Expression[Id] = Reference[Id](Identifier(name))

  def select(qualifier: Expression[Id], name: String): Expression[Id] =
    Select[Id](qualifier, Identifier(name))

  def infix(left: Expression[Id], op: String, right: Expression[Id]): Expression[Id] =
    Call.infix[Id](left, Identifier(op), Vector.empty, right)

  def call(target: Expression[Id]): CallBuilder =
    new CallBuilder(target, Vector.empty, Vector.empty)

  /**
   * Calls with 1 or more arguments in a single group, with no type arguments.
   */
  def callSimple(target: Expression[Id], first: Argument[Id], rest: Argument[Id]*): Expression[Id] =
    call(target).group(ArgumentGroup[Id](Vector(first) ++ rest.toVector)).build()

  def typed(expr: Expression[Id], ascription: TypeIdentifier[Id]): Expression[Id] =
    Typed[Id](expr, ascription)

  def tName(name: String): TypeIdentifier[Id] = TypeIdentifier.name[Id](Identifier[Id](name))

  def tApplied(name: String, args: TypeIdentifier[Id]*): TypeIdentifier[Id] =
    TypeIdentifier.applied[Id](tName(name), args: _*)

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

  def pId(name: String): Pattern[Id] = Pattern.Identifier[Id](Identifier(name))

  def pTypedId(name: String, typeName: String): Pattern[Id] =
    pTyped(pId(name), typeName)

  def pWildTyped(typeName: String): Pattern[Id] =
    pTyped(pWild, typeName)

  def pLit(expr: Literal[Id]): Pattern[Id] = Pattern.Literal[Id](expr)

  def pAs(name: String, pattern: Pattern[Id]): Pattern[Id] = Pattern.As[Id](Identifier(name), pattern)

  def pAsTyped(name: String, pattern: Pattern[Id], typeName: String): Pattern[Id] =
    pTyped(pAs(name, pattern), typeName)

  def pTyped(pattern: Pattern[Id], typeName: String): Pattern[Id] =
    Pattern.Typed[Id](pattern, TypeIdentifier.name[Id](Identifier[Id](typeName)))

  def pTuple(elements: Pattern[Id]*): Pattern[Id] = Pattern.Tuple[Id](elements.toVector)

  def pProduct(typeName: String, args: Pattern[Id]*): Pattern[Id] =
    Pattern.Product[Id](TypeIdentifier.name[Id](Identifier[Id](typeName)), args.toVector)

  def matchExpr(expr: Expression[Id], cases: Case[Id]*): Expression[Id] =
    Match[Id](expr, cases.toVector)

  def caseExpr(pat: Pattern[Id], result: Expression[Id]): Case[Id] =
    Case[Id](pat, None, result)

  def param(name: String, typ: String): FormalParameter[Id] =
    FormalParameter[Id](Identifier[Id](name), TypeIdentifier.name[Id](Identifier[Id](typ)), None)

  def param(name: String, typ: String, default: Expression[Id]): FormalParameter[Id] =
    FormalParameter[Id](Identifier[Id](name), TypeIdentifier.name[Id](Identifier[Id](typ)), Some(default))

  def paramGroup(params: FormalParameter[Id]*): FormalParameterGroup[Id] =
    FormalParameterGroup[Id](params.toVector)

  implicit def arg(expression: Expression[Id]): Argument[Id] = Argument[Id](expression)

  implicit def typeId(name: String): TypeIdentifier[Id] = TypeIdentifier.name[Id](Identifier[Id](name))

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
