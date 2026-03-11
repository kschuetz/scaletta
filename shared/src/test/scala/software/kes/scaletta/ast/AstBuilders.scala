package software.kes.scaletta.ast

import software.kes.scaletta.util.functional.Id._

object AstBuilders {
  def lit(n: Int): Expression[Id] = Literal.int(n)

  def lit(s: String): Expression[Id] = Literal.string(s)

  def lit(b: Boolean): Expression[Id] = Literal.boolean(b)

  def litNull: Expression[Id] = Literal.null_()

  def ref(name: String): Expression[Id] = Reference.single[Id](Identifier(name))

  def infix(left: Expression[Id], op: String, right: Expression[Id]): Expression[Id] =
    Call.infix[Id](left, Identifier(op), Vector.empty, right)

  def call(target: Expression[Id], args: Expression[Id]*): Expression[Id] = {
    val argGroup = ArgumentGroup[Id](args.toVector.map(a => Argument[Id](a)))
    Call.standard[Id](target, Vector.empty, Vector(argGroup))
  }

  def multiCall(target: Expression[Id], argGroups: Vector[Vector[Expression[Id]]]): Expression[Id] = {
    val groups = argGroups.map(group => ArgumentGroup[Id](group.map(a => Argument[Id](a))))
    Call.standard[Id](target, Vector.empty, groups)
  }

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

  def defDecl(name: String, params: Vector[Vector[(String, String)]], body: Expression[Id]): Declaration[Id] = {
    val paramGroups = params.map { group =>
      FormalParameterGroup[Id](group.map { case (n, t) =>
        FormalParameter[Id](Identifier[Id](n), TypeIdentifier.name[Id](Identifier[Id](t)), None)
      })
    }
    Declaration.def_[Id](Identifier(name), paramGroups, body)
  }

  def defSimple(name: String, body: Expression[Id]): Declaration[Id] =
    defDecl(name, Vector.empty, body)

  def defUnary(name: String, param: (String, String), body: Expression[Id]): Declaration[Id] =
    defDecl(name, Vector(Vector(param)), body)

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
}
