package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.scanner.{CharReader, IdentifierPolicy, Scanner}
import software.kes.scaletta.util.functional.Id._
import software.kes.scaletta.util.functional.~>

class ParserTest extends AnyFunSpec with Matchers {
  describe("Parser") {
    it("should parse an integer literal") {
      parseValue("123") shouldBe lit(123)
    }

    it("should parse a string literal") {
      parseValue("\"hello\"") shouldBe lit("hello")
    }

    it("should parse boolean literals") {
      parseValue("true") shouldBe lit(true)
      parseValue("false") shouldBe lit(false)
    }

    it("should parse null literal") {
      parseValue("null") shouldBe litNull
    }

    it("should parse an identifier") {
      parseValue("foo") shouldBe ref("foo")
    }

    it("should parse infix addition") {
      parseValue("1 + 2") shouldBe infix(lit(1), "+", lit(2))
    }

    it("should respect operator precedence (1 + 2 * 3)") {
      parseValue("1 + 2 * 3") shouldBe infix(lit(1), "+", infix(lit(2), "*", lit(3)))
    }

    it("should respect operator precedence (1 * 2 + 3)") {
      parseValue("1 * 2 + 3") shouldBe infix(infix(lit(1), "*", lit(2)), "+", lit(3))
    }

    it("should handle parentheses for grouping") {
      parseValue("(1 + 2) * 3") shouldBe infix(infix(lit(1), "+", lit(2)), "*", lit(3))
    }

    it("should parse a standard function call (f(x, y))") {
      parseValue("f(x, y)") shouldBe call(ref("f"), ref("x"), ref("y"))
    }

    it("should parse a function call with no arguments (f())") {
      parseValue("f()") shouldBe call(ref("f"))
    }

    it("should parse a nested function call (f(g(x)))") {
      parseValue("f(g(x))") shouldBe call(ref("f"), call(ref("g"), ref("x")))
    }

    it("should parse a call on an expression target ((f + g)(x))") {
      parseValue("(f + g)(x)") shouldBe call(infix(ref("f"), "+", ref("g")), ref("x"))
    }

    it("should parse multiple argument groups (f(x)(y))") {
      parseValue("f(x)(y)") shouldBe multiCall(ref("f"), Vector(Vector(ref("x")), Vector(ref("y"))))
    }
  }

  private def parseValue(input: String): Expression[Id] = {
    val result = parse(input)
    result.errors shouldBe empty
    result.value match {
      case Some(pos) => pos.value.mapK(posToId)
      case None => fail(s"Parser returned no value for input: $input")
    }
  }

  private def lit(n: Int): Expression[Id] = Literal.int(n)

  private def lit(s: String): Expression[Id] = Literal.string(s)

  private def lit(b: Boolean): Expression[Id] = Literal.boolean(b)

  private def litNull: Expression[Id] = Literal.null_()

  private def ref(name: String): Expression[Id] = Reference.single[Id](Identifier(name))

  private def infix(left: Expression[Id], op: String, right: Expression[Id]): Expression[Id] =
    Call.infix[Id](left, Identifier(op), Vector.empty, right)

  private def call(target: Expression[Id], args: Expression[Id]*): Expression[Id] = {
    val argGroup = ArgumentGroup[Id](args.toVector.map(a => Argument[Id](a)))
    Call.standard[Id](target, Vector.empty, Vector(argGroup))
  }

  private def multiCall(target: Expression[Id], argGroups: Vector[Vector[Expression[Id]]]): Expression[Id] = {
    val groups = argGroups.map(group => ArgumentGroup[Id](group.map(a => Argument[Id](a))))
    Call.standard[Id](target, Vector.empty, groups)
  }

  private def block(declarations: Vector[Declaration[Id]], result: Expression[Id]): Expression[Id] =
    Block[Id](declarations, result)

  private def tuple(elements: Expression[Id]*): Expression[Id] =
    Tuple[Id](elements.toVector)

  private def cond(condition: Expression[Id], thenBranch: Expression[Id], elseBranch: Expression[Id]): Expression[Id] =
    Conditional[Id](condition, thenBranch, elseBranch)

  private def valDecl(pattern: Pattern[Id], rhs: Expression[Id]): Declaration[Id] =
    Declaration.val_[Id](pattern, rhs)

  private def lazyValDecl(pattern: Pattern[Id], rhs: Expression[Id]): Declaration[Id] =
    Declaration.lazyVal[Id](pattern, rhs)

  private def defDecl(name: String, params: Vector[Vector[(String, String)]], body: Expression[Id]): Declaration[Id] = {
    val paramGroups = params.map { group =>
      FormalParameterGroup[Id](group.map { case (n, t) =>
        FormalParameter[Id](Identifier(n), TypeIdentifier.name(Identifier(t)), None)
      })
    }
    Declaration.def_[Id](Identifier(name), paramGroups, body)
  }

  private def pWild: Pattern[Id] = Pattern.Wildcard[Id]()

  private def pId(name: String): Pattern[Id] = Pattern.Identifier[Id](Identifier(name))

  private def pLit(expr: Literal[Id]): Pattern[Id] = Pattern.Literal[Id](expr)

  private def pAs(name: String, pattern: Pattern[Id]): Pattern[Id] = Pattern.As[Id](Identifier(name), pattern)

  private def pTyped(pattern: Pattern[Id], typeName: String): Pattern[Id] =
    Pattern.Typed[Id](pattern, TypeIdentifier.name(Identifier(typeName)))

  private def pTuple(elements: Pattern[Id]*): Pattern[Id] = Pattern.Tuple[Id](elements.toVector)

  private def pProduct(typeName: String, args: Pattern[Id]*): Pattern[Id] =
    Pattern.Product[Id](TypeIdentifier.name(Identifier(typeName)), args.toVector)

  private def parse(input: String): ParseResult[Pos] = {
    val reader = CharReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    parser.parse(scanner)
  }

  private object posToId extends (Pos ~> Id) {
    def apply[A](fa: Pos[A]): Id[A] = fa.value
  }

}
