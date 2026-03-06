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
      parseValue("123") shouldBe Literal.int(123)
    }

    it("should parse a string literal") {
      parseValue("\"hello\"") shouldBe Literal.string("hello")
    }

    it("should parse boolean literals") {
      parseValue("true") shouldBe Literal.true_()
      parseValue("false") shouldBe Literal.false_()
    }

    it("should parse null literal") {
      parseValue("null") shouldBe Literal.null_()
    }

    it("should parse an identifier") {
      expectReference(parseValue("foo"), "foo")
    }

    it("should parse infix addition") {
      val root = expectInfix(parseValue("1 + 2"))
      expectInt(root.left, 1)
      root.operation shouldBe Identifier("+")
      expectInt(root.right, 2)
    }

    it("should respect operator precedence (1 + 2 * 3)") {
      val root = expectInfix(parseValue("1 + 2 * 3"))
      expectInt(root.left, 1)
      root.operation shouldBe Identifier("+")

      val right = expectInfix(root.right)
      expectInt(right.left, 2)
      right.operation shouldBe Identifier("*")
      expectInt(right.right, 3)
    }

    it("should respect operator precedence (1 * 2 + 3)") {
      val root = expectInfix(parseValue("1 * 2 + 3"))
      val left = expectInfix(root.left)
      expectInt(left.left, 1)
      left.operation shouldBe Identifier("*")
      expectInt(left.right, 2)

      root.operation shouldBe Identifier("+")
      expectInt(root.right, 3)
    }

    it("should handle parentheses for grouping") {
      val root = expectInfix(parseValue("(1 + 2) * 3"))
      val left = expectInfix(root.left)
      expectInt(left.left, 1)
      left.operation shouldBe Identifier("+")
      expectInt(left.right, 2)

      root.operation shouldBe Identifier("*")
      expectInt(root.right, 3)
    }

    it("should parse a standard function call (f(x, y))") {
      val root = expectStandardCall(parseValue("f(x, y)"))
      expectReference(root.target, "f")
      root.typeArgs shouldBe empty
      root.args.size shouldBe 1
      val group = root.args.head
      group.arguments.size shouldBe 2

      expectReference(group.arguments(0).value, "x")
      expectReference(group.arguments(1).value, "y")
    }

    it("should parse a function call with no arguments (f())") {
      val root = expectStandardCall(parseValue("f()"))
      root.args.size shouldBe 1
      root.args.head.arguments shouldBe empty
    }

    it("should parse a nested function call (f(g(x)))") {
      val root = expectStandardCall(parseValue("f(g(x))"))
      expectReference(root.target, "f")
      val gCallExpr = root.args.head.arguments.head.value
      val gCall = expectStandardCall(gCallExpr)
      expectReference(gCall.target, "g")
      val xExpr = gCall.args.head.arguments.head.value
      expectReference(xExpr, "x")
    }

    it("should parse a call on an expression target ((f + g)(x))") {
      val root = expectStandardCall(parseValue("(f + g)(x)"))
      val target = expectInfix(root.target)
      expectReference(target.left, "f")
      target.operation shouldBe Identifier("+")
      expectReference(target.right, "g")

      root.args.size shouldBe 1
      val xExpr = root.args.head.arguments.head.value
      expectReference(xExpr, "x")
    }

    it("should parse multiple argument groups (f(x)(y))") {
      val root = expectStandardCall(parseValue("f(x)(y)"))
      root.args.size shouldBe 2
      expectReference(root.target, "f")
      expectReference(root.args(0).arguments(0).value, "x")
      expectReference(root.args(1).arguments(0).value, "y")
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

  private def expectInt(expr: Expression[Id], expected: Int): Literal.IntLiteral[Id] = {
    expr shouldBe Literal.int(expected)
    expr.asInstanceOf[Literal.IntLiteral[Id]]
  }

  private def expectReference(expr: Expression[Id], expected: String): Reference[Id] = {
    expr match {
      case r@Reference(::(id, Nil)) =>
        id shouldBe Identifier(expected)
        r
      case other => fail(s"Expected Identifier '$expected', but got $other")
    }
  }

  private def expectInfix(expr: Expression[Id]): Call.Infix[Id] = {
    expr match {
      case c: Call.Infix[Id] @unchecked => c
      case other => fail(s"Expected Infix call, but got $other")
    }
  }

  private def expectStandardCall(expr: Expression[Id]): Call.Standard[Id] = {
    expr match {
      case s: Call.Standard[Id] @unchecked => s
      case other => fail(s"Expected Standard call, got $other")
    }
  }

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
