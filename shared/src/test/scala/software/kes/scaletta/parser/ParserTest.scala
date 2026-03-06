package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.scanner.{CharReader, IdentifierPolicy, Scanner}

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
      expectInt(root.left.value, 1)
      root.operation.value shouldBe Identifier("+")
      expectInt(root.right.value, 2)
    }

    it("should respect operator precedence (1 + 2 * 3)") {
      val root = expectInfix(parseValue("1 + 2 * 3"))
      expectInt(root.left.value, 1)
      root.operation.value shouldBe Identifier("+")

      val right = expectInfix(root.right.value)
      expectInt(right.left.value, 2)
      right.operation.value shouldBe Identifier("*")
      expectInt(right.right.value, 3)
    }

    it("should respect operator precedence (1 * 2 + 3)") {
      val root = expectInfix(parseValue("1 * 2 + 3"))
      val left = expectInfix(root.left.value)
      expectInt(left.left.value, 1)
      left.operation.value shouldBe Identifier("*")
      expectInt(left.right.value, 2)

      root.operation.value shouldBe Identifier("+")
      expectInt(root.right.value, 3)
    }

    it("should handle parentheses for grouping") {
      val root = expectInfix(parseValue("(1 + 2) * 3"))
      val left = expectInfix(root.left.value)
      expectInt(left.left.value, 1)
      left.operation.value shouldBe Identifier("+")
      expectInt(left.right.value, 2)

      root.operation.value shouldBe Identifier("*")
      expectInt(root.right.value, 3)
    }

    it("should parse a standard function call (f(x, y))") {
      val root = expectStandardCall(parseValue("f(x, y)"))
      expectReference(root.target.value, "f")
      root.typeArgs shouldBe empty
      root.args.size shouldBe 1
      val group = root.args.head.value
      group.arguments.size shouldBe 2

      expectReference(group.arguments(0).value.value.value, "x")
      expectReference(group.arguments(1).value.value.value, "y")
    }

    it("should parse a function call with no arguments (f())") {
      val root = expectStandardCall(parseValue("f()"))
      root.args.size shouldBe 1
      root.args.head.value.arguments shouldBe empty
    }

    it("should parse a nested function call (f(g(x)))") {
      val root = expectStandardCall(parseValue("f(g(x))"))
      expectReference(root.target.value, "f")
      val gCallExpr = root.args.head.value.arguments.head.value.value
      val gCall = expectStandardCall(gCallExpr.value)
      expectReference(gCall.target.value, "g")
      val xExpr = gCall.args.head.value.arguments.head.value.value
      expectReference(xExpr.value, "x")
    }

    it("should parse a call on an expression target ((f + g)(x))") {
      val root = expectStandardCall(parseValue("(f + g)(x)"))
      val target = expectInfix(root.target.value)
      expectReference(target.left.value, "f")
      target.operation.value shouldBe Identifier("+")
      expectReference(target.right.value, "g")

      root.args.size shouldBe 1
      val xExpr = root.args.head.value.arguments.head.value.value
      expectReference(xExpr.value, "x")
    }

    it("should parse multiple argument groups (f(x)(y))") {
      val root = expectStandardCall(parseValue("f(x)(y)"))
      root.args.size shouldBe 2
      expectReference(root.target.value, "f")
      expectReference(root.args(0).value.arguments(0).value.value.value, "x")
      expectReference(root.args(1).value.arguments(0).value.value.value, "y")
    }
  }

  private def parseValue(input: String): Expression[Pos] = {
    val result = parse(input)
    result.errors shouldBe empty
    result.value match {
      case Some(pos) => pos.value
      case None => fail(s"Parser returned no value for input: $input")
    }
  }

  private def expectInt(expr: Expression[Pos], expected: Int): Literal.IntLiteral[Pos] = {
    expr shouldBe Literal.int(expected)
    expr.asInstanceOf[Literal.IntLiteral[Pos]]
  }

  private def expectReference(expr: Expression[Pos], expected: String): Reference[Pos] = {
    expr match {
      case r@Reference(::(id, Nil)) =>
        id.value shouldBe Identifier(expected)
        r
      case other => fail(s"Expected Identifier '$expected', but got $other")
    }
  }

  private def expectInfix(expr: Expression[Pos]): Call.Infix[Pos] = {
    expr match {
      case c: Call.Infix[Pos] @unchecked => c
      case other => fail(s"Expected Infix call, but got $other")
    }
  }

  private def expectStandardCall(expr: Expression[Pos]): Call.Standard[Pos] = {
    expr match {
      case s: Call.Standard[Pos] @unchecked => s
      case other => fail(s"Expected Standard call, got $other")
    }
  }

  private def parse(input: String): ParseResult[Pos] = {
    val reader = CharReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    parser.parse(scanner)
  }
}
