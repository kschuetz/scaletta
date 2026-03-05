package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.scanner.{CharReader, IdentifierPolicy, Scanner}

class ParserTest extends AnyFunSpec with Matchers {

  describe("Parser") {
    it("should parse an integer literal") {
      parseValue("123") shouldBe Literal.IntLiteral[Pos](123)
    }

    it("should parse a string literal") {
      parseValue("\"hello\"") shouldBe Literal.StringLiteral[Pos]("hello")
    }

    it("should parse boolean literals") {
      parseValue("true") shouldBe Literal.True[Pos]()
      parseValue("false") shouldBe Literal.False[Pos]()
    }

    it("should parse null literal") {
      parseValue("null") shouldBe Literal.Null[Pos]()
    }

    it("should parse an identifier") {
      parseValue("foo") match {
        case Reference(::(id, Nil)) => id.value shouldBe Identifier("foo")
        case other => fail(s"Expected Reference, got $other")
      }
    }

    it("should parse infix addition") {
      parseValue("1 + 2") match {
        case Call.Infix(left, op, _, right) =>
          left.value shouldBe Literal.IntLiteral[Pos](1)
          op.value shouldBe Identifier("+")
          right.value shouldBe Literal.IntLiteral[Pos](2)
        case other => fail(s"Expected Infix call, got $other")
      }
    }

    it("should respect operator precedence (1 + 2 * 3)") {
      parseValue("1 + 2 * 3") match {
        case Call.Infix(left, op, _, right) =>
          left.value shouldBe Literal.IntLiteral[Pos](1)
          op.value shouldBe Identifier("+")
          right.value match {
            case Call.Infix(rLeft, rOp, _, rRight) =>
              rLeft.value shouldBe Literal.IntLiteral[Pos](2)
              rOp.value shouldBe Identifier("*")
              rRight.value shouldBe Literal.IntLiteral[Pos](3)
            case other => fail(s"Expected nested Infix call, got $other")
          }
        case other => fail(s"Expected Infix call, got $other")
      }
    }

    it("should respect operator precedence (1 * 2 + 3)") {
      parseValue("1 * 2 + 3") match {
        case Call.Infix(left, op, _, right) =>
          left.value match {
            case Call.Infix(lLeft, lOp, _, lRight) =>
              lLeft.value shouldBe Literal.IntLiteral[Pos](1)
              lOp.value shouldBe Identifier("*")
              lRight.value shouldBe Literal.IntLiteral[Pos](2)
            case other => fail(s"Expected nested Infix call, got $other")
          }
          op.value shouldBe Identifier("+")
          right.value shouldBe Literal.IntLiteral[Pos](3)
        case other => fail(s"Expected Infix call, got $other")
      }
    }

    it("should handle parentheses for grouping") {
      parseValue("(1 + 2) * 3") match {
        case Call.Infix(left, op, _, right) =>
          left.value match {
            case Call.Infix(lLeft, lOp, _, lRight) =>
              lLeft.value shouldBe Literal.IntLiteral[Pos](1)
              lOp.value shouldBe Identifier("+")
              lRight.value shouldBe Literal.IntLiteral[Pos](2)
            case other => fail(s"Expected nested Infix call, got $other")
          }
          op.value shouldBe Identifier("*")
          right.value shouldBe Literal.IntLiteral[Pos](3)
        case other => fail(s"Expected Infix call, got $other")
      }
    }

    it("should parse a standard function call (f(x, y))") {
      parseValue("f(x, y)") match {
        case Call.Standard(target, typeArgs, args) =>
          target.value match {
            case Reference(::(id, Nil)) => id.value shouldBe Identifier("f")
            case other => fail(s"Expected Reference target, got $other")
          }
          typeArgs shouldBe empty
          args.size shouldBe 1
          val group = args.head.value
          group.arguments.size shouldBe 2

          def checkRef(obj: Any, expected: String): Unit = obj match {
            case r: Reference[_] => r.path.head.asInstanceOf[Pos[_]].value shouldBe Identifier(expected)
            case p: Pos[_] => checkRef(p.value, expected)
            case other => fail(s"Expected Reference, got $other")
          }

          checkRef(group.arguments(0).value.value.value, "x")
          checkRef(group.arguments(1).value.value.value, "y")
        case other => fail(s"Expected Standard call, got $other")
      }
    }

    it("should parse a function call with no arguments (f())") {
      parseValue("f()") match {
        case Call.Standard(_, _, args) =>
          args.size shouldBe 1
          args.head.value.arguments shouldBe empty
        case other => fail(s"Expected Standard call, got $other")
      }
    }

    it("should parse a nested function call (f(g(x)))") {
      parseValue("f(g(x))") match {
        case Call.Standard(_, _, args) =>
          val gCallResult = args.head.value.arguments.head.value
          val gCall = gCallResult.value
          gCall.toString should include("Standard")
        case other => fail(s"Expected nested Standard call, got $other")
      }
    }

    it("should parse a call on an expression target ((f + g)(x))") {
      parseValue("(f + g)(x)") match {
        case Call.Standard(target, _, args) =>
          target.value match {
            case Call.Infix(_, _, _, _) => // OK
            case other => fail(s"Expected Infix target, got $other")
          }
          args.size shouldBe 1
        case other => fail(s"Expected Standard call, got $other")
      }
    }

    ignore("should parse multiple argument groups (f(x)(y))") {
      parseValue("f(x)(y)") match {
        case Call.Standard(target, _, args) =>
          target.value.toString should include("Standard")
          args.size shouldBe 1
          args.head.value.arguments.head.value.toString should include("Reference")
        case other => fail(s"Expected Standard call, got $other")
      }
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

  private def parse(input: String): ParseResult[Pos] = {
    val reader = CharReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    parser.parse(scanner)
  }
}
