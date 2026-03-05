package software.kes.scaletta.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.{LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.scanner.{CharReader, IdentifierPolicy, Scanner}

class ParserTest extends AnyFunSpec with Matchers {

  describe("Parser") {
    it("should parse an integer literal") {
      val result = parse("123")
      result.value.map(_.value) shouldBe Some(Literal.IntLiteral[Pos](123))
      result.errors shouldBe empty
    }

    it("should parse a string literal") {
      val result = parse("\"hello\"")
      result.value.map(_.value) shouldBe Some(Literal.StringLiteral[Pos]("hello"))
      result.errors shouldBe empty
    }

    it("should parse boolean literals") {
      parse("true").value.map(_.value) shouldBe Some(Literal.True[Pos]())
      parse("false").value.map(_.value) shouldBe Some(Literal.False[Pos]())
    }

    it("should parse null literal") {
      parse("null").value.map(_.value) shouldBe Some(Literal.Null[Pos]())
    }

    it("should parse an identifier") {
      val result = parse("foo")
      result.value.map(_.value) match {
        case Some(Reference(::(id, Nil))) => id.value shouldBe Identifier("foo")
        case other => fail(s"Expected Reference, got $other")
      }
    }

    it("should parse infix addition") {
      val result = parse("1 + 2")
      result.value.map(_.value) match {
        case Some(Call.Infix(left, op, _, right)) =>
          left.value shouldBe Literal.IntLiteral[Pos](1)
          op.value shouldBe Identifier("+")
          right.value shouldBe Literal.IntLiteral[Pos](2)
        case other => fail(s"Expected Infix call, got $other")
      }
    }

    it("should respect operator precedence (1 + 2 * 3)") {
      val result = parse("1 + 2 * 3")
      result.value.map(_.value) match {
        case Some(Call.Infix(left, op, _, right)) =>
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
      val result = parse("1 * 2 + 3")
      result.value.map(_.value) match {
        case Some(Call.Infix(left, op, _, right)) =>
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
      val result = parse("(1 + 2) * 3")
      result.value.map(_.value) match {
        case Some(Call.Infix(left, op, _, right)) =>
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
  }

  private def parse(input: String): ParseResult[Pos] = {
    val reader = CharReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    val scanner = Scanner.create(reader, IdentifierPolicy.Default)
    val parser = Parser.create()
    parser.parse(scanner)
  }
}
