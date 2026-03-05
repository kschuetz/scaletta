package software.kes.scaletta.parser

import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.{Scanner, Token}

object Parser {
  def create(): Parser = new Parser()
}

final class Parser private() {
  def parse(scanner: Scanner): ParseResult[Pos] = {
    parseExpression(scanner, BindingPower.Minimum)
  }

  private def parseExpression(scanner: Scanner, minBindingPower: BindingPower): ParseResult[Pos] = {
    val firstToken = scanner.get()
    var currentResult = nud(firstToken, scanner)

    while (shouldContinue(scanner, minBindingPower, currentResult)) {
      val nextToken = scanner.get()
      currentResult = led(currentResult, nextToken, scanner)
    }

    currentResult
  }

  private def shouldContinue(scanner: Scanner, minBindingPower: BindingPower, currentResult: ParseResult[Pos]): Boolean = {
    if (currentResult.errors.nonEmpty) {
      false
    } else {
      val next = scanner.peek(1).value
      next match {
        case id: Token.Identifier =>
          val bp = Operators.bindingPower(id)
          bp > minBindingPower
        case _ => false
      }
    }
  }

  private def nud(token: Pos[Token], scanner: Scanner): ParseResult[Pos] = {
    token.value match {
      case Token.IntLiteral(v) => ParseResult.create(token.withNewValue(Literal.IntLiteral[Pos](v)))
      case Token.StringLiteral(v) => ParseResult.create(token.withNewValue(Literal.StringLiteral[Pos](v)))
      case Token.True => ParseResult.create(token.withNewValue(Literal.True[Pos]()))
      case Token.False => ParseResult.create(token.withNewValue(Literal.False[Pos]()))
      case Token.Null => ParseResult.create(token.withNewValue(Literal.Null[Pos]()))
      case Token.Identifier.Lower(name) =>
        ParseResult.create(token.withNewValue(Reference(::(Pos(Identifier(name), token.begin, token.end), Nil))))
      case Token.Identifier.Upper(name) =>
        ParseResult.create(token.withNewValue(Reference(::(Pos(Identifier(name), token.begin, token.end), Nil))))
      case Token.Identifier.Operator(name) =>
        ParseResult.create(token.withNewValue(Reference(::(Pos(Identifier(name), token.begin, token.end), Nil))))
      case Token.Identifier.Quoted(name) =>
        ParseResult.create(token.withNewValue(Reference(::(Pos(Identifier(name), token.begin, token.end), Nil))))
      case Token.LParen =>
        val result = parseExpression(scanner, BindingPower.Minimum)
        val next = scanner.get()
        next.value match {
          case Token.RParen =>
            result.value match {
              case Some(inner) =>
                ParseResult.create(Pos(inner.value, token.begin, next.end))
              case None => result
            }
          case _ =>
            result.addError(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
        }
      case _ =>
        ParseResult.error(Pos(ParseError.UnexpectedToken(token.value), token.begin, token.end))
    }
  }

  private def led(leftResult: ParseResult[Pos], opToken: Pos[Token], scanner: Scanner): ParseResult[Pos] = {
    opToken.value match {
      case id: Token.Identifier =>
        val bp = Operators.bindingPower(id)
        val rightResult = parseExpression(scanner, bp)

        (leftResult.value, rightResult.value) match {
          case (Some(left), Some(right)) =>
            val opId = Pos(Identifier(id.name), opToken.begin, opToken.end)
            ParseResult.create(Pos(
              Call.Infix(left, opId, Vector.empty, right),
              left.begin,
              right.end
            ))
          case _ =>
            ParseResult(
              value = None,
              errors = leftResult.errors ++ rightResult.errors,
              warnings = leftResult.warnings ++ rightResult.warnings
            )
        }
      case _ => leftResult
    }
  }
}
