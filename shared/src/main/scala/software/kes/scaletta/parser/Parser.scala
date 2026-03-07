package software.kes.scaletta.parser

import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.{CharIndex, Pos}
import software.kes.scaletta.scanner.{Scanner, Token}

object Parser {
  def create(): Parser = new Parser()
}

case class ParseOptions(requireExhaustion: Boolean = true)

final class Parser private() {
  def parse(scanner: Scanner, options: ParseOptions = ParseOptions()): ParseResult[Pos] = {
    val result = parseExpression(scanner, BindingPower.Minimum)
    if (options.requireExhaustion) {
      val next = scanner.peek(1)
      next.value match {
        case Token.EndOfInput => result
        case _ => result.addError(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
      }
    } else {
      result
    }
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
    if (currentResult.hasErrors) {
      false
    } else {
      scanner.peek(1).value match {
        case Token.LParen =>
          // ( always has high precedence when acting as a postfix call
          // Use AllOthers to match what a normal high-priority operator would have
          BindingPower.AllOthers > minBindingPower
        case id: Token.Identifier =>
          val bp = Operators.bindingPower(id)
          if (bp > minBindingPower) {
            // If it's alphanumeric and we are at the top level,
            // we check if it's followed by another expression.
            // If it's not, then it's trailing garbage, not an infix operator.
            if (minBindingPower == BindingPower.Minimum && !id.isInstanceOf[Token.Identifier.Operator]) {
              // peek(2) to see if there is a RHS
              val afterId = scanner.peek(2)
              afterId.value match {
                case Token.EndOfInput | Token.RParen | Token.Comma | Token.Semicolon | Token.RBrace => false
                case _ => true
              }
            } else true
          } else false
        case _ => false
      }
    }
  }

  private def nud(token: Pos[Token], scanner: Scanner): ParseResult[Pos] = {
    token.value match {
      case Token.IntLiteral(v) => ParseResult.create(token.as(Literal.int(v)))
      case Token.StringLiteral(v) => ParseResult.create(token.as(Literal.string(v)))
      case Token.True => ParseResult.create(token.as(Literal.true_()))
      case Token.False => ParseResult.create(token.as(Literal.false_()))
      case Token.Null => ParseResult.create(token.as(Literal.null_()))
      case idToken: Token.Identifier =>
        val id = token.as(Identifier(idToken.name))
        ParseResult.create(token.as(Reference.single(id)))
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
      case Token.LParen =>
        val (argsOpt, end, errors, warnings) = parseArgumentGroup(opToken, scanner)
        argsOpt match {
          case (Some(args)) =>
            val group = Pos(args, opToken.begin, end)
            leftResult.value match {
              case Some(left) =>
                val call: Expression[Pos] = left.value match {
                  case sc: Call.Standard[Pos @unchecked] =>
                    sc.copy(args = sc.args :+ group)
                  case _ =>
                    Call.standard(left, Vector.empty, Vector(group))
                }
                ParseResult.create(Pos(
                  call,
                  left.begin,
                  end
                )).copy(errors = leftResult.errors ++ errors, warnings = leftResult.warnings ++ warnings)
              case None =>
                ParseResult(None, leftResult.errors ++ errors, leftResult.warnings ++ warnings)
            }
          case None =>
            ParseResult(None, leftResult.errors ++ errors, leftResult.warnings ++ warnings)
        }
      case id: Token.Identifier =>
        val bp = Operators.bindingPower(id)
        val rightResult = parseExpression(scanner, bp)

        (leftResult.value, rightResult.value) match {
          case (Some(left), Some(right)) =>
            val opId = Pos(Identifier(id.name), opToken.begin, opToken.end)
            ParseResult(
              value = Some(Pos(
                Call.infix(left, opId, Vector.empty, right),
                left.begin,
                right.end
              )),
              errors = leftResult.errors ++ rightResult.errors,
              warnings = leftResult.warnings ++ rightResult.warnings
            )
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

  private def parseArgumentGroup(lParenToken: Pos[Token], scanner: Scanner): (Option[ArgumentGroup[Pos]], CharIndex, Vector[Pos[ParseError]], Vector[Pos[ParseWarning]]) = {
    var args = Vector.empty[Pos[Argument[Pos]]]
    var errors = Vector.empty[Pos[ParseError]]
    var warnings = Vector.empty[Pos[ParseWarning]]

    def continueParsing(): Boolean = {
      val next = scanner.peek(1).value
      next != Token.RParen && next != Token.EndOfInput
    }

    while (continueParsing()) {
      val argExprResult = parseExpression(scanner, BindingPower.Minimum)
      errors ++= argExprResult.errors
      warnings ++= argExprResult.warnings

      argExprResult.value match {
        case Some(expr) =>
          args :+= expr.as(Argument(expr))
        case None => // Error already collected
      }

      val next = scanner.peek(1)
      next.value match {
        case Token.Comma =>
          scanner.get() // consume comma
        case Token.RParen => // will exit loop
        case _ if continueParsing() =>
          errors :+= Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end)
          // Synchronize to next comma or RParen
          var sync = scanner.peek(1)
          while (sync.value != Token.Comma && sync.value != Token.RParen && sync.value != Token.EndOfInput) {
            scanner.get()
            sync = scanner.peek(1)
          }
          if (sync.value == Token.Comma) scanner.get()
      }
    }

    val rParenToken = scanner.get()
    rParenToken.value match {
      case Token.RParen =>
        (Some(ArgumentGroup[Pos](args)), rParenToken.end, errors, warnings)
      case _ =>
        (None, rParenToken.end, errors :+ Pos(ParseError.UnexpectedToken(rParenToken.value), rParenToken.begin, rParenToken.end), warnings)
    }
  }
}
