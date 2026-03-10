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
    new Session(scanner, options).run
  }

  private class Session(scanner: Scanner, options: ParseOptions) {
    private case class Result[+A](value: Option[A],
                                  errors: Vector[Pos[ParseError]] = Vector.empty,
                                  warnings: Vector[Pos[ParseWarning]] = Vector.empty)

    private object Result {
      def error[A](error: Pos[ParseError]): Result[A] = Result(None, Vector(error))
    }

    def run: ParseResult[Pos] = {
      val result = parseExpression(BindingPower.Minimum)
      if (options.requireExhaustion) {
        val next = scanner.peek(1)
        next.value match {
          case Token.EndOfInput => result
          case _ => result.addError(Pos(ParseError.ExtraToken(next.value, "end of input"), next.begin, next.end))
        }
      } else {
        result
      }
    }

    private def parseExpression(minBindingPower: BindingPower): ParseResult[Pos] = {
      val firstToken = scanner.get()
      var currentResult = nud(firstToken)

      while (shouldContinue(minBindingPower, currentResult)) {
        val nextToken = scanner.get()
        currentResult = led(currentResult, nextToken)
      }

      currentResult
    }

    private def isFollowedByExpression: Boolean = {
      val afterId = scanner.peek(2).value
      afterId match {
        case Token.EndOfInput | Token.RParen | Token.Comma | Token.Semicolon | Token.RBrace => false
        case _ => true
      }
    }

    private def shouldContinue(minBindingPower: BindingPower, currentResult: ParseResult[Pos]): Boolean = {
      if (currentResult.hasErrors) {
        false
      } else {
        val nextToken = scanner.peek(1)
        if (isStructuralBoundary(nextToken.value) && minBindingPower == BindingPower.Minimum) {
          false
        } else {
          nextToken.value match {
            case Token.LParen =>
              // ( always has high precedence when acting as a postfix call
              BindingPower.PostfixCall > minBindingPower
            case idToken: Token.Identifier =>
              val bp = Operators.bindingPower(idToken)
              if (bp > minBindingPower) {
                // If it's alphanumeric and we are at the top level,
                // we check if it's followed by another expression.
                // If it's not, then it's trailing garbage, not an infix operator.
                if (minBindingPower == BindingPower.Minimum && !idToken.isInstanceOf[Token.Identifier.Operator]) {
                  isFollowedByExpression
                } else true
              } else false
            case rw: Token.ReservedWord =>
              val bp = Operators.bindingPower(rw)
              bp > minBindingPower
            case _ => false
          }
        }
      }
    }

    private def nud(token: Pos[Token]): ParseResult[Pos] = {
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
          parseParenthesizedExpression(token)
        case Token.LBrace =>
          parseBlock(token)
        case _ =>
          ParseResult.error(Pos(ParseError.UnexpectedToken(token.value), token.begin, token.end))
      }
    }

    private def parseParenthesizedExpression(token: Pos[Token]): ParseResult[Pos] = {
      val result = parseExpression(BindingPower.Minimum)
      val next = scanner.get()
      next.value match {
        case Token.RParen =>
          processParenthesizedResult(token, result, next)
        case Token.EndOfInput =>
          result.addError(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), token.begin, token.end))
        case _ =>
          result.addError(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
      }
    }

    private def processParenthesizedResult(token: Pos[Token], result: ParseResult[Pos], next: Pos[Token]): ParseResult[Pos] = {
      result.value match {
        case Some(inner) =>
          val finalResult = ParseResult.create(Pos(inner.value, token.begin, next.end))
            .copy(errors = result.errors, warnings = result.warnings, hints = result.hints)
          val isUnnecessary = inner.value.getClass match {
            case c if classOf[Literal[Pos]].isAssignableFrom(c) => true
            case c if classOf[Reference[Pos]].isAssignableFrom(c) => true
            case _ => false
          }
          if (isUnnecessary) {
            finalResult.addHint(Pos(ParseHint.UnnecessaryParentheses, token.begin, next.end))
          } else {
            finalResult
          }
        case None => result
      }
    }

    private def parseBlock(token: Pos[Token]): ParseResult[Pos] = {
      var declarations = Vector.empty[Pos[Declaration[Pos]]]
      var errors = Vector.empty[Pos[ParseError]]
      var warnings = Vector.empty[Pos[ParseWarning]]

      def isAtDeclarationStart: Boolean = {
        scanner.peek(1).value match {
          case Token.Val | Token.Lazy | Token.Def => true
          case _ => false
        }
      }

      while (isAtDeclarationStart) {
        val declResult = parseDeclaration()
        declResult.value.foreach(d => declarations = declarations :+ d)
        errors ++= declResult.errors
        warnings ++= declResult.warnings

        val next = scanner.peek(1)
        next.value match {
          case Token.Semicolon =>
            scanner.get()
          case Token.Newline =>
            scanner.get()
          case Token.RBrace =>
          // will be handled by final expression parsing
          case _ if isAtDeclarationStart || !isStructuralBoundary(next.value) =>
            errors :+= Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end)
          case _ =>
        }
      }

      val resultExpr = parseExpression(BindingPower.Minimum)
      errors ++= resultExpr.errors
      warnings ++= resultExpr.warnings

      val next = scanner.get()
      next.value match {
        case Token.RBrace =>
          resultExpr.value match {
            case Some(res) =>
              ParseResult[Pos](Some(Pos(Block(declarations, res), token.begin, next.end)), errors, warnings)
            case None =>
              ParseResult[Pos](None, errors :+ Pos(ParseError.MissingExpression("block result"), next.begin, next.end), warnings)
          }
        case Token.EndOfInput =>
          ParseResult[Pos](None, errors :+ Pos(ParseError.UnclosedDelimiter(Token.LBrace, Token.RBrace), token.begin, token.end), warnings)
        case _ =>
          ParseResult[Pos](None, errors :+ Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end), warnings)
      }
    }

    private def parseDeclaration(): Result[Pos[Declaration[Pos]]] = {
      val firstToken = scanner.get()
      firstToken.value match {
        case Token.Val =>
          parseValDeclaration(firstToken)
        case Token.Lazy =>
          val next = scanner.get()
          next.value match {
            case Token.Val =>
              parseLazyValDeclaration(firstToken, next)
            case _ =>
              Result.error(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
          }
        case Token.Def =>
          parseDefDeclaration(firstToken)
        case _ =>
          Result.error(Pos(ParseError.UnexpectedToken(firstToken.value), firstToken.begin, firstToken.end))
      }
    }

    private def parseValDeclaration(valToken: Pos[Token]): Result[Pos[Declaration[Pos]]] = {
      val patternResult = parsePattern()
      val eqToken = scanner.get()
      if (eqToken.value == Token.Eq) {
        val rhsResult = parseExpression(BindingPower.Minimum)
        (patternResult.value, rhsResult.value) match {
          case (Some(pat), Some(rhs)) =>
            Result(Some(Pos(Declaration.val_(pat, rhs), valToken.begin, rhs.end)), patternResult.errors ++ rhsResult.errors, patternResult.warnings ++ rhsResult.warnings)
          case _ =>
            Result(None, patternResult.errors ++ rhsResult.errors :+ Pos(ParseError.MissingExpression("val rhs"), eqToken.begin, eqToken.end), patternResult.warnings ++ rhsResult.warnings)
        }
      } else {
        Result(None, patternResult.errors :+ Pos(ParseError.UnexpectedToken(eqToken.value), eqToken.begin, eqToken.end), patternResult.warnings)
      }
    }

    private def parseLazyValDeclaration(lazyToken: Pos[Token], valToken: Pos[Token]): Result[Pos[Declaration[Pos]]] = {
      val patternResult = parsePattern()
      val eqToken = scanner.get()
      if (eqToken.value == Token.Eq) {
        val rhsResult = parseExpression(BindingPower.Minimum)
        (patternResult.value, rhsResult.value) match {
          case (Some(pat), Some(rhs)) =>
            Result(Some(Pos(Declaration.lazyVal(pat, rhs), lazyToken.begin, rhs.end)), patternResult.errors ++ rhsResult.errors, patternResult.warnings ++ rhsResult.warnings)
          case _ =>
            Result(None, patternResult.errors ++ rhsResult.errors :+ Pos(ParseError.MissingExpression("lazy val rhs"), eqToken.begin, eqToken.end), patternResult.warnings ++ rhsResult.warnings)
        }
      } else {
        Result(None, patternResult.errors :+ Pos(ParseError.UnexpectedToken(eqToken.value), eqToken.begin, eqToken.end), patternResult.warnings)
      }
    }

    private def parseDefDeclaration(defToken: Pos[Token]): Result[Pos[Declaration[Pos]]] = {
      val nameToken = scanner.get()
      nameToken.value match {
        case idToken: Token.Identifier =>
          val name = nameToken.as(Identifier(idToken.name))
          // For now, support simple 'def name = expr' without params
          val eqToken = scanner.get()
          if (eqToken.value == Token.Eq) {
            val rhsResult = parseExpression(BindingPower.Minimum)
            rhsResult.value match {
              case Some(rhs) =>
                Result(Some(Pos(Declaration.def_(name, Vector.empty, rhs), defToken.begin, rhs.end)), rhsResult.errors, rhsResult.warnings)
              case None =>
                Result(None, rhsResult.errors :+ Pos(ParseError.MissingExpression("def body"), eqToken.begin, eqToken.end), rhsResult.warnings)
            }
          } else {
            Result.error(Pos(ParseError.UnexpectedToken(eqToken.value), eqToken.begin, eqToken.end))
          }
        case _ =>
          Result.error(Pos(ParseError.UnexpectedToken(nameToken.value), nameToken.begin, nameToken.end))
      }
    }

    private def parsePattern(): Result[Pos[Pattern[Pos]]] = {
      val token = scanner.get()
      token.value match {
        case idToken: Token.Identifier =>
          val id = token.as(Identifier(idToken.name))
          Result(Some(token.as(Pattern.Identifier(id))))
        case Token.Underscore =>
          Result(Some(token.as(Pattern.Wildcard())))
        case _ =>
          Result.error(Pos(ParseError.UnexpectedToken(token.value), token.begin, token.end))
      }
    }

    private def led(leftResult: ParseResult[Pos], opToken: Pos[Token]): ParseResult[Pos] = {
      opToken.value match {
        case Token.LParen =>
          parseFunctionCall(leftResult, opToken)
        case id: Token.Identifier =>
          parseInfixExpression(leftResult, opToken, id.name)
        case rw: Token.ReservedWord =>
          parseInfixExpression(leftResult, opToken, rw.name)
        case _ => leftResult
      }
    }

    private def parseFunctionCall(leftResult: ParseResult[Pos], opToken: Pos[Token]): ParseResult[Pos] = {
      val (argsOpt, end, errors, warnings) = parseArgumentGroup(opToken)
      argsOpt match {
        case Some(args) =>
          val group = Pos(args, opToken.begin, end)
          leftResult.value match {
            case Some(left) =>
              val call: Expression[Pos] = if (left.value.getClass == classOf[Call.Standard[Pos]]) {
                val sc = left.value.asInstanceOf[Call.Standard[Pos]]
                sc.copy(args = sc.args :+ group)
              } else {
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
    }

    private def parseInfixExpression(leftResult: ParseResult[Pos], opToken: Pos[Token], opName: String): ParseResult[Pos] = {
      val bp = opToken.value match {
        case id: Token.Identifier => Operators.bindingPower(id)
        case rw: Token.ReservedWord => Operators.bindingPower(rw)
        case _ => BindingPower.Minimum
      }
      val isRightAssoc = opName.endsWith(":")
      val rightResult = if (isRightAssoc) parseExpression(bp.nudge(-1))
      else parseExpression(bp)

      val isSuspicious = opToken.value.isInstanceOf[Token.Identifier] &&
        !opToken.value.isInstanceOf[Token.Identifier.Operator] &&
        !isRightAssoc

      (leftResult.value, rightResult.value) match {
        case (Some(left), Some(right)) =>
          val opId = Pos(Identifier(opName), opToken.begin, opToken.end)
          var res = ParseResult[Pos](
            value = Some(Pos(
              Call.infix(left, opId, Vector.empty, right),
              left.begin,
              right.end
            )),
            errors = leftResult.errors ++ rightResult.errors,
            warnings = leftResult.warnings ++ rightResult.warnings
          )
          if (isSuspicious) res = res.addWarning(Pos(ParseWarning.SuspiciousInfixExpression(opName), opToken.begin, opToken.end))
          res
        case _ =>
          ParseResult[Pos](
            value = None,
            errors = leftResult.errors ++ rightResult.errors,
            warnings = leftResult.warnings ++ rightResult.warnings
          )
      }
    }

    private def isSynchronizationBoundary(token: Token): Boolean = {
      token match {
        case Token.Comma | Token.RParen | Token.EndOfInput | Token.Val | Token.Def |
             Token.If | Token.Case | Token.Semicolon | Token.RBrace => true
        case _ => false
      }
    }

    private def isStructuralBoundary(token: Token): Boolean = {
      token match {
        case Token.Val | Token.Def | Token.If | Token.Case | Token.Semicolon | Token.RBrace | Token.EndOfInput => true
        case _ => false
      }
    }

    private def parseArgumentGroup(lParenToken: Pos[Token]): (Option[ArgumentGroup[Pos]], CharIndex, Vector[Pos[ParseError]], Vector[Pos[ParseWarning]]) = {
      var args = Vector.empty[Pos[Argument[Pos]]]
      var errors = Vector.empty[Pos[ParseError]]
      var warnings = Vector.empty[Pos[ParseWarning]]

      def continueParsing(): Boolean = {
        val next = scanner.peek(1).value
        next != Token.RParen && next != Token.EndOfInput
      }

      var shouldStop = false
      while (!shouldStop && continueParsing()) {
        val nextToken = scanner.peek(1)
        if (nextToken.value == Token.Comma) {
          errors :+= Pos(ParseError.MissingExpression("argument"), nextToken.begin, nextToken.end)
          scanner.get() // consume comma
        } else {
          val argExprResult = parseExpression(BindingPower.Minimum)
          val (argOpt, argErrors, argWarnings) = parseSingleArgument(argExprResult)
          argOpt.foreach(args :+= _)
          errors ++= argErrors
          warnings ++= argWarnings

          val next = scanner.peek(1)
          next.value match {
            case Token.Comma =>
              scanner.get() // consume comma
            case Token.RParen => // will exit loop
            case _ if continueParsing() =>
              val (stop, syncErrors) = synchronizeToNextArgument()
              shouldStop = stop
              errors ++= syncErrors
            case _ =>
          }
        }
      }

      val rParenToken = scanner.peek(1)
      if (rParenToken.value == Token.RParen) {
        scanner.get()
        (Some(ArgumentGroup[Pos](args)), rParenToken.end, errors, warnings)
      } else {
        (Some(ArgumentGroup[Pos](args)), lParenToken.end, errors :+ Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), lParenToken.begin, lParenToken.end), warnings)
      }
    }

    private def parseSingleArgument(argExprResult: ParseResult[Pos]): (Option[Pos[Argument[Pos]]], Vector[Pos[ParseError]], Vector[Pos[ParseWarning]]) = {
      argExprResult.value match {
        case Some(expr) =>
          (Some(expr.as(Argument(expr))), argExprResult.errors, argExprResult.warnings)
        case None =>
          if (argExprResult.errors.isEmpty) {
            val next = scanner.peek(1)
            (None, Vector(Pos(ParseError.MissingExpression("argument"), next.begin, next.end)), argExprResult.warnings)
          } else {
            (None, argExprResult.errors, argExprResult.warnings)
          }
      }
    }

    private def synchronizeToNextArgument(): (Boolean, Vector[Pos[ParseError]]) = {
      var errors = Vector.empty[Pos[ParseError]]
      val next = scanner.peek(1)
      if (!isStructuralBoundary(next.value)) {
        errors :+= Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end)
      }
      // Synchronize to next comma or RParen
      var sync = scanner.peek(1)
      while (!isSynchronizationBoundary(sync.value)) {
        scanner.get()
        sync = scanner.peek(1)
      }
      if (sync.value == Token.Comma) {
        scanner.get()
        (false, errors)
      } else if (isStructuralBoundary(sync.value) && sync.value != Token.RParen) {
        (true, errors)
      } else {
        (false, errors)
      }
    }
  }
}
