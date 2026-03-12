package software.kes.scaletta.parser

import software.kes.scaletta.ast._
import software.kes.scaletta.reporting.{CharIndex, Pos}
import software.kes.scaletta.scanner.{Scanner, Token}

object Parser {
  def create(): Parser = new Parser()
}

case class ParseOptions(requireExhaustion: Boolean = true)

final class Parser private() {
  def parse(scanner: Scanner, options: ParseOptions = ParseOptions()): ExprResult[Pos] = {
    new Session(scanner, options).run
  }

  private class Session(scanner: Scanner, options: ParseOptions) {

    def run: ExprResult[Pos] = {
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

    private def parseExpression(minBindingPower: BindingPower): ExprResult[Pos] = {
      val firstToken = scanner.get()
      var currentResult = nud(firstToken)

      while (currentResult.hasValue && !currentResult.hasErrors && shouldContinueAtInfix(minBindingPower, scanner.peek(1).value)) {
        val nextToken = scanner.get()
        currentResult = led(currentResult, nextToken)
      }

      currentResult
    }

    private def isFollowedByExpression: Boolean = {
      val afterId = scanner.peek(2).value
      afterId match {
        case Token.EndOfInput | Token.RParen | Token.Comma | Token.Semicolon | Token.RBrace | Token.Newline => false
        case _ => true
      }
    }

    private def shouldContinueAtInfix(minBindingPower: BindingPower, token: Token): Boolean = {
      if (isStructuralBoundary(token)) {
        false
      } else {
        token match {
          case Token.LParen => shouldContinueWithCall(minBindingPower)
          case Token.Dot => shouldContinueWithSelection(minBindingPower)
          case id: Token.Identifier =>
            shouldContinueWithIdentifier(minBindingPower, id)
          case rw: Token.ReservedWord =>
            shouldContinueWithReservedWord(minBindingPower, rw)
          case _ => false
        }
      }
    }

    private def shouldContinueWithSelection(minBindingPower: BindingPower): Boolean = {
      BindingPower.MemberAccess > minBindingPower
    }

    private def shouldContinueWithCall(minBindingPower: BindingPower): Boolean = {
      // ( acts as a function call with very high precedence
      BindingPower.MemberAccess > minBindingPower
    }

    private def shouldContinueWithIdentifier(minBindingPower: BindingPower, idToken: Token.Identifier): Boolean = {
      val bp = Operators.bindingPower(idToken)
      if (bp > minBindingPower) {
        // Alphanumeric infix check: if we are at the top level and it's not a symbolic operator,
        // ensure it's actually followed by an expression to avoid consuming trailing garbage.
        if (minBindingPower == BindingPower.Minimum && !idToken.isInstanceOf[Token.Identifier.Operator]) {
          isFollowedByExpression
        } else {
          true
        }
      } else {
        false
      }
    }

    private def shouldContinueWithReservedWord(minBindingPower: BindingPower, rw: Token.ReservedWord): Boolean = {
      Operators.bindingPower(rw) > minBindingPower
    }

    private def nud(token: Pos[Token]): ExprResult[Pos] = {
      if (isStructuralBoundary(token.value)) {
        return ParseResult.error(Pos(ParseError.UnexpectedToken(token.value), token.begin, token.end))
      }
      token.value match {
        case Token.IntLiteral(v) => ParseResult.create(token.as(Literal.int(v)))
        case Token.StringLiteral(v) => ParseResult.create(token.as(Literal.string(v)))
        case Token.True => ParseResult.create(token.as(Literal.true_()))
        case Token.False => ParseResult.create(token.as(Literal.false_()))
        case Token.Null => ParseResult.create(token.as(Literal.null_()))
        case idToken: Token.Identifier =>
          val id = token.as(Identifier[Pos](idToken.name))
          ParseResult.create(token.as(Reference(id)))
        case Token.LParen =>
          parseParenthesizedExpression(token)
        case Token.LBrace =>
          parseBlock(token)
        case Token.If =>
          parseConditional(token)
        case _ =>
          ParseResult.error(Pos(ParseError.UnexpectedToken(token.value), token.begin, token.end))
      }
    }

    private def parseParenthesizedExpression(token: Pos[Token]): ExprResult[Pos] = {
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

    private def processParenthesizedResult(token: Pos[Token], result: ExprResult[Pos], next: Pos[Token]): ExprResult[Pos] = {
      result.value match {
        case Some(inner) =>
          val finalResult: ExprResult[Pos] = ParseResult.create(Pos(inner.value, token.begin, next.end))
            .addDiagnostics(result.diagnostics)
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

    private def parseBlock(token: Pos[Token]): ExprResult[Pos] = {
      var declarations = Vector.empty[Pos[Declaration[Pos]]]
      var diagnostics = ParseDiagnostics.empty

      def isAtDeclarationStart: Boolean = {
        scanner.peek(1).value match {
          case Token.Val | Token.Lazy | Token.Def => true
          case _ => false
        }
      }

      while (isAtDeclarationStart) {
        val declResult = parseDeclaration()
        declResult.value.foreach(d => declarations = declarations :+ d)
        diagnostics = diagnostics ++ declResult.diagnostics

        val next = scanner.peek(1)
        next.value match {
          case Token.Semicolon =>
            scanner.get()
          case Token.Newline =>
            scanner.get()
          case Token.RBrace =>
          // will be handled by final expression parsing
          case _ if isAtDeclarationStart || !isStructuralBoundary(next.value) =>
            diagnostics = diagnostics.addError(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
          case _ =>
        }
      }

      val resultExpr = parseExpression(BindingPower.Minimum)
      diagnostics = diagnostics ++ resultExpr.diagnostics

      val next = scanner.get()
      next.value match {
        case Token.RBrace =>
          resultExpr.value match {
            case Some(res) =>
              ParseResult(Some(Pos(Block(declarations, res), token.begin, next.end)), diagnostics)
            case None =>
              ParseResult(None, diagnostics.addError(Pos(ParseError.MissingExpression("block result"), next.begin, next.end)))
          }
        case Token.EndOfInput =>
          ParseResult(None, diagnostics.addError(Pos(ParseError.UnclosedDelimiter(Token.LBrace, Token.RBrace), token.begin, token.end)))
        case _ =>
          ParseResult(None, diagnostics.addError(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end)))
      }
    }

    private def parseDeclaration(): DeclResult[Pos] = {
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
              ParseResult.error(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
          }
        case Token.Def =>
          parseDefDeclaration(firstToken)
        case _ =>
          ParseResult.error(Pos(ParseError.UnexpectedToken(firstToken.value), firstToken.begin, firstToken.end))
      }
    }

    private def parseValDeclaration(valToken: Pos[Token]): DeclResult[Pos] = {
      val patternResult = parsePattern()
      val eqToken = scanner.get()
      if (eqToken.value == Token.Eq) {
        val rhsResult = parseExpression(BindingPower.Minimum)
        (patternResult.value, rhsResult.value) match {
          case (Some(pat), Some(rhs)) =>
            ParseResult(Some(Pos(Declaration.val_(pat, rhs), valToken.begin, rhs.end)), patternResult.diagnostics ++ rhsResult.diagnostics)
          case _ =>
            ParseResult(None, (patternResult.diagnostics ++ rhsResult.diagnostics).addError(Pos(ParseError.MissingExpression("val rhs"), eqToken.begin, eqToken.end)))
        }
      } else {
        ParseResult(None, patternResult.diagnostics.addError(Pos(ParseError.UnexpectedToken(eqToken.value), eqToken.begin, eqToken.end)))
      }
    }

    private def parseLazyValDeclaration(lazyToken: Pos[Token], valToken: Pos[Token]): DeclResult[Pos] = {
      val patternResult = parsePattern()
      val eqToken = scanner.get()
      if (eqToken.value == Token.Eq) {
        val rhsResult = parseExpression(BindingPower.Minimum)
        (patternResult.value, rhsResult.value) match {
          case (Some(pat), Some(rhs)) =>
            ParseResult(Some(Pos(Declaration.lazyVal(pat, rhs), lazyToken.begin, rhs.end)), patternResult.diagnostics ++ rhsResult.diagnostics)
          case _ =>
            ParseResult(None, (patternResult.diagnostics ++ rhsResult.diagnostics).addError(Pos(ParseError.MissingExpression("lazy val rhs"), eqToken.begin, eqToken.end)))
        }
      } else {
        ParseResult(None, patternResult.diagnostics.addError(Pos(ParseError.UnexpectedToken(eqToken.value), eqToken.begin, eqToken.end)))
      }
    }

    private def parseFormalParameterGroup(): ParseResult[Pos, Pos[FormalParameterGroup[Pos]]] = {
      val lParen = scanner.get()
      if (lParen.value != Token.LParen) {
        return ParseResult.error(Pos(ParseError.UnexpectedToken(lParen.value), lParen.begin, lParen.end))
      }

      var params = Vector.empty[Pos[FormalParameter[Pos]]]
      var variadic: Option[Pos[FormalParameter[Pos]]] = None
      var diagnostics = ParseDiagnostics.empty

      def continue(): Boolean = {
        val next = scanner.peek(1).value
        next != Token.RParen && next != Token.EndOfInput && !isStructuralBoundary(next)
      }

      while (continue() && variadic.isEmpty) {
        val nameToken = scanner.get()
        nameToken.value match {
          case idToken: Token.Identifier =>
            val name = nameToken.as(Identifier[Pos](idToken.name))
            val colon = scanner.get()
            if (colon.value == Token.Colon) {
              val typeResult = TypeIdentifierParser.parse(scanner)
              diagnostics ++= typeResult.diagnostics

              typeResult.value match {
                case Some(t) =>
                  // Check for variadic asterisk
                  val nextToken = scanner.peek(1)
                  var asteriskPos: Option[Pos[Token]] = None
                  val hasAsterisk = nextToken.value match {
                    case id: Token.Identifier if id.name == "*" =>
                      asteriskPos = Some(scanner.get())
                      true
                    case _ => false
                  }

                  // Parse default value
                  var default: Option[Pos[Expression[Pos]]] = None
                  if (scanner.peek(1).value == Token.Eq) {
                    val eqToken = scanner.get()
                    val defaultResult = parseExpression(BindingPower.Minimum)
                    diagnostics ++= defaultResult.diagnostics
                    defaultResult.value match {
                      case Some(expr) =>
                        default = Some(expr)
                      case None =>
                        diagnostics = diagnostics.addError(Pos(ParseError.MissingExpression("parameter default value"), eqToken.begin, eqToken.end))
                    }
                  }

                  val pEnd = default.map(_.end).getOrElse(asteriskPos.map(_.end).getOrElse(t.end))
                  val param = Pos(FormalParameter(name, t, default), nameToken.begin, pEnd)

                  if (hasAsterisk) {
                    if (default.isDefined) {
                      val errPos = asteriskPos.getOrElse(Pos(Token.EndOfInput, t.end, t.end))
                      diagnostics = diagnostics.addError(Pos(ParseError.Message("Variadic parameter cannot have a default value"), errPos.begin, errPos.end))
                    }
                    variadic = Some(param)
                  } else {
                    params :+= param
                  }
                case None =>
                // Error already in diagnostics from TypeIdentifierParser
              }
            } else {
              diagnostics = diagnostics.addError(Pos(ParseError.UnexpectedToken(colon.value), colon.begin, colon.end))
            }
          case _ =>
            diagnostics = diagnostics.addError(Pos(ParseError.UnexpectedToken(nameToken.value), nameToken.begin, nameToken.end))
        }

        val next = scanner.peek(1)
        if (next.value == Token.Comma) {
          scanner.get()
          if (variadic.isDefined) {
            diagnostics = diagnostics.addError(Pos(ParseError.VariadicParameterMustBeLast, next.begin, next.end))
          }
        } else if (next.value != Token.RParen && continue()) {
          // Basic synchronization
          while (continue() && scanner.peek(1).value != Token.Comma && scanner.peek(1).value != Token.RParen) {
            scanner.get()
          }
          if (scanner.peek(1).value == Token.Comma) scanner.get()
        }
      }

      val rParen = scanner.get()
      val groupEnd = if (rParen.value == Token.RParen) rParen.end else scanner.peek(1).end
      if (rParen.value != Token.RParen) {
        diagnostics = diagnostics.addError(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), lParen.begin, lParen.end))
      }

      ParseResult(Some(Pos(FormalParameterGroup(params, variadic), lParen.begin, groupEnd)), diagnostics)
    }

    private def parseDefDeclaration(defToken: Pos[Token]): DeclResult[Pos] = {
      val nameToken = scanner.get()
      nameToken.value match {
        case idToken: Token.Identifier =>
          val name = nameToken.as(Identifier[Pos](idToken.name))

          // Step 4: Parse multiple parameter groups (currying support)
          var paramGroups = Vector.empty[Pos[FormalParameterGroup[Pos]]]
          var allDiagnostics = ParseDiagnostics.empty

          while (scanner.peek(1).value == Token.LParen) {
            val groupResult = parseFormalParameterGroup()
            allDiagnostics ++= groupResult.diagnostics
            groupResult.value.foreach(paramGroups :+= _)
          }

          // Step 3: Optional Return Type
          val returnTypeResult: ParseResult[Pos, Option[Pos[TypeIdentifier[Pos]]]] = if (scanner.peek(1).value == Token.Colon) {
            scanner.get() // Consume ':'
            val tr = TypeIdentifierParser.parse(scanner)
            ParseResult[Pos, Option[Pos[TypeIdentifier[Pos]]]](tr.value.map(Some(_)), tr.diagnostics)
          } else {
            ParseResult[Pos, Option[Pos[TypeIdentifier[Pos]]]](Some(None))
          }
          allDiagnostics ++= returnTypeResult.diagnostics

          val eqToken = scanner.get()
          if (eqToken.value == Token.Eq) {
            val rhsResult = parseExpression(BindingPower.Minimum)
            allDiagnostics ++= rhsResult.diagnostics

            (returnTypeResult.value, rhsResult.value) match {
              case (Some(returnType), Some(rhs)) =>
                ParseResult(
                  Some(Pos(Declaration.def_(name, paramGroups, returnType, rhs), defToken.begin, rhs.end)),
                  allDiagnostics
                )
              case (None, Some(rhs)) =>
                ParseResult(
                  Some(Pos(Declaration.def_(name, paramGroups, None, rhs), defToken.begin, rhs.end)),
                  allDiagnostics
                )
              case (_, _) =>
                ParseResult(None, allDiagnostics.addError(Pos(ParseError.MissingExpression("def body"), eqToken.begin, eqToken.end)))
            }
          } else {
            ParseResult.error(Pos(ParseError.UnexpectedToken(eqToken.value), eqToken.begin, eqToken.end))
              .addDiagnostics(allDiagnostics)
          }
        case _ =>
          ParseResult.error(Pos(ParseError.UnexpectedToken(nameToken.value), nameToken.begin, nameToken.end))
      }
    }

    private def parsePattern(): PatResult[Pos] = {
      val firstToken = scanner.get()
      val baseResult: PatResult[Pos] = firstToken.value match {
        case idToken: Token.Identifier =>
          val id = firstToken.as(Identifier[Pos](idToken.name))
          ParseResult(Some(firstToken.as(Pattern.Identifier(id))))
        case Token.Underscore =>
          ParseResult(Some(firstToken.as(Pattern.Wildcard())))
        case _ =>
          ParseResult.error(Pos(ParseError.UnexpectedToken(firstToken.value), firstToken.begin, firstToken.end))
      }

      baseResult.value match {
        case Some(basePat) if scanner.peek(1).value == Token.Colon =>
          scanner.get() // consume ':'
          val typeResult = TypeIdentifierParser.parse(scanner)
          typeResult.value match {
            case Some(ascription) =>
              val typedPat: Pattern[Pos] = Pattern.Typed(basePat, ascription)
              ParseResult(Some(Pos(typedPat, basePat.begin, ascription.end)), baseResult.diagnostics ++ typeResult.diagnostics)
            case None =>
              baseResult.addDiagnostics(typeResult.diagnostics)
          }
        case _ =>
          baseResult
      }
    }

    private def led(leftResult: ExprResult[Pos], opToken: Pos[Token]): ExprResult[Pos] = {
      opToken.value match {
        case Token.LParen =>
          parseFunctionCall(leftResult, opToken)
        case Token.Dot =>
          parseSelection(leftResult, opToken)
        case Token.Colon =>
          parseTypeAscription(leftResult, opToken)
        case id: Token.Identifier =>
          parseInfixExpression(leftResult, opToken, id.name)
        case rw: Token.ReservedWord if rw != Token.Colon =>
          parseInfixExpression(leftResult, opToken, rw.name)
        case _ => leftResult
      }
    }

    private def parseSelection(leftResult: ExprResult[Pos], dotToken: Pos[Token]): ExprResult[Pos] = {
      val next = scanner.get()
      next.value match {
        case idToken: Token.Identifier =>
          val name = next.as(Identifier[Pos](idToken.name))
          leftResult.value match {
            case Some(qualifier) =>
              val selection: Expression[Pos] = Select[Pos](qualifier, name)
              ParseResult.create(Pos(selection, qualifier.begin, next.end))
                .addDiagnostics(leftResult.diagnostics)
            case None =>
              // This should ideally not happen in a correctly functioning Pratt parser
              // if leftResult.value is None, it should have diagnostics already.
              ParseResult(None, leftResult.diagnostics.addError(Pos(ParseError.MissingExpression("selection qualifier"), dotToken.begin, dotToken.end)))
          }
        case _ =>
          val result = leftResult.addError(Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end))
          // Synchronize to avoid cascading errors
          while (!isSynchronizationBoundary(scanner.peek(1).value)) {
            scanner.get()
          }
          result
      }
    }

    private def parseTypeAscription(leftResult: ExprResult[Pos], colonToken: Pos[Token]): ExprResult[Pos] = {
      val typeResult: TypeResult[Pos] = TypeIdentifierParser.parse(scanner)

      (leftResult.value, typeResult.value) match {
        case (Some(leftExpr), Some(ascription)) =>
          val typedNode: Expression[Pos] = Typed[Pos](leftExpr, ascription)
          ParseResult.create(Pos(
            typedNode,
            leftExpr.begin,
            ascription.end
          )).addDiagnostics(leftResult.diagnostics ++ typeResult.diagnostics)

        case _ =>
          val result: ExprResult[Pos] = ParseResult(leftResult.value, leftResult.diagnostics ++ typeResult.diagnostics)
          // Synchronize if we failed to parse a type
          while (!isSynchronizationBoundary(scanner.peek(1).value)) {
            scanner.get()
          }
          if (scanner.peek(1).value == Token.Semicolon) {
            scanner.get()
          }
          result
      }
    }

    private def parseFunctionCall(leftResult: ExprResult[Pos], opToken: Pos[Token]): ExprResult[Pos] = {
      val (argsOpt, end, diagnostics) = parseArgumentGroup(opToken)
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
              )).addDiagnostics(leftResult.diagnostics ++ diagnostics)
            case None =>
              ParseResult(None, leftResult.diagnostics ++ diagnostics)
          }
        case None =>
          ParseResult(None, leftResult.diagnostics ++ diagnostics)
      }
    }

    private def parseInfixExpression(leftResult: ExprResult[Pos], opToken: Pos[Token], opName: String): ExprResult[Pos] = {
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
          val opId = Pos(Identifier[Pos](opName), opToken.begin, opToken.end)
          var res: ExprResult[Pos] = ParseResult(
            value = Some(Pos(
              Call.infix(left, opId, Vector.empty, right),
              left.begin,
              right.end
            )),
            diagnostics = leftResult.diagnostics ++ rightResult.diagnostics
          )
          if (isSuspicious) res = res.addWarning(Pos(ParseWarning.SuspiciousInfixExpression(opName), opToken.begin, opToken.end))
          res
        case (Some(left), None) =>
          ParseResult(Some(left), leftResult.diagnostics ++ rightResult.diagnostics)
        case _ =>
          ParseResult(
            value = None,
            diagnostics = leftResult.diagnostics ++ rightResult.diagnostics
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
        case Token.Val | Token.Def | Token.Else | Token.Then | Token.Case | Token.Semicolon | Token.RBrace | Token.EndOfInput => true
        case _ => false
      }
    }

    private def parseConditional(ifToken: Pos[Token]): ExprResult[Pos] = {
      val next = scanner.peek(1)
      val conditionResult = if (next.value == Token.LParen) {
        scanner.get()
        val cond = parseExpression(BindingPower.Minimum)
        val closeParen = scanner.get()
        val finalCond = if (closeParen.value != Token.RParen) {
          cond.addError(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), next.begin, next.end))
        } else {
          cond
        }
        if (scanner.peek(1).value == Token.Then) {
          scanner.get()
        }
        finalCond
      } else {
        val cond = parseExpression(BindingPower.Minimum)
        if (scanner.peek(1).value == Token.Then) {
          scanner.get()
          cond
        } else {
          cond.addError(Pos(ParseError.UnexpectedToken(scanner.peek(1).value), scanner.peek(1).begin, scanner.peek(1).end))
        }
      }

      val thenResult = parseExpression(BindingPower.Minimum)
      val nextAfterThen = scanner.peek(1)
      val elseResult = if (nextAfterThen.value == Token.Else) {
        scanner.get()
        parseExpression(BindingPower.Minimum)
      } else {
        ParseResult.error[Pos, Pos[Expression[Pos]]](Pos(ParseError.UnexpectedToken(nextAfterThen.value), nextAfterThen.begin, nextAfterThen.end))
      }

      val allDiagnostics = conditionResult.diagnostics ++ thenResult.diagnostics ++ elseResult.diagnostics

      (conditionResult.value, thenResult.value, elseResult.value) match {
        case (Some(c), Some(t), Some(e)) =>
          ParseResult(Some(Pos(Conditional(c, t, e), ifToken.begin, e.end)), allDiagnostics)
        case _ =>
          ParseResult(None, allDiagnostics)
      }
    }

    private def parseArgumentGroup(lParenToken: Pos[Token]): (Option[ArgumentGroup[Pos]], CharIndex, ParseDiagnostics) = {
      var args = Vector.empty[Pos[Argument[Pos]]]
      var diagnostics = ParseDiagnostics.empty

      def continueParsing(): Boolean = {
        val next = scanner.peek(1).value
        next != Token.RParen && next != Token.EndOfInput
      }

      var shouldStop = false
      while (!shouldStop && continueParsing()) {
        val nextToken = scanner.peek(1)
        if (nextToken.value == Token.Comma) {
          diagnostics = diagnostics.addError(Pos(ParseError.MissingExpression("argument"), nextToken.begin, nextToken.end))
          scanner.get() // consume comma
        } else {
          val argExprResult = parseExpression(BindingPower.Minimum)
          val (argOpt, argDiagnostics) = parseSingleArgument(argExprResult)
          argOpt.foreach(args :+= _)
          diagnostics = diagnostics ++ argDiagnostics

          val next = scanner.peek(1)
          next.value match {
            case Token.Comma =>
              scanner.get() // consume comma
            case Token.RParen => // will exit loop
            case _ if continueParsing() =>
              val (stop, syncErrors) = synchronizeToNextArgument()
              shouldStop = stop
              diagnostics = syncErrors.foldLeft(diagnostics)(_ addError _)
            case _ =>
          }
        }
      }

      val rParenToken = scanner.peek(1)
      if (rParenToken.value == Token.RParen) {
        scanner.get()
        (Some(ArgumentGroup[Pos](args)), rParenToken.end, diagnostics)
      } else {
        (Some(ArgumentGroup[Pos](args)), lParenToken.end, diagnostics.addError(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), lParenToken.begin, lParenToken.end)))
      }
    }

    private def parseSingleArgument(argExprResult: ExprResult[Pos]): (Option[Pos[Argument[Pos]]], ParseDiagnostics) = {
      argExprResult.value match {
        case Some(expr) =>
          (Some(expr.as(Argument(expr))), argExprResult.diagnostics)
        case None =>
          if (argExprResult.diagnostics.errors.isEmpty) {
            val next = scanner.peek(1)
            (None, argExprResult.diagnostics.addError(Pos(ParseError.MissingExpression("argument"), next.begin, next.end)))
          } else {
            (None, argExprResult.diagnostics)
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
