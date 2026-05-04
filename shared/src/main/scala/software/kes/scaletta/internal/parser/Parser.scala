package software.kes.scaletta.internal.parser

import software.kes.scaletta.internal.ast._
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.internal.scanner.{Scanner, Token}

object Parser {
  def create(): Parser = new Parser()
}

case class ParseOptions(requireExhaustion: Boolean = true)

final class Parser private() {

  def parse(scanner: Scanner, options: ParseOptions = ParseOptions()): ExprResult[Pos] = {
    new Session(scanner, options).run
  }

  private class Session(scanner: Scanner, options: ParseOptions) {
    private var diagnostics: ParseDiagnostics = ParseDiagnostics.empty

    def run: ExprResult[Pos] = {
      val result = parseExpression(BindingPower.Minimum)
      val resultWithExhaustion = if (options.requireExhaustion) {
        val next = scanner.peek(1)
        next.value match {
          case Token.EndOfInput => result
          case _ =>
            reportError(Pos(ParseError.ExtraToken(next.value, "end of input"), next.begin, next.end))
            result
        }
      } else {
        result
      }
      resultWithExhaustion.addDiagnostics(diagnostics)
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
      if (ParserSupport.isStructuralBoundary(token)) {
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
          if (isStructuralBoundary(token.value)) {
            val error = ParseError.ExpectedIdentifier(token.value, "expression")
            reportError(Pos(error, token.begin, token.end))
            ParseResult.create[Pos, Pos[Expression[Pos]]](syntheticExpression(error, token.begin, token.end))
          } else {
            val error = ParseError.UnexpectedToken(token.value)
            reportError(Pos(error, token.begin, token.end))
            ParseResult.create[Pos, Pos[Expression[Pos]]](syntheticExpression(error, token.begin, token.end))
          }
      }
    }

    private def parseParenthesizedExpression(token: Pos[Token]): ExprResult[Pos] = {
      val result = parseExpression(BindingPower.Minimum)
      val next = expect(Token.RParen, "parenthesized expression", Some(token.begin))
      processParenthesizedResult(token, result, next)
    }

    private def processParenthesizedResult(token: Pos[Token], result: ExprResult[Pos], next: Pos[Token]): ExprResult[Pos] = {
      result.value match {
        case Some(inner) =>
          val isUnnecessary = inner.value.getClass match {
            case c if classOf[Literal[Pos]].isAssignableFrom(c) => true
            case c if classOf[Reference[Pos]].isAssignableFrom(c) => true
            case _ => false
          }
          if (isUnnecessary) {
            reportHint(Pos(ParseHint.UnnecessaryParentheses, token.begin, next.end))
          }
          ParseResult.create(Pos(inner.value, token.begin, next.end))
        case None => result
      }
    }

    private def parseBlock(token: Pos[Token]): ExprResult[Pos] = {
      val declarations = Vector.newBuilder[Pos[Declaration[Pos]]]

      def isAtDeclarationStart: Boolean = {
        scanner.peek(1).value match {
          case Token.Val | Token.Lazy | Token.Def => true
          case _ => false
        }
      }

      while (isAtDeclarationStart) {
        val declResult = parseDeclaration()
        declResult.value match {
          case Some(d) =>
            declarations += d
            // Semicolon check moved here to ensure it's checked after a successful declaration
            if (scanner.peek(1).value != Token.RBrace) {
              val nextToken = scanner.peek(1)
              nextToken.value match {
                case Token.Semicolon | Token.Newline =>
                  scanner.get()
                case _ =>
                  if (isAtDeclarationStart) {
                    reportError(Pos(ParseError.ExpectedToken(Token.Semicolon, nextToken.value, "block"), nextToken.begin, nextToken.end))
                  } else if (!isStructuralBoundary(nextToken.value)) {
                    reportError(Pos(ParseError.ExpectedToken(Token.Semicolon, nextToken.value, "block"), nextToken.begin, nextToken.end))
                    if (synchronizeToNextDeclaration()) {
                      val lastError = diagnostics.errors.last.value
                      val resultExpr = ParseResult.create[Pos, Pos[Expression[Pos]]](syntheticExpression(lastError, nextToken.begin, nextToken.end))
                      return finalizeBlock(token, declarations.result(), resultExpr)
                    }
                  }
              }
            }
          case None =>
            // If parseDeclaration returned None, it must have already reported an error.
            // We'll create a synthetic error declaration to keep the block complete.
            val lastError = diagnostics.errors.lastOption.map(_.value).getOrElse(ParseError.Message("unknown error in declaration"))
            val nextToken = scanner.peek(1)
            declarations += syntheticDeclaration(lastError, nextToken.begin, nextToken.end)
            val syncResult = synchronizeToNextDeclaration()
            if (syncResult && scanner.peek(1).value == Token.RBrace) {
              // Return early if we hit a structural boundary (like Token.RBrace handled below)
              val resultExpr = ParseResult.create[Pos, Pos[Expression[Pos]]](syntheticExpression(lastError, nextToken.begin, nextToken.end))
              return finalizeBlock(token, declarations.result(), resultExpr)
            }
        }
      }

      val resultExpr = parseExpression(BindingPower.Minimum)
      finalizeBlock(token, declarations.result(), resultExpr)
    }

    private def finalizeBlock(token: Pos[Token], declarations: Vector[Pos[Declaration[Pos]]], resultExpr: ExprResult[Pos]): ExprResult[Pos] = {
      val next = expect(Token.RBrace, "block", Some(token.begin))
      next.value match {
        case Token.RBrace =>
          val finalExpr = resultExpr.value match {
            case Some(res) => res
            case None =>
              val error = if (diagnostics.hasErrors) {
                diagnostics.errors.last.value
              } else {
                ParseError.MissingExpression("block result")
              }
              if (!diagnostics.hasErrors) {
                reportError(Pos(error, next.begin, next.end))
              }
              syntheticExpression(error, next.begin, next.end)
          }
          ParseResult.create(Pos(Block(declarations, finalExpr), token.begin, next.end))
        case _ =>
          // Even if we couldn't find the closing brace, we return a block with what we have.
          val error = ParseError.UnclosedDelimiter(token.value, Token.RBrace)
          val finalExpr = resultExpr.value.getOrElse(syntheticExpression(error, next.begin, next.end))
          ParseResult.create(Pos(Block(declarations, finalExpr), token.begin, next.end))
      }
    }

    private def parseDeclaration(): DeclResult[Pos] = {
      val firstToken = scanner.get()
      firstToken.value match {
        case Token.Val =>
          parseValDeclaration(firstToken)
        case Token.Lazy =>
          val next = expect(Token.Val, "lazy val declaration")
          if (next.value == Token.Val) {
            parseLazyValDeclaration(firstToken, next)
          } else {
            ParseResult.empty
          }
        case Token.Def =>
          parseDefDeclaration(firstToken)
        case _ =>
          reportError(Pos(ParseError.UnexpectedToken(firstToken.value), firstToken.begin, firstToken.end))
          ParseResult.empty
      }
    }

    private def parseValDeclaration(valToken: Pos[Token]): DeclResult[Pos] = {
      val patternResult = parsePattern()
      val eqToken = expect(Token.Eq, "val declaration")
      val pattern = patternResult.value.getOrElse {
        val lastError = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.ExpectedIdentifier(Token.EndOfInput, "pattern")
        syntheticPattern(lastError, valToken.end, eqToken.begin)
      }

      val rhsResult = if (eqToken.value == Token.Eq) {
        parseExpression(BindingPower.Minimum)
      } else {
        val error = ParseError.ExpectedToken(Token.Eq, eqToken.value, "val declaration")
        val pos = syntheticExpression(error, eqToken.begin, eqToken.end)
        ParseResult.create[Pos, Pos[Expression[Pos]]](pos)
      }

      val rhs = rhsResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("val rhs")
        if (!diagnostics.hasErrors) reportError(Pos(error, eqToken.begin, eqToken.end))
        syntheticExpression(error, eqToken.begin, eqToken.end)
      }

      ParseResult.create(Pos(Declaration.val_(pattern, rhs), valToken.begin, rhs.end))
    }

    private def parseLazyValDeclaration(lazyToken: Pos[Token], valToken: Pos[Token]): DeclResult[Pos] = {
      val patternResult = parsePattern()
      val eqToken = expect(Token.Eq, "lazy val declaration")
      val pattern = patternResult.value.getOrElse {
        val lastError = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.ExpectedIdentifier(Token.EndOfInput, "pattern")
        syntheticPattern(lastError, valToken.end, eqToken.begin)
      }

      val rhsResult = if (eqToken.value == Token.Eq) {
        parseExpression(BindingPower.Minimum)
      } else {
        val error = ParseError.ExpectedToken(Token.Eq, eqToken.value, "lazy val declaration")
        val pos = syntheticExpression(error, eqToken.begin, eqToken.end)
        ParseResult.create[Pos, Pos[Expression[Pos]]](pos)
      }

      val rhs = rhsResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("lazy val rhs")
        if (!diagnostics.hasErrors) reportError(Pos(error, eqToken.begin, eqToken.end))
        syntheticExpression(error, eqToken.begin, eqToken.end)
      }

      ParseResult.create(Pos(Declaration.lazyVal(pattern, rhs), lazyToken.begin, rhs.end))
    }

    private def parseFormalParameterGroup(): ParseResult[Pos, Pos[FormalParameterGroup[Pos]]] = {
      val lParen = expect(Token.LParen, "formal parameter group")
      if (lParen.value != Token.LParen) {
        return ParseResult.empty
      }

      val params = Vector.newBuilder[Pos[FormalParameter[Pos]]]
      var variadic: Option[Pos[FormalParameter[Pos]]] = None

      def continue(): Boolean = {
        val next = scanner.peek(1).value
        next != Token.RParen && next != Token.EndOfInput && !isStructuralBoundary(next)
      }

      while (continue() && variadic.isEmpty) {
        val nameToken = expectIdentifier("formal parameter")
        val name = nameToken.as(Identifier[Pos](nameToken.value.name))
        val colon = expect(Token.Colon, "formal parameter")
        if (colon.value == Token.Colon) {
          val typeResult = TypeIdentifierParser.parse(scanner)
          addDiagnostics(typeResult.diagnostics)

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
                val defaultExpr = defaultResult.value.getOrElse {
                  val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("parameter default value")
                  if (!diagnostics.hasErrors) reportError(Pos(error, eqToken.begin, eqToken.end))
                  syntheticExpression(error, eqToken.begin, eqToken.end)
                }
                default = Some(defaultExpr)
              }

              val pEnd = default.map(_.end).getOrElse(asteriskPos.map(_.end).getOrElse(t.end))
              val param = Pos(FormalParameter(name, t, default), nameToken.begin, pEnd)

              if (hasAsterisk) {
                if (default.isDefined) {
                  val errPos = asteriskPos.getOrElse(Pos(Token.EndOfInput, t.end, t.end))
                  reportError(Pos(ParseError.Message("Variadic parameter cannot have a default value"), errPos.begin, errPos.end))
                }
                variadic = Some(param)
              } else {
                params += param
              }
            case None =>
            // Error already in diagnostics from TypeIdentifierParser
          }
        }

        val next = scanner.peek(1)
        if (next.value == Token.Comma) {
          scanner.get()
          if (variadic.isDefined) {
            reportError(Pos(ParseError.VariadicParameterMustBeLast, next.begin, next.end))
          }
        } else if (next.value != Token.RParen && continue()) {
          reportError(Pos(ParseError.ExpectedToken(Token.Comma, next.value, "formal parameter group"), next.begin, next.end))
          val isFatal: Token => Boolean = {
            case Token.RBrace | Token.EndOfInput => true
            case _ => false
          }
          if (synchronizeTo(t => t == Token.Comma || t == Token.RParen, isFatal)) {
            val groupEnd = scanner.peek(1).end
            return ParseResult.create(Pos(FormalParameterGroup(params.result(), variadic), lParen.begin, groupEnd))
          }
          if (scanner.peek(1).value == Token.Comma) {
            scanner.get()
          }
        }
      }

      val rParen = expect(Token.RParen, "formal parameter group", Some(lParen.begin))
      val groupEnd = if (rParen.value == Token.RParen) rParen.end else scanner.peek(1).end

      ParseResult.create(Pos(FormalParameterGroup(params.result(), variadic), lParen.begin, groupEnd))
    }

    private def parseDefDeclaration(defToken: Pos[Token]): DeclResult[Pos] = {
      val nameToken = expectIdentifier("def declaration")
      val name = nameToken.as(Identifier[Pos](nameToken.value.name))

      // Step 4: Parse multiple parameter groups (currying support)
      val paramGroups = Vector.newBuilder[Pos[FormalParameterGroup[Pos]]]

      while (scanner.peek(1).value == Token.LParen) {
        val groupResult = parseFormalParameterGroup()
        groupResult.value.foreach(paramGroups += _)
      }

      // Step 3: Optional Return Type
      val returnType: Option[Pos[TypeIdentifier[Pos]]] = if (scanner.peek(1).value == Token.Colon) {
        scanner.get() // Consume ':'
        val tr = TypeIdentifierParser.parse(scanner)
        addDiagnostics(tr.diagnostics)
        tr.value
      } else {
        None
      }

      val eqToken = expect(Token.Eq, "def declaration")
      val rhsResult = if (eqToken.value == Token.Eq) {
        parseExpression(BindingPower.Minimum)
      } else {
        val error = ParseError.ExpectedToken(Token.Eq, eqToken.value, "def declaration")
        val pos = syntheticExpression(error, eqToken.begin, eqToken.end)
        ParseResult.create[Pos, Pos[Expression[Pos]]](pos)
      }

      val rhs = rhsResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("def body")
        if (!diagnostics.hasErrors) reportError(Pos(error, eqToken.begin, eqToken.end))
        syntheticExpression(error, eqToken.begin, eqToken.end)
      }

      ParseResult.create(Pos(Declaration.def_(name, paramGroups.result(), returnType, rhs), defToken.begin, rhs.end))
    }

    private def parsePattern(): PatResult[Pos] = {
      val firstToken = scanner.get()
      val baseResult: PatResult[Pos] = firstToken.value match {
        case idToken: Token.Identifier =>
          val id = firstToken.as(Identifier[Pos](idToken.name))
          ParseResult.create(firstToken.as(Pattern.Identifier(id)))
        case Token.Underscore =>
          ParseResult.create(firstToken.as(Pattern.Wildcard()))
        case _ =>
          reportError(Pos(ParseError.ExpectedIdentifier(firstToken.value, "pattern"), firstToken.begin, firstToken.end))
          ParseResult.empty
      }

      (baseResult.value, scanner.peek(1).value) match {
        case (Some(basePat), Token.Colon) =>
          scanner.get() // consume ':'
          val typeResult = TypeIdentifierParser.parse(scanner)
          addDiagnostics(typeResult.diagnostics)
          typeResult.value match {
            case Some(ascription) =>
              val typedPat: Pattern[Pos] = Pattern.Typed(basePat, ascription)
              ParseResult.create(Pos(typedPat, basePat.begin, ascription.end))
            case None =>
              // TypeIdentifierParser already reported error if it couldn't produce a value
              baseResult
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
      val nameToken = expectIdentifier("selection")
      val name = nameToken.as(Identifier[Pos](nameToken.value.name))
      leftResult.value match {
        case Some(qualifier) =>
          val selection: Expression[Pos] = Select[Pos](qualifier, name)
          ParseResult.create(Pos(selection, qualifier.begin, nameToken.end))
        case None =>
          // This should ideally not happen in a correctly functioning Pratt parser
          // if leftResult.value is None, it should have diagnostics already.
          if (!diagnostics.hasErrors) {
            reportError(Pos(ParseError.MissingExpression("selection qualifier"), dotToken.begin, dotToken.end))
          }
          ParseResult.empty
      }
    }

    private def parseTypeAscription(leftResult: ExprResult[Pos], colonToken: Pos[Token]): ExprResult[Pos] = {
      val typeResult: TypeResult[Pos] = TypeIdentifierParser.parse(scanner)
      addDiagnostics(typeResult.diagnostics)

      (leftResult.value, typeResult.value) match {
        case (Some(leftExpr), Some(ascription)) =>
          val typedNode: Expression[Pos] = Typed[Pos](leftExpr, ascription)
          ParseResult.create[Pos, Pos[Expression[Pos]]](Pos(
            typedNode,
            leftExpr.begin,
            ascription.end
          ))

        case _ =>
          // Synchronize if we failed to parse a type
          if (typeResult.value.isEmpty) {
            while (!isSynchronizationBoundary(scanner.peek(1).value)) {
              scanner.get()
            }
            if (scanner.peek(1).value == Token.Semicolon) {
              scanner.get()
            }
          }
          leftResult
      }
    }

    private def parseFunctionCall(leftResult: ExprResult[Pos], opToken: Pos[Token]): ExprResult[Pos] = {
      val (argsOpt, end) = parseArgumentGroup(opToken)
      val group: Pos[ArgumentGroup[Pos]] = argsOpt match {
        case Some(args) => Pos(args, opToken.begin, end)
        case None =>
          val lastError = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.Message("invalid argument group")
          Pos(ArgumentGroup[Pos](Vector.empty), opToken.begin, end)
      }

      val left = leftResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("call target")
        syntheticExpression(error, opToken.begin, opToken.begin)
      }

      val call: Expression[Pos] = left.value match {
        case sc: Call.Standard[Pos] @unchecked =>
          sc.copy(args = sc.args :+ group)
        case _ =>
          Call.standard(left, Vector.empty[Pos[TypeArgument[Pos]]], Vector[Pos[ArgumentGroup[Pos]]](group))
      }

      ParseResult.create[Pos, Pos[Expression[Pos]]](Pos(call, left.begin, end))
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
          val resValue = Pos(
            Call.infix(left, opId, Vector.empty, right),
            left.begin,
            right.end
          )
          if (isSuspicious) {
            reportWarning(Pos(ParseWarning.SuspiciousInfixExpression(opName), opToken.begin, opToken.end))
          }
          ParseResult.create[Pos, Pos[Expression[Pos]]](resValue.as(resValue.value: Expression[Pos]))
        case (Some(left), None) =>
          val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("infix rhs")
          val right = syntheticExpression(error, scanner.peek(1).begin, scanner.peek(1).end)
          val opId = Pos(Identifier[Pos](opName), opToken.begin, opToken.end)
          val resValue = Pos(
            Call.infix(left, opId, Vector.empty, right),
            left.begin,
            right.end
          )
          ParseResult.create[Pos, Pos[Expression[Pos]]](resValue.as(resValue.value: Expression[Pos]))
        case (None, Some(right)) =>
          val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("infix lhs")
          val left = syntheticExpression(error, opToken.begin, opToken.begin)
          val opId = Pos(Identifier[Pos](opName), opToken.begin, opToken.end)
          val resValue = Pos(
            Call.infix(left, opId, Vector.empty, right),
            left.begin,
            right.end
          )
          ParseResult.create[Pos, Pos[Expression[Pos]]](resValue.as(resValue.value: Expression[Pos]))
        case _ =>
          val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("infix expression")
          val pos = syntheticExpression(error, opToken.begin, opToken.end)
          ParseResult.create[Pos, Pos[Expression[Pos]]](pos)
      }
    }

    private def synchronizeTo(predicate: Token => Boolean, isFatal: Token => Boolean): Boolean = {
      ParserSupport.synchronizeTo(scanner, predicate, isFatal)
    }

    private def synchronizeToNextDeclaration(): Boolean = {
      val isDeclStart: Token => Boolean = {
        case Token.Val | Token.Def | Token.Lazy => true
        case _ => false
      }
      val isSyncBoundary: Token => Boolean = {
        case t if isDeclStart(t) => true
        case Token.Semicolon | Token.RBrace | Token.EndOfInput => true
        case _ => false
      }

      val isFatal: Token => Boolean = {
        case Token.RBrace | Token.EndOfInput => true
        case _ => false
      }

      synchronizeTo(isSyncBoundary, isFatal)
      val next = scanner.peek(1).value
      if (next == Token.Semicolon) {
        scanner.get()
        false
      } else {
        ParserSupport.isStructuralBoundary(next) && next != Token.RBrace
      }
    }

    private def isSynchronizationBoundary(token: Token): Boolean = {
      ParserSupport.isCommonSynchronizationBoundary(token)
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
        val lParenToken = scanner.get()
        val cond = parseExpression(BindingPower.Minimum)
        expect(Token.RParen, "conditional", Some(lParenToken.begin))
        if (scanner.peek(1).value == Token.Then) {
          scanner.get()
        }
        cond
      } else {
        val cond = parseExpression(BindingPower.Minimum)
        val currentNext = scanner.peek(1)
        if (currentNext.value == Token.Then) {
          scanner.get()
        } else {
          reportError(Pos(ParseError.ExpectedToken(Token.Then, currentNext.value, "conditional"), currentNext.begin, currentNext.end))
        }
        cond
      }

      val condition = conditionResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("conditional condition")
        if (!diagnostics.hasErrors) reportError(Pos(error, ifToken.end, ifToken.end))
        syntheticExpression(error, ifToken.end, ifToken.end)
      }

      val thenResult = parseExpression(BindingPower.Minimum)
      val thenBranch = thenResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("conditional then branch")
        val p = scanner.peek(1)
        val pos = Pos(error, p.begin, p.end)
        if (!diagnostics.hasErrors) reportError(pos)
        syntheticExpression(error, p.begin, p.end)
      }

      val nextAfterThen = scanner.peek(1)
      val elseResult: ExprResult[Pos] = if (nextAfterThen.value == Token.Else) {
        scanner.get()
        parseExpression(BindingPower.Minimum)
      } else {
        reportError(Pos(ParseError.ExpectedToken(Token.Else, nextAfterThen.value, "conditional"), nextAfterThen.begin, nextAfterThen.end))
        val error = ParseError.ExpectedToken(Token.Else, nextAfterThen.value, "conditional")
        ParseResult.create[Pos, Pos[Expression[Pos]]](syntheticExpression(error, nextAfterThen.begin, nextAfterThen.end))
      }

      val elseBranch = elseResult.value.getOrElse {
        val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("conditional else branch")
        syntheticExpression(error, nextAfterThen.begin, nextAfterThen.end)
      }

      ParseResult.create(Pos(Conditional(condition, thenBranch, elseBranch), ifToken.begin, elseBranch.end))
    }

    private def parseArgumentGroup(lParenToken: Pos[Token]): (Option[ArgumentGroup[Pos]], CharIndex) = {
      val args = Vector.newBuilder[Pos[Argument[Pos]]]
      var hasNamedArgument = false

      def continueParsing(): Boolean = {
        val next = scanner.peek(1).value
        next != Token.RParen && next != Token.EndOfInput
      }

      while (continueParsing()) {
        val nextToken = scanner.peek(1)
        if (nextToken.value == Token.Comma) {
          val errorPos: Pos[ParseError] = nextToken.as(ParseError.MissingExpression("argument"))
          reportError(errorPos)
          val errorExpr = syntheticExpression(errorPos.value, errorPos.begin, errorPos.end)
          args += errorPos.as(Argument(errorExpr))
          scanner.get() // consume comma
        } else {
          val argExprResult = parseExpression(BindingPower.Minimum)
          val argPos = parseSingleArgument(argExprResult)
          if (argPos.value.name.isDefined) {
            hasNamedArgument = true
          } else if (hasNamedArgument) {
            reportError(argPos.as(ParseError.PositionalAfterNamedArgument))
          }
          args += argPos

          val next = scanner.peek(1)
          next.value match {
            case Token.Comma =>
              scanner.get() // consume comma
            case Token.RParen => // will exit loop
            case _ if continueParsing() =>
              if (synchronizeToNextArgument()) {
                // Return early if we hit a structural boundary
                val rParenToken = expect(Token.RParen, "argument group", Some(lParenToken.begin))
                return (Some(ArgumentGroup[Pos](args.result())), rParenToken.end)
              }
            case _ =>
          }
        }
      }

      val rParenToken = expect(Token.RParen, "argument group", Some(lParenToken.begin))
      (Some(ArgumentGroup[Pos](args.result())), rParenToken.end)
    }

    private def parseSingleArgument(argExprResult: ExprResult[Pos]): Pos[Argument[Pos]] = {
      argExprResult.value match {
        case Some(expr) =>
          val nextToken = scanner.peek(1)
          expr.value match {
            case Reference(idPos) if nextToken.value == Token.Eq =>
              scanner.get() // consume '='
              val valueResult = parseExpression(BindingPower.Minimum)
              val valueExpr = valueResult.value.getOrElse {
                val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("named argument value")
                syntheticExpression(error, nextToken.begin, nextToken.end)
              }
              Pos(Argument(valueExpr, Some(idPos)), expr.begin, valueExpr.end)
            case _ =>
              expr.as(Argument(expr))
          }
        case None =>
          val nextToken = scanner.peek(1)
          val error = if (diagnostics.hasErrors) diagnostics.errors.last.value else ParseError.MissingExpression("argument")
          val errorExpr = syntheticExpression(error, nextToken.begin, nextToken.end)
          nextToken.as(Argument(errorExpr))
      }
    }

    private def synchronizeToNextArgument(): Boolean = {
      val next = scanner.peek(1)
      if (!isSynchronizationBoundary(next.value) && next.value != Token.Comma && next.value != Token.RParen) {
        reportError(Pos(ParseError.ExpectedToken(Token.Comma, next.value, "argument list"), next.begin, next.end))
        // Ensure progress if we aren't at a comma or closing paren
        scanner.get()
      }

      val isSyncPoint: Token => Boolean = {
        case Token.Comma | Token.RParen => true
        case _ => false
      }

      val isFatal: Token => Boolean = {
        case Token.RBrace | Token.EndOfInput => true
        case _ => false
      }

      synchronizeTo(isSyncPoint, isFatal)

      val sync = scanner.peek(1)
      if (sync.value == Token.Comma) {
        scanner.get()
        false
      } else {
        isFatal(sync.value)
      }
    }

    private def reportError(error: Pos[ParseError]): Unit =
      diagnostics = diagnostics.addError(error)

    private def syntheticExpression(error: ParseError, begin: CharIndex, end: CharIndex): Pos[Expression[Pos]] =
      Pos(Expression.Error[Pos](error), begin, end)

    private def syntheticDeclaration(error: ParseError, begin: CharIndex, end: CharIndex): Pos[Declaration[Pos]] =
      Pos(Declaration.Error[Pos](error), begin, end)

    private def syntheticPattern(error: ParseError, begin: CharIndex, end: CharIndex): Pos[Pattern[Pos]] =
      Pos(Pattern.Error[Pos](error), begin, end)

    private def reportWarning(warning: Pos[ParseWarning]): Unit =
      diagnostics = diagnostics.addWarning(warning)

    private def reportHint(hint: Pos[ParseHint]): Unit =
      diagnostics = diagnostics.addHint(hint)

    /**
     * Consumes the next token and verifies it matches the `expected` token.
     *
     * If the token matches, it is returned.
     *
     * If the token does not match, a [[ParseError.ExpectedToken]] (or [[ParseError.UnclosedDelimiter]]
     * if the end of input was reached while looking for a closing delimiter) is reported to the session's
     * diagnostics. The actual token found is still returned to allow the parser to attempt to continue.
     *
     * @param expected the token that is expected to be at the current position
     * @param context  a descriptive string explaining what was being parsed (e.g., "if condition")
     * @param openPos  optional starting position of an opening delimiter, used to provide better error
     *                 locations for unclosed delimiters
     * @return the token consumed from the scanner
     */
    private def expect(expected: Token, context: String, openPos: Option[CharIndex] = None): Pos[Token] = {
      ParserSupport.expect(scanner, expected, context, reportError, openPos)
    }

    private def expectIdentifier(context: String): Pos[Token.Identifier] = {
      ParserSupport.expectIdentifier(scanner, context, reportError)
    }

    private def addDiagnostics(other: ParseDiagnostics): Unit =
      diagnostics = diagnostics ++ other
  }
}
