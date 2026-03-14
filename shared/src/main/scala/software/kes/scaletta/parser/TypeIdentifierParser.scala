package software.kes.scaletta.parser

import software.kes.scaletta.ast.{Identifier, TypeIdentifier}
import software.kes.scaletta.reporting.{CharIndex, Pos}
import software.kes.scaletta.scanner.{Scanner, Token}
import software.kes.scaletta.types.ConjunctionType

import scala.annotation.tailrec

private[scaletta] object TypeIdentifierParser {
  def parse(scanner: Scanner): TypeResult[Pos] =
    new Session(scanner).run()

  private class Session(scanner: Scanner) {
    private var diagnostics: ParseDiagnostics = ParseDiagnostics.empty

    def run(): TypeResult[Pos] = {
      val result = parseTypeIdentifier(BindingPower.Minimum)
      result.addDiagnostics(diagnostics)
    }

    private def parseTypeIdentifier(minPower: BindingPower): TypeResult[Pos] = {
      var result = parseAtom()

      var continue = true
      while (continue && result.value.isDefined) {
        val next = scanner.peek(1)
        next.value match {
          case Token.Dot if BindingPower.MemberAccess > minPower =>
            result = parseSelect(result)
          case Token.LBracket =>
            result = parseApplied(result)
          case Token.Ampersand if BindingPower.LogicalAnd > minPower =>
            result = parseConjunction(result, ConjunctionType.Intersection, BindingPower.LogicalAnd)
          case Token.Pipe if BindingPower.LogicalOr > minPower =>
            result = parseConjunction(result, ConjunctionType.Union, BindingPower.LogicalOr)
          case Token.RDoubleArrow if BindingPower.Minimum >= minPower =>
            // Function has lowest precedence, and is right-associative.
            // But we handle it slightly differently because of (A, B) => C
            result = parseFunction(result)
          case _ =>
            continue = false
        }
      }
      result
    }

    private def parseAtom(): TypeResult[Pos] = {
      val next = scanner.peek(1)
      next.value match {
        case _: Token.Identifier =>
          val idToken = expectIdentifier("type")
          ParseResult.create(Pos(TypeIdentifier.name(Pos(Identifier(idToken.value.name), idToken.begin, idToken.end)), idToken.begin, idToken.end))

        case Token.LParen =>
          parseParenthesizedOrFunctionStart()

        case _ =>
          reportError(Pos(ParseError.ExpectedIdentifier(next.value, "type"), next.begin, next.end))
          ParseResult.empty[Pos, Pos[TypeIdentifier[Pos]]]
      }
    }

    private def parseApplied(qualifierResult: TypeResult[Pos]): TypeResult[Pos] = {
      qualifierResult.value match {
        case Some(qualifierPos) =>
          val lbracket = expect(Token.LBracket, "type application")
          val argsResult = parseCommaSeparatedTypes(Token.RBracket)
          argsResult.value match {
            case Some(args) if args.nonEmpty =>
              val rbracket = expect(Token.RBracket, "type application", Some(lbracket.begin))
              ParseResult.create(Pos(TypeIdentifier.Applied(qualifierPos, ::(args.head, args.tail.toList)), qualifierPos.begin, rbracket.end))
            case _ =>
              // Empty brackets or error in args
              val rbracket = expect(Token.RBracket, "type application")
              reportError(Pos(ParseError.ExpectedIdentifier(rbracket.value, "type argument"), rbracket.begin, rbracket.end))
              ParseResult.empty
          }
        case _ =>
          // Should not happen if called correctly
          reportError(Pos(ParseError.UnexpectedToken(scanner.peek(1).value), scanner.peek(1).begin, scanner.peek(1).end))
          qualifierResult
      }
    }

    private def parseSelect(qualifierResult: TypeResult[Pos]): TypeResult[Pos] = {
      qualifierResult.value match {
        case Some(qualifierPos) =>
          expect(Token.Dot, "type selection")
          val idToken = expectIdentifier("type selection")
          val idPos: Pos[Identifier[Pos]] = Pos(Identifier(idToken.value.name), idToken.begin, idToken.end)
          val selectPos: Pos[TypeIdentifier[Pos]] = Pos(TypeIdentifier.Select(qualifierPos, idPos), qualifierPos.begin, idToken.end)
          ParseResult.create(selectPos)
        case None => qualifierResult
      }
    }

    private def parseConjunction(left: TypeResult[Pos], cType: ConjunctionType, power: BindingPower): TypeResult[Pos] = {
      scanner.get() // consume '&' or '|'
      val right = parseTypeIdentifier(power)
      (left.value, right.value) match {
        case (Some(l), Some(r)) =>
          val components: Vector[Pos[TypeIdentifier[Pos]]] = l.value match {
            case TypeIdentifier.Conjunction(ct, comps) if ct == cType => comps :+ r
            case _ => Vector(l, r)
          }
          ParseResult.create(Pos(TypeIdentifier.Conjunction(cType, components), l.begin, r.end))
        case _ =>
          ParseResult.empty
      }
    }

    private def parseFunction(left: TypeResult[Pos]): TypeResult[Pos] = {
      scanner.get() // consume '=>'
      val right = parseTypeIdentifier(BindingPower.Minimum)
      (left.value, right.value) match {
        case (Some(l), Some(r)) =>
          val params = Vector(l)
          ParseResult.create(Pos(TypeIdentifier.function(params, r), l.begin, r.end))
        case _ =>
          ParseResult.empty
      }
    }

    private def parseParenthesizedOrFunctionStart(): TypeResult[Pos] = {
      val lparen = expect(Token.LParen, "parenthesized type")
      val typesResult = parseCommaSeparatedTypes(Token.RParen)
      val rparen = expect(Token.RParen, "parenthesized type", Some(lparen.begin))

      val next = scanner.peek(1)
      if (next.value == Token.RDoubleArrow) {
        scanner.get() // consume '=>'
        val resultType = parseTypeIdentifier(BindingPower.Minimum)
        resultType.value match {
          case Some(rt) =>
            val params = typesResult.value.getOrElse(Vector.empty)
            ParseResult.create(Pos(TypeIdentifier.function(params, rt), lparen.begin, rt.end))
          case None =>
            ParseResult.empty
        }
      } else {
        // Parenthesized or tuple type
        typesResult.value match {
          case Some(Vector(t)) =>
            ParseResult.create(Pos(t.value, lparen.begin, rparen.end))
          case Some(elements) =>
            ParseResult.create(Pos(TypeIdentifier.tuple(elements), lparen.begin, rparen.end))
          case None =>
            ParseResult.empty
        }
      }
    }

    private def parseCommaSeparatedTypes(terminal: Token): ParseResult[Pos, Vector[Pos[TypeIdentifier[Pos]]]] = {
      if (scanner.peek(1).value == terminal) {
        return ParseResult(Some(Vector.empty))
      }

      val results = Vector.newBuilder[Pos[TypeIdentifier[Pos]]]

      @tailrec
      def go(): Unit = {
        val t = parseTypeIdentifier(BindingPower.Minimum)
        t.value match {
          case Some(v) => results += v
          case None => // error already reported
        }

        val next = scanner.peek(1)
        if (next.value == Token.Comma) {
          scanner.get()
          go()
        } else if (next.value != terminal && !isSynchronizationBoundary(next.value)) {
          // Unexpected token, try to recover
          reportError(Pos(ParseError.ExpectedToken(Token.Comma, next.value, "type list"), next.begin, next.end))
          synchronize()
          if (scanner.peek(1).value == Token.Comma) {
            scanner.get()
            go()
          }
        }
      }

      go()
      ParseResult(Some(results.result()))
    }

    private def isSynchronizationBoundary(token: Token): Boolean = {
      ParserSupport.isCommonSynchronizationBoundary(token) || token == Token.RBracket
    }

    private def synchronize(): Unit = {
      ParserSupport.synchronizeTo(scanner, isSynchronizationBoundary, _ => false)
    }

    private def expect(expected: Token, context: String, openPos: Option[CharIndex] = None): Pos[Token] = {
      ParserSupport.expect(scanner, expected, context, reportError, openPos)
    }

    private def expectIdentifier(context: String): Pos[Token.Identifier] = {
      ParserSupport.expectIdentifier(scanner, context, reportError)
    }

    private def reportError(error: Pos[ParseError]): Unit =
      diagnostics = diagnostics.addError(error)

  }
}
