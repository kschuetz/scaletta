package software.kes.scaletta.parser

import software.kes.scaletta.ast.{Identifier, TypeIdentifier}
import software.kes.scaletta.reporting.Pos
import software.kes.scaletta.scanner.{Scanner, Token}
import software.kes.scaletta.types.ConjunctionType

import scala.annotation.tailrec

object TypeIdentifierParser {
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
        case id: Token.Identifier =>
          scanner.get()
          ParseResult.create(Pos(TypeIdentifier.name(Pos(Identifier(id.name), next.begin, next.end)), next.begin, next.end))

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
          scanner.get() // consume '['
          val argsResult = parseCommaSeparatedTypes(Token.RBracket)
          argsResult.value match {
            case Some(args) if args.nonEmpty =>
              val rbracket = scanner.peek(1)
              if (rbracket.value == Token.RBracket) {
                scanner.get()
                ParseResult.create(Pos(TypeIdentifier.Applied(qualifierPos, ::(args.head, args.tail.toList)), qualifierPos.begin, rbracket.end))
              } else {
                reportError(Pos(ParseError.UnclosedDelimiter(Token.LBracket, Token.RBracket), qualifierPos.end + 1, qualifierPos.end + 2))
                ParseResult.create(Pos(TypeIdentifier.Applied(qualifierPos, ::(args.head, args.tail.toList)), qualifierPos.begin, rbracket.begin))
              }
            case _ =>
              // Empty brackets or error in args
              val rbracket = scanner.get()
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
          scanner.get() // consume '.'
          val next = scanner.peek(1)
          next.value match {
            case id: Token.Identifier =>
              scanner.get()
              val idPos: Pos[Identifier[Pos]] = Pos(Identifier(id.name), next.begin, next.end)
              val selectPos: Pos[TypeIdentifier[Pos]] = Pos(TypeIdentifier.Select(qualifierPos, idPos), qualifierPos.begin, next.end)
              ParseResult.create(selectPos)
            case _ =>
              reportError(Pos(ParseError.ExpectedIdentifier(next.value, "type selection"), next.begin, next.end))
              ParseResult.empty
          }
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
      val lparen = scanner.get()
      val typesResult = parseCommaSeparatedTypes(Token.RParen)
      val rparen = scanner.peek(1)

      if (rparen.value != Token.RParen) {
        reportError(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), lparen.begin, lparen.end))
        return ParseResult.empty
      }
      scanner.get() // consume ')'

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
      var results = Vector.empty[Pos[TypeIdentifier[Pos]]]

      if (scanner.peek(1).value == terminal) {
        return ParseResult(Some(results))
      }

      @tailrec
      def go(): Unit = {
        val t = parseTypeIdentifier(BindingPower.Minimum)
        t.value match {
          case Some(v) => results = results :+ v
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
      ParseResult(Some(results))
    }

    private def isSynchronizationBoundary(token: Token): Boolean = {
      token match {
        case Token.Comma | Token.RParen | Token.RBracket | Token.RBrace | Token.Semicolon | Token.EndOfInput => true
        case Token.Val | Token.Def => true
        case _ => false
      }
    }

    private def synchronize(): Unit = {
      while (!isSynchronizationBoundary(scanner.peek(1).value)) {
        scanner.get()
      }
    }

    private def reportError(error: Pos[ParseError]): Unit =
      diagnostics = diagnostics.addError(error)

  }
}
