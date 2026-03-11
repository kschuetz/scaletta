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
    def run(): TypeResult[Pos] = {
      parseTypeIdentifier(BindingPower.Minimum)
    }

    private def parseTypeIdentifier(minPower: BindingPower): TypeResult[Pos] = {
      var result = parseAtom()

      var continue = true
      while (continue && result.value.isDefined) {
        val next = scanner.peek(1)
        next.value match {
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
          val err: Pos[ParseError] = Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end)
          ParseResult.error[Pos, Pos[TypeIdentifier[Pos]]](err)
      }
    }

    private def parseApplied(nameResult: TypeResult[Pos]): TypeResult[Pos] = {
      // nameResult is already a TypeIdentifier.Name
      // We need to extract the identifier from it.
      nameResult.value match {
        case Some(Pos(TypeIdentifier.Name(idPos), begin, _)) =>
          scanner.get() // consume '['
          val argsResult = parseCommaSeparatedTypes(Token.RBracket)
          argsResult.value match {
            case Some(args) if args.nonEmpty =>
              val rbracket = scanner.peek(1)
              if (rbracket.value == Token.RBracket) {
                scanner.get()
                ParseResult(Some(Pos(TypeIdentifier.applied(idPos, args: _*), begin, rbracket.end)),
                  nameResult.diagnostics ++ argsResult.diagnostics)
              } else {
                val err: Pos[ParseError] = Pos(ParseError.UnclosedDelimiter(Token.LBracket, Token.RBracket), begin + idPos.value.name.length, begin + idPos.value.name.length + 1)
                ParseResult(Some(Pos(TypeIdentifier.applied(idPos, args: _*), begin, rbracket.begin)),
                  (nameResult.diagnostics ++ argsResult.diagnostics).addError(err))
              }
            case _ =>
              // Empty brackets or error in args
              val rbracket = scanner.get()
              val err: Pos[ParseError] = Pos(ParseError.UnexpectedToken(rbracket.value), rbracket.begin, rbracket.end)
              ParseResult(None, (nameResult.diagnostics ++ argsResult.diagnostics).addError(err))
          }
        case _ =>
          // Should not happen if called correctly
          nameResult.addError(Pos(ParseError.UnexpectedToken(scanner.peek(1).value), scanner.peek(1).begin, scanner.peek(1).end))
      }
    }

    private def parseConjunction(left: TypeResult[Pos], cType: ConjunctionType, power: BindingPower): TypeResult[Pos] = {
      scanner.get() // consume '&' or '|'
      val right = parseTypeIdentifier(power)
      (left.value, right.value) match {
        case (Some(l), Some(r)) =>
          val components = l.value match {
            case TypeIdentifier.Conjunction(ct, comps) if ct == cType => comps :+ r
            case _ => Vector(l, r)
          }
          ParseResult(Some(Pos(TypeIdentifier.Conjunction(cType, components), l.begin, r.end)),
            left.diagnostics ++ right.diagnostics)
        case _ =>
          ParseResult(None, left.diagnostics ++ right.diagnostics)
      }
    }

    private def parseFunction(left: TypeResult[Pos]): TypeResult[Pos] = {
      scanner.get() // consume '=>'
      val right = parseTypeIdentifier(BindingPower.Minimum)
      (left.value, right.value) match {
        case (Some(l), Some(r)) =>
          val params = Vector(l)
          ParseResult(Some(Pos(TypeIdentifier.function(params, r), l.begin, r.end)),
            left.diagnostics ++ right.diagnostics)
        case _ =>
          ParseResult(None, left.diagnostics ++ right.diagnostics)
      }
    }

    private def parseParenthesizedOrFunctionStart(): TypeResult[Pos] = {
      val lparen = scanner.get()
      val typesResult = parseCommaSeparatedTypes(Token.RParen)
      val rparen = scanner.peek(1)

      if (rparen.value != Token.RParen) {
        val err: Pos[ParseError] = Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), lparen.begin, lparen.end)
        return ParseResult[Pos, Pos[TypeIdentifier[Pos]]](None, typesResult.diagnostics.addError(err))
      }
      scanner.get() // consume ')'

      val next = scanner.peek(1)
      if (next.value == Token.RDoubleArrow) {
        scanner.get() // consume '=>'
        val resultType = parseTypeIdentifier(BindingPower.Minimum)
        resultType.value match {
          case Some(rt) =>
            val params = typesResult.value.getOrElse(Vector.empty)
            ParseResult.create[Pos, Pos[TypeIdentifier[Pos]]](Pos(TypeIdentifier.function(params, rt), lparen.begin, rt.end))
              .addDiagnostics(typesResult.diagnostics ++ resultType.diagnostics)
          case None =>
            ParseResult[Pos, Pos[TypeIdentifier[Pos]]](None, typesResult.diagnostics ++ resultType.diagnostics)
        }
      } else {
        // Parenthesized or tuple type
        typesResult.value match {
          case Some(Vector(t)) =>
            ParseResult.create[Pos, Pos[TypeIdentifier[Pos]]](Pos(t.value, lparen.begin, rparen.end))
              .addDiagnostics(typesResult.diagnostics)
          case Some(elements) =>
            ParseResult.create[Pos, Pos[TypeIdentifier[Pos]]](Pos(TypeIdentifier.tuple(elements), lparen.begin, rparen.end))
              .addDiagnostics(typesResult.diagnostics)
          case None =>
            ParseResult[Pos, Pos[TypeIdentifier[Pos]]](None, typesResult.diagnostics)
        }
      }
    }

    private def parseCommaSeparatedTypes(terminal: Token): ParseResult[Pos, Vector[Pos[TypeIdentifier[Pos]]]] = {
      var results = Vector.empty[Pos[TypeIdentifier[Pos]]]
      var diagnostics = ParseDiagnostics.empty

      if (scanner.peek(1).value == terminal) {
        return ParseResult(Some(results))
      }

      @tailrec
      def go(): Unit = {
        val t = parseTypeIdentifier(BindingPower.Minimum)
        t.value match {
          case Some(v) => results = results :+ v
          case None => // error already in t.diagnostics
        }
        diagnostics = diagnostics ++ t.diagnostics

        val next = scanner.peek(1)
        if (next.value == Token.Comma) {
          scanner.get()
          go()
        } else if (next.value != terminal && !isSynchronizationBoundary(next.value)) {
          // Unexpected token, try to recover
          val err: Pos[ParseError] = Pos(ParseError.UnexpectedToken(next.value), next.begin, next.end)
          diagnostics = diagnostics.addError(err)
          synchronize()
          if (scanner.peek(1).value == Token.Comma) {
            scanner.get()
            go()
          }
        }
      }

      go()
      ParseResult(Some(results), diagnostics)
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
  }
}
