package software.kes.scaletta.internal.parser

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, LineMapBuilder, Pos}
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner, Token}

class ParserSupportSpec extends AnyFunSpec with Matchers {

  describe("ParserSupport") {
    describe("expect") {
      it("should return the token and not report error if it matches") {
        val scanner = createScanner("123")
        var errorReported = false
        val result = ParserSupport.expect(scanner, Token.IntLiteral(123), "context", _ => errorReported = true)

        result.value shouldBe Token.IntLiteral(123)
        errorReported shouldBe false
      }

      it("should report ExpectedToken and return actual token if it does not match") {
        val scanner = createScanner("true")
        var reportedError: Option[Pos[ParseError]] = None
        val result = ParserSupport.expect(scanner, Token.IntLiteral(123), "context", err => reportedError = Some(err))

        result.value shouldBe Token.True
        reportedError shouldBe Some(Pos(ParseError.ExpectedToken(Token.IntLiteral(123), Token.True, "context"), CharIndex(0), CharIndex(3)))
      }

      it("should report UnclosedDelimiter when EndOfInput is reached while looking for a delimiter") {
        val scanner = createScanner("(")
        scanner.get() // consume '('
        var reportedError: Option[Pos[ParseError]] = None
        val result = ParserSupport.expect(scanner, Token.RParen, "context", err => reportedError = Some(err))

        result.value shouldBe Token.EndOfInput
        reportedError shouldBe Some(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), CharIndex(1), CharIndex(1)))
      }

      it("should use openPos for UnclosedDelimiter error position if provided") {
        val scanner = createScanner("(")
        val openToken = scanner.get()
        var reportedError: Option[Pos[ParseError]] = None
        val result = ParserSupport.expect(scanner, Token.RParen, "context", err => reportedError = Some(err), Some(openToken.begin))

        result.value shouldBe Token.EndOfInput
        reportedError shouldBe Some(Pos(ParseError.UnclosedDelimiter(Token.LParen, Token.RParen), CharIndex(0), CharIndex(1)))
      }

      it("should report ExpectedToken (not UnclosedDelimiter) if EndOfInput is reached but expected is not a delimiter") {
        val scanner = createScanner("")
        var reportedError: Option[Pos[ParseError]] = None
        val result = ParserSupport.expect(scanner, Token.Val, "context", err => reportedError = Some(err))

        result.value shouldBe Token.EndOfInput
        reportedError shouldBe Some(Pos(ParseError.ExpectedToken(Token.Val, Token.EndOfInput, "context"), CharIndex(0), CharIndex(0)))
      }
    }

    describe("expectIdentifier") {
      it("should return the identifier if it matches") {
        val scanner = createScanner("foo")
        var errorReported = false
        val result = ParserSupport.expectIdentifier(scanner, "context", _ => errorReported = true)

        result.value shouldBe Token.Identifier.Lower("foo")
        errorReported shouldBe false
      }

      it("should report ExpectedIdentifier and return synthetic identifier on failure") {
        val scanner = createScanner("123")
        var reportedError: Option[Pos[ParseError]] = None
        val result = ParserSupport.expectIdentifier(scanner, "context", err => reportedError = Some(err))

        result.value shouldBe Token.Identifier.Lower("<error:IntLiteral(123)>")
        reportedError shouldBe Some(Pos(ParseError.ExpectedIdentifier(Token.IntLiteral(123), "context"), CharIndex(0), CharIndex(2)))
      }

      it("should handle EndOfInput with a clean synthetic identifier name") {
        val scanner = createScanner("")
        var reportedError: Option[Pos[ParseError]] = None
        val result = ParserSupport.expectIdentifier(scanner, "context", err => reportedError = Some(err))

        result.value shouldBe Token.Identifier.Lower("<error>")
        reportedError shouldBe Some(Pos(ParseError.ExpectedIdentifier(Token.EndOfInput, "context"), CharIndex(0), CharIndex(0)))
      }
    }

    describe("synchronizeTo") {
      it("should consume tokens until predicate is met") {
        val scanner = createScanner("1 2 3 ; 4")
        val isSemicolon: Token => Boolean = {
          case Token.Semicolon => true
          case _ => false
        }
        val isFatal: Token => Boolean = _ => false

        val fatalHit = ParserSupport.synchronizeTo(scanner, isSemicolon, isFatal)

        fatalHit shouldBe false
        scanner.peek(1).value shouldBe Token.Semicolon
      }

      it("should stop and return true if a fatal token is hit") {
        val scanner = createScanner("1 2 } ; 4")
        val isSemicolon: Token => Boolean = {
          case Token.Semicolon => true
          case _ => false
        }
        val isRBrace: Token => Boolean = {
          case Token.RBrace => true
          case _ => false
        }

        val fatalHit = ParserSupport.synchronizeTo(scanner, isSemicolon, isRBrace)

        fatalHit shouldBe true
        scanner.peek(1).value shouldBe Token.RBrace
      }

      it("should stop at EndOfInput even if predicates don't match") {
        val scanner = createScanner("1 2 3")
        val never: Token => Boolean = _ => false

        val fatalHit = ParserSupport.synchronizeTo(scanner, never, never)

        fatalHit shouldBe false
        scanner.peek(1).value shouldBe Token.EndOfInput
      }
    }

    describe("getOpenForClose") {
      it("should map closing delimiters to their opening counterparts") {
        ParserSupport.getOpenForClose(Token.RParen) shouldBe Token.LParen
        ParserSupport.getOpenForClose(Token.RBrace) shouldBe Token.LBrace
        ParserSupport.getOpenForClose(Token.RBracket) shouldBe Token.LBracket
      }

      it("should return the same token if it's not a closing delimiter") {
        ParserSupport.getOpenForClose(Token.LParen) shouldBe Token.LParen
        ParserSupport.getOpenForClose(Token.Val) shouldBe Token.Val
        ParserSupport.getOpenForClose(Token.IntLiteral(41)) shouldBe Token.IntLiteral(41)
      }
    }

    describe("Boundaries") {
      it("should identify common synchronization boundaries") {
        val boundaries = Set[Token](
          Token.Comma, Token.RParen, Token.EndOfInput, Token.Val, Token.Def,
          Token.If, Token.Case, Token.Semicolon, Token.RBrace
        )

        boundaries.foreach { t =>
          withClue(s"Token $t should be a boundary") {
            ParserSupport.isCommonSynchronizationBoundary(t) shouldBe true
          }
        }

        ParserSupport.isCommonSynchronizationBoundary(Token.IntLiteral(1)) shouldBe false
      }

      it("should identify structural boundaries") {
        val boundaries = Set[Token](
          Token.Val, Token.Def, Token.Else, Token.Then, Token.Case,
          Token.Semicolon, Token.RBrace, Token.EndOfInput
        )

        boundaries.foreach { t =>
          withClue(s"Token $t should be a structural boundary") {
            ParserSupport.isStructuralBoundary(t) shouldBe true
          }
        }

        ParserSupport.isStructuralBoundary(Token.Comma) shouldBe false
      }
    }
  }

  private def createScanner(input: String): Scanner = {
    val reader = SourceReader.create(input.iterator, LineMapBuilder.create(LineMap.create()))
    Scanner.create(reader, IdentifierPolicy.Default)
  }

}
