package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerConstants.{Raw, TripleQuote}
import software.kes.scaletta.scanner.ScannerResult._
import software.kes.scaletta.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object Scanner {
  def create(reader: CharReader,
             identifierPolicy: IdentifierPolicy): Scanner = {
    val buffer = CharBuffer.create()
    val identifierScanner = new IdentifierScanner(identifierPolicy)
    new Scanner(reader, buffer, identifierScanner, Token.BeginOfInput, Nil, Nil)
  }
}

final class Scanner private(reader: CharReader,
                            buffer: CharBuffer,
                            identifierScanner: IdentifierScanner,
                            private var prevToken: Token,
                            private var queue: List[Pos[Token]],
                            private var regions: List[RegionAttributes]) {
  def get(): ScannerResult = {
    @tailrec
    def go(): ScannerResult =
      queue match {
        case h :: t =>
          queue = t
          yieldSuccess(h)
        case Nil =>
          readNext() match {
            case Success(h) =>
              if (queue.nonEmpty) go()
              else Success(h)
            case other => other
          }
      }

    go()
  }

  private def yieldSuccess(token: Pos[Token]): ScannerResult = {
    prevToken = token.value
    val oldQueue = queue
    queue = Nil
    val suppressed = updateRegions(token)
    queue = queue ++ oldQueue
    if (suppressed) {
      get()
    } else {
      Success(token)
    }
  }

  private def readNext(): ScannerResult = {
    regions match {
      case RegionAttributes.InterpolatedString(multiLine, isRaw) :: _ =>
        return scanInterpolatedStringPart(multiLine, isRaw)
      case _ => ()
    }

    buffer.reset()
    var begin = reader.currentIndex

    // skipCommentsAndWhitespace returns true on success, false on unterminated comment.
    @tailrec
    def skipCommentsAndWhitespace(): Boolean = {
      Whitespace.scanWhitespace(reader)
      val commentResult = Comments.scanComments(reader)
      commentResult match {
        case CommentResult.NoComment => true
        case CommentResult.Unterminated => false
        case _ => skipCommentsAndWhitespace()
      }
    }

    if (!skipCommentsAndWhitespace()) {
      val end = reader.currentIndex
      return Error(Pos(ScannerError.UnclosedComment, begin, end))
    }

    begin = reader.currentIndex

    // For tokens containing only one char
    def success1(token: Token): ScannerResult =
      yieldSuccess(Pos(token, begin, begin))

    def fromEither(either: Pos[Either[ScannerError, Token]]): ScannerResult =
      either.value match {
        case Left(error) => Error(either.withNewValue(error))
        case Right(value) => yieldSuccess(either.withNewValue(value))
      }

    val next = reader.get() match {
      case Some(ch) =>
        (ch: @switch) match {
          case '(' => success1(Token.LParen)
          case ')' => success1(Token.RParen)
          case '[' => success1(Token.LBracket)
          case ']' => success1(Token.RBracket)
          case '{' => success1(Token.LBrace)
          case '}' => success1(Token.RBrace)
          case ',' => success1(Token.Comma)
          case ';' => success1(Token.Semicolon)
          case '\'' => fromEither(Literals.charLiteral(reader))
          case '"' => fromEither(Literals.stringLiteral(reader, buffer))
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            reader.unget(ch)
            fromEither(Literals.tryNumericLiteral(reader, buffer).getOrElse {
              // Should not happen as we just peeked a digit
              Pos(Left(ScannerError.InvalidLiteralNumber), begin, begin)
            })
          case '.' =>
            reader.get() match {
              case Some(c) if CharacterClass.isDigit(c) =>
                reader.unget(c)
                reader.unget('.')
                fromEither(Literals.tryNumericLiteral(reader, buffer).getOrElse {
                  Pos(Left(ScannerError.InvalidLiteralNumber), begin, begin)
                })
              case Some(c) =>
                reader.unget(c)
                success1(Token.Dot)
              case None =>
                success1(Token.Dot)
            }
          case '-' =>
            reader.get() match {
              case Some(c) if CharacterClass.isDigit(c) || c == '.' =>
                reader.unget(c)
                reader.unget('-')
                fromEither(Literals.tryNumericLiteral(reader, buffer).getOrElse {
                  Pos(Left(ScannerError.InvalidLiteralNumber), begin, begin)
                })
              case Some(c) =>
                reader.unget(c)
                reader.unget('-')
                identifierScanner.tryScan(reader, buffer) match {
                  case Some(result) => fromEither(result)
                  case None =>
                    reader.get() // consume the '-'
                    success1(Token.Identifier.Operator("-"))
                }
              case None =>
                reader.unget('-')
                identifierScanner.tryScan(reader, buffer) match {
                  case Some(result) => fromEither(result)
                  case None =>
                    reader.get() // consume the '-'
                    success1(Token.Identifier.Operator("-"))
                }
            }
          case _ =>
            reader.unget(ch)
            identifierScanner.tryScan(reader, buffer) match {
              case Some(result) => fromEither(result)
              case None =>
                // Handle numbers or other things not yet implemented
                reader.get() // consume the char we ungetted
                Error(Pos(ScannerError.InvalidCharacter, begin, begin))
            }
        }

      case None => ScannerResult.EndOfInput
    }

    next
  }

  private def scanInterpolatedStringPart(multiLine: Boolean, isRaw: Boolean): ScannerResult = {
    val begin = reader.currentIndex

    // Check for end of string first
    val isEnd = if (multiLine) {
      reader.matchSequence(TripleQuote)
    } else {
      reader.tryGet('"')
    }

    if (isEnd) {
      val end = reader.prevIndex
      yieldSuccess(Pos(Token.EndInterpolatedString, begin, end))
    } else if (reader.tryGet('$')) {
      if (reader.tryGet('$')) {
        // Escaped $ - let scanPart handle it
        reader.unget('$')
        reader.unget('$')
        val partResult = InterpolatedStrings.scanPart(reader, buffer, multiLine, isRaw)
        partResult.value match {
          case Right(token) => yieldSuccess(partResult.withNewValue(token))
          case Left(error) => Error(partResult.withNewValue(error))
        }
      } else {
        // $identifier or ${
        if (reader.tryGet('{')) {
          yieldSuccess(Pos(Token.BeginInterpolatedEscape, begin, reader.prevIndex))
        } else {
          reader.get() match {
            case Some(ch) =>
              if (CharacterClass.isOperator(ch)) {
                yieldSuccess(Pos(Token.Identifier.Operator(ch.toString), begin + 1, reader.prevIndex))
              } else if (CharacterClass.isIdentifierStart(ch)) {
                buffer.reset()
                buffer.write(ch)

                @tailrec
                def scanIdentifier(): ScannerResult = {
                  reader.get() match {
                    case Some(c) if CharacterClass.isIdentifierInner(c) && c != '$' =>
                      buffer.write(c)
                      scanIdentifier()
                    case Some(c) =>
                      reader.unget(c)
                      val name = buffer.slice()
                      val token = if (CharacterClass.isUppercase(ch)) Token.Identifier.Upper(name) else Token.Identifier.Lower(name)
                      yieldSuccess(Pos(token, begin + 1, reader.prevIndex))
                    case None =>
                      val name = buffer.slice()
                      val token = if (CharacterClass.isUppercase(ch)) Token.Identifier.Upper(name) else Token.Identifier.Lower(name)
                      yieldSuccess(Pos(token, begin + 1, reader.prevIndex))
                  }
                }

                scanIdentifier()
              } else {
                reader.unget(ch)
                Error(Pos(ScannerError.InvalidCharacter, begin, begin))
              }
            case None =>
              Error(Pos(ScannerError.InvalidCharacter, begin, begin))
          }
        }
      }
    } else {
      val partResult = InterpolatedStrings.scanPart(reader, buffer, multiLine, isRaw)
      partResult.value match {
        case Right(token) =>
          yieldSuccess(partResult.withNewValue(token))
        case Left(error) =>
          Error(partResult.withNewValue(error))
      }
    }
  }

  /**
   * Updates the current tracking regions based on the provided token.
   *
   * @param token the token that was just scanned
   * @return true if the token should be suppressed (not emitted to the token stream),
   *         false otherwise.
   */
  private def updateRegions(token: Pos[Token]): Boolean =
    token.value match {
      case LParen =>
        enterRegion(RegionAttributes.Parens)
        false
      case LBracket =>
        enterRegion(RegionAttributes.Brackets)
        false
      case LBrace =>
        regions match {
          case RegionAttributes.InterpolatedEscape :: _ =>
            // Already in escape, but we might have a brace in an expression
            enterRegion(RegionAttributes.Braces)
          case _ =>
            enterRegion(RegionAttributes.Braces)
        }
        false
      case Case =>
        enterRegion(RegionAttributes.Case)
        false
      case BeginInterpolatedString(name) =>
        enterRegion(RegionAttributes.InterpolatedString(multiLine = false, isRaw = name == Raw))
        false
      case BeginMultiLineInterpolatedString(name) =>
        enterRegion(RegionAttributes.InterpolatedString(multiLine = true, isRaw = name == Raw))
        false
      case BeginInterpolatedEscape =>
        enterRegion(RegionAttributes.InterpolatedEscape)
        false
      case EndInterpolatedString =>
        exitRegion(RegionType.InterpolatedString)
        false
      case RParen =>
        exitRegion(RegionType.Parens)
        false
      case RBracket =>
        exitRegion(RegionType.Brackets)
        false
      case RBrace =>
        regions match {
          case RegionAttributes.InterpolatedEscape :: _ =>
            exitRegion(RegionType.InterpolatedEscape)
            queue = queue :+ Pos(Token.EndInterpolatedEscape: Token, token.begin, token.end)
            true
          case _ =>
            exitRegion(RegionType.Braces)
            false
        }
      case RDoubleArrow =>
        exitRegion(RegionType.Case)
        false
      case _ => false
    }

  private def enterRegion(regionAttributes: RegionAttributes): Unit =
    regions = regionAttributes :: regions

  private def exitRegion(regionType: RegionType): Unit =
    regions match {
      case x :: xs =>
        if (x.regionType == regionType) regions = xs
      case Nil => ()
    }
}
