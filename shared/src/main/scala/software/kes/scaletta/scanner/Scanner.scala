package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerConstants.DoubleQuotes3
import software.kes.scaletta.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object Scanner {
  def create(reader: CharReader,
             identifierPolicy: IdentifierPolicy): Scanner = {
    val buffer = CharBuffer.create()
    val identifierScanner = new IdentifierScanner(identifierPolicy)
    val initialPos = Pos(Token.BeginOfInput: Token, reader.currentIndex, reader.currentIndex)
    new Scanner(reader, buffer, identifierScanner, Token.BeginOfInput, Nil, RegionStack.empty, initialPos)
  }
}

final class Scanner private(reader: CharReader,
                            buffer: CharBuffer,
                            identifierScanner: IdentifierScanner,
                            private var prevToken: Token,
                            private var queue: List[Pos[Token]],
                            private var regionStack: RegionStack,
                            private var lastPos: Pos[Token]) {
  def get(): Pos[Token] =
    queue match {
      case h :: t =>
        queue = t
        lastPos = h
        prevToken = h.value
        h
      case Nil =>
        val result = readNext()
        lastPos = result
        result
    }

  private def yieldSuccess(token: Pos[Token],
                           newlineEncounteredBefore: Option[CharIndex]): Pos[Token] = {
    if (newlineEncounteredBefore.isDefined &&
      prevToken.canTerminateStatement &&
      token.value.canBeginStatement &&
      newlinesEnabledInRegion() &&
      token.value != (Token.Semicolon: Token) &&
      prevToken != (Token.Semicolon: Token)) {
      val index = newlineEncounteredBefore.get
      val semicolonPos = Pos(Token.Semicolon: Token, index, index)
      queue = token :: queue
      prevToken = Token.Semicolon
      semicolonPos
    } else {
      val suppressed = updateRegions(token)
      if (suppressed) {
        prevToken = token.value
        get()
      } else {
        prevToken = token.value
        token
      }
    }
  }

  private def newlinesEnabledInRegion(): Boolean =
    regionStack.peek match {
      case Some(x) => x.newlinesEnabled
      case None => true
    }

  private def readNext(): Pos[Token] =
    regionStack.peek match {
      case Some(RegionAttributes.InterpolatedString(multiLine, isRaw)) =>
        scanInterpolatedStringPart(multiLine, isRaw)
      case _ =>
        val begin = reader.currentIndex
        skipCommentsAndWhitespace(None) match {
          case SkipCommentsResult.Unterminated =>
            Pos(Token.Error(ScannerError.UnclosedComment), begin, reader.currentIndex)
          case SkipCommentsResult.NewLinesEncountered(value) =>
            checkForForcedExit(Some(value)) match {
              case Some(p) => p
              case None => readToken(Some(value))
            }
          case SkipCommentsResult.NoNewLinesEncountered =>
            checkForForcedExit(None) match {
              case Some(p) => p
              case None => readToken(None)
            }
        }
    }

  private def checkForForcedExit(newlineEncountered: Option[CharIndex]): Option[Pos[Token]] = {
    regionStack.findFirstInterpolatedString.flatMap {
      case parentStringRegion: RegionAttributes.InterpolatedString if parentStringRegion.multiLine =>
        val begin = reader.currentIndex
        if (reader.matchSequence(ScannerConstants.DoubleQuotes3)) {
          val end = reader.prevIndex
          //          regionStack = regionStack.dropUntilAndIncluding(RegionType.InterpolatedString)
          regionStack = regionStack.dropUntilInterpolatedString
          val error = ScannerError.UnclosedMultiLineString
          Some(yieldSuccess(Pos(Token.Error(error), begin, end), newlineEncountered))
        } else None
      case _ => None
    }
  }

  private def readNormalToken(): Pos[Token] = {
    buffer.reset()
    val begin = reader.currentIndex
    skipCommentsAndWhitespace(None) match {
      case SkipCommentsResult.Unterminated =>
        Pos(Token.Error(ScannerError.UnclosedComment), begin, reader.currentIndex)
      case SkipCommentsResult.NewLinesEncountered(value) =>
        readToken(Some(value))
      case SkipCommentsResult.NoNewLinesEncountered =>
        readToken(None)
    }
  }

  private def readToken(newlineEncountered: Option[CharIndex]): Pos[Token] = {
    val begin = reader.currentIndex

    def canStartToken(ch: Char): Boolean =
      (ch: @switch) match {
        case '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | '.' | '\'' | '"' |
             '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '/' | '*' | '$' => true
        case _ => CharacterClass.isIdentifierStart(ch) || CharacterClass.isOperator(ch)
      }

    // For tokens containing only one char
    def success1(token: Token): Pos[Token] =
      yieldSuccess(Pos(token, begin, begin), newlineEncountered)

    def success(p: Pos[Token]): Pos[Token] =
      yieldSuccess(p, newlineEncountered)

    reader.get() match {
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
          case '\'' => success(Literals.charLiteral(reader))
          case '"' => success(Literals.stringLiteral(reader, buffer))
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            reader.unget(ch)
            Literals.tryNumericLiteral(reader, buffer) match {
              case Some(result) => success(result)
              case None =>
                // Should not happen as we just peeked a digit
                Pos(Error(ScannerError.InvalidLiteralNumber), begin, begin)
            }
          case '.' =>
            reader.get() match {
              case Some(c) if CharacterClass.isDigit(c) =>
                reader.unget(c)
                reader.unget('.')
                Literals.tryNumericLiteral(reader, buffer) match {
                  case Some(result) => success(result)
                  case None =>
                    Pos(Error(ScannerError.InvalidLiteralNumber), begin, begin)
                }
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
                Literals.tryNumericLiteral(reader, buffer) match {
                  case Some(result) => success(result)
                  case None =>
                    Pos(Error(ScannerError.InvalidLiteralNumber), begin, begin)
                }
              case Some(c) =>
                reader.unget(c)
                reader.unget('-')
                identifierScanner.tryScan(reader, buffer) match {
                  case Some(result) => success(result)
                  case None =>
                    reader.get() // consume the '-'
                    success1(Token.Identifier.Operator("-"))
                }
              case None =>
                reader.unget('-')
                identifierScanner.tryScan(reader, buffer) match {
                  case Some(result) => success(result)
                  case None =>
                    reader.get() // consume the '-'
                    success1(Token.Identifier.Operator("-"))
                }
            }
          case _ =>
            reader.unget(ch)
            identifierScanner.tryScan(reader, buffer) match {
              case Some(result) => success(result)
              case None =>
                @tailrec
                def skipGarbage(): Pos[Token] = {
                  reader.get() match {
                    case Some(c) =>
                      if (CharacterClass.isWhitespace(c) || canStartToken(c)) {
                        reader.unget(c)
                        Pos(Token.Error(ScannerError.InvalidCharacter), begin, reader.prevIndex)
                      } else {
                        skipGarbage()
                      }
                    case None =>
                      Pos(Token.Error(ScannerError.InvalidCharacter), begin, reader.prevIndex)
                  }
                }

                skipGarbage()
            }
        }

      case None =>
        lastPos match {
          case Pos(Token.EndOfInput, _, _) => lastPos
          case _ => Pos(Token.EndOfInput, begin, begin)
        }
    }
  }

  private def scanInterpolatedStringPart(multiLine: Boolean, isRaw: Boolean): Pos[Token] = {
    val begin = reader.currentIndex

    // Check for end of string first
    val isEnd = if (multiLine) {
      reader.matchSequence(DoubleQuotes3)
    } else {
      reader.tryGet('"')
    }

    if (isEnd) {
      val end = reader.prevIndex
      yieldSuccess(Pos(Token.EndInterpolatedString, begin, end), newlineEncounteredBefore = None)
    } else if (reader.tryGet('$')) {
      if (reader.tryGet('$')) {
        // Escaped $ - let scanPart handle it
        reader.unget('$')
        reader.unget('$')
        val partResult = InterpolatedStrings.scanPart(reader, buffer, multiLine, isRaw)
        partResult.value match {
          case Error(error) => partResult.withNewValue(Token.Error(error))
          case token => yieldSuccess(partResult.withNewValue(token), newlineEncounteredBefore = None)
        }
      } else {
        // $identifier or ${
        if (reader.tryGet('{')) {
          yieldSuccess(Pos(Token.BeginInterpolatedEscape, begin, reader.prevIndex), newlineEncounteredBefore = None)
        } else {
          reader.get() match {
            case Some(ch) if CharacterClass.isOperator(ch) =>
              yieldSuccess(Pos(Token.Identifier.Operator(ch.toString), begin + 1, reader.prevIndex), newlineEncounteredBefore = None)
            case Some(ch) if CharacterClass.isIdentifierStart(ch) =>
              buffer.reset()
              buffer.write(ch)

              @tailrec
              def scanIdentifier(): Pos[Token] = {
                reader.get() match {
                  case Some(c) if CharacterClass.isIdentifierInner(c) && c != '$' =>
                    buffer.write(c)
                    scanIdentifier()
                  case Some(c) =>
                    reader.unget(c)
                    val name = buffer.slice()
                    val token = if (CharacterClass.isUppercase(ch)) Token.Identifier.Upper(name) else Token.Identifier.Lower(name)
                    yieldSuccess(Pos(token, begin + 1, reader.prevIndex), newlineEncounteredBefore = None)
                  case None =>
                    val name = buffer.slice()
                    val token = if (CharacterClass.isUppercase(ch)) Token.Identifier.Upper(name) else Token.Identifier.Lower(name)
                    yieldSuccess(Pos(token, begin + 1, reader.prevIndex), newlineEncounteredBefore = None)
                }
              }

              scanIdentifier()
            case Some(ch) =>
              reader.unget(ch)
              Pos(Token.Error(ScannerError.InvalidCharacter), begin, begin)
            case None =>
              Pos(Token.Error(ScannerError.InvalidCharacter), begin, begin)
          }
        }
      }
    } else {
      val partResult = InterpolatedStrings.scanPart(reader, buffer, multiLine, isRaw)
      partResult.value match {
        case Error(error) =>
          // When a fatal literal error occurs, we must exit the interpolated string region
          // to prevent saturation and redundant errors.
          exitRegion(RegionType.InterpolatedString)
          val posToken: Pos[Token] = partResult.withNewValue(Token.Error(error))
          prevToken = posToken.value
          posToken

        case token =>
          yieldSuccess(partResult.withNewValue(token), newlineEncounteredBefore = None)
      }
    }
  }

  @tailrec
  private def skipCommentsAndWhitespace(mostRecentNewline: Option[CharIndex] = None): SkipCommentsResult = {
    val wsResult = Whitespace.scanWhitespace(reader)
    val newlineEncountered = mostRecentNewline.orElse(wsResult.indexOfLastNewline)
    Comments.scanComments(reader) match {
      case CommentResult.NoComment =>
        newlineEncountered match {
          case Some(value) => SkipCommentsResult.NewLinesEncountered(value)
          case None => SkipCommentsResult.NoNewLinesEncountered
        }
      case CommentResult.Unterminated => SkipCommentsResult.Unterminated
      case CommentResult.BlockComment.MultiLine =>
        skipCommentsAndWhitespace(Some(reader.prevIndex)) // TODO: reevaluate if this is correct
      case CommentResult.LineComment(indexOfNewLine) =>
        skipCommentsAndWhitespace(Some(indexOfNewLine)) // TODO: reevaluate if this is correct
      case _ =>
        skipCommentsAndWhitespace(newlineEncountered)
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
        enterRegion(RegionAttributes.Braces)
        false
      case Case =>
        enterRegion(RegionAttributes.Case)
        false
      case BeginInterpolatedString(interpolator) =>
        enterRegion(RegionAttributes.InterpolatedString(multiLine = false, isRaw = interpolator == Interpolator.Raw))
        false
      case BeginMultiLineInterpolatedString(interpolator) =>
        enterRegion(RegionAttributes.InterpolatedString(multiLine = true, isRaw = interpolator == Interpolator.Raw))
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
        regionStack.peek match {
          case Some(RegionAttributes.InterpolatedEscape) =>
            exitRegion(RegionType.InterpolatedEscape)
            val endPos = Pos(Token.EndInterpolatedEscape: Token, token.begin, token.end)
            queue = endPos :: queue
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
    regionStack = regionStack.enter(regionAttributes)

  private def exitRegion(regionType: RegionType): Unit =
    regionStack = regionStack.exit(regionType)

  // TODO: revisit how we model this
  private sealed trait SkipCommentsResult {
    def encounteredNewline: Option[CharIndex]
  }

  private object SkipCommentsResult {
    case object NoNewLinesEncountered extends SkipCommentsResult {
      def encounteredNewline: Option[CharIndex] = None
    }

    case class NewLinesEncountered(indexOfMostRecent: CharIndex) extends SkipCommentsResult {
      def encounteredNewline: Option[CharIndex] = Some(indexOfMostRecent)
    }

    case object Unterminated extends SkipCommentsResult {
      def encounteredNewline: Option[CharIndex] = None
    }
  }
}
