package software.kes.scaletta.scanner

import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.reporting.{CharIndex, Pos}
import software.kes.scaletta.scanner.ScannerConstants.DoubleQuotes3
import software.kes.scaletta.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

object Scanner {
  def create(reader: CharReader,
             identifierPolicy: IdentifierPolicy,
             portalMode: Boolean = false): Scanner = {
    val buffer = CharBuffer.create()
    val identifierScanner = new IdentifierScanner(identifierPolicy)
    val tokenBuffer = TokenBuffer.create()
    val regionStack = if (portalMode) {
      RegionStack.empty.enter(RegionAttributes.Portal)
    } else RegionStack.empty
    new Scanner(reader, buffer, identifierScanner, Token.BeginOfInput, tokenBuffer, regionStack, portalMode)
  }
}

final class Scanner private(reader: CharReader,
                            buffer: CharBuffer,
                            identifierScanner: IdentifierScanner,
                            private var lastConsumedToken: Token,
                            private val tokenBuffer: TokenBuffer,
                            private var regionStack: RegionStack,
                            private val portalMode: Boolean) {

  def get(): Pos[Token] = {
    fillBuffer(1)
    val result = tokenBuffer.dequeue()
    lastConsumedToken = result.value
    result
  }

  /**
   * Returns the token at the specified lookahead position without consuming it.
   *
   * @param n the lookahead position (1 for the next token, 2 for the one after that, etc.)
   * @return the token at the specified position, or EndOfInput if the position is beyond the end of the input
   * @throws IllegalArgumentException if n < 1
   */
  def peek(n: Int): Pos[Token] = {
    require(n >= 1, "n must be at least 1")
    fillBuffer(n)
    tokenBuffer.get(n - 1)
  }

  @tailrec
  private def fillBuffer(n: Int): Unit = {
    if (tokenBuffer.length < n && !tokenBuffer.isExhausted) {
      val effect = readNext()
      effect(tokenBuffer)
      fillBuffer(n)
    }
  }

  private def yieldSuccess(token: Pos[Token],
                           newlineEncounteredBefore: Option[CharIndex]): TokenBuffer.Effect =
    (tokenBuffer: TokenBuffer) => {
      val isFinalClosingBrace = regionStack.peek.contains(RegionAttributes.Portal) && token.value == Token.RBrace
      val effectOnBuffer = updateRegions(token)
      val precedingToken = tokenBuffer.mostRecentlyAdded.map(_.value).getOrElse(lastConsumedToken)

      if (!isFinalClosingBrace &&
        newlineEncounteredBefore.isDefined &&
        precedingToken.canTerminateStatement &&
        token.value.canBeginStatement &&
        newlinesEnabledInRegion() &&
        token.value != (Token.Semicolon: Token) &&
        precedingToken != (Token.Semicolon: Token)) {
        val index = newlineEncounteredBefore.get
        val semicolonPos = Pos(Token.Semicolon: Token, index, index)
        tokenBuffer.enqueue(semicolonPos)
      }
      effectOnBuffer match {
        case Some(effect) =>
          effect(tokenBuffer)
        case None =>
          tokenBuffer.enqueue(token)
      }
    }

  private def newlinesEnabledInRegion(): Boolean =
    regionStack.peek match {
      case Some(x) => x.newlinesEnabled
      case None => true
    }

  private def readNext(): TokenBuffer.Effect =
    regionStack.peek match {
      case Some(RegionAttributes.InterpolatedString(multiLine, isRaw)) =>
        scanInterpolatedStringPart(multiLine, isRaw)
      case _ =>
        val begin = reader.currentIndex
        skipCommentsAndWhitespace(None) match {
          case SkipCommentsResult.Unterminated =>
            (tokenBuffer: TokenBuffer) =>
              tokenBuffer.enqueue(Pos(Token.Error(ScanError.UnclosedComment), begin, reader.currentIndex))
          case SkipCommentsResult.NewLinesEncountered(index) =>
            val someIndex = Some(index)
            checkForForcedExit(someIndex).getOrElse(readToken(someIndex))
          case SkipCommentsResult.NoNewLinesEncountered =>
            checkForForcedExit(None).getOrElse {
              if (reader.peek().isEmpty) {
                (tokenBuffer: TokenBuffer) => {
                  if (portalMode && regionStack.peek.isDefined) {
                    val pos: Pos[Token] = Pos(Token.Error(ScanError.UnbalancedBraces): Token, begin, begin)
                    tokenBuffer.enqueue(pos)
                  }
                  val endOfInputPos: Pos[Token] = Pos(Token.EndOfInput: Token, begin, begin)
                  tokenBuffer.terminate(endOfInputPos)
                }
              } else {
                readToken(None)
              }
            }
        }
    }

  private def checkForForcedExit(newlineEncountered: Option[CharIndex]): Option[TokenBuffer.Effect] = {
    regionStack.findFirstInterpolatedString match {
      case Some(parentStringRegion: RegionAttributes.InterpolatedString) if parentStringRegion.multiLine =>
        val begin = reader.currentIndex
        if (reader.matchSequence(ScannerConstants.DoubleQuotes3)) {
          val end = reader.prevIndex
          regionStack = regionStack.dropUntilInterpolatedString
          val error = ScanError.UnclosedMultiLineString
          Some(yieldSuccess(Pos(Token.Error(error), begin, end), newlineEncountered))
        } else {
          None
        }
      case _ => None
    }
  }

  private def readToken(newlineEncountered: Option[CharIndex]): TokenBuffer.Effect = {
    val begin = reader.currentIndex

    def canStartToken(ch: Char): Boolean =
      (ch: @switch) match {
        case '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | '.' | '\'' | '"' |
             '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '/' | '*' | '$' => true
        case _ => CharacterClass.isIdentifierStart(ch) || CharacterClass.isOperator(ch)
      }

    // For tokens containing only one char
    def success1(token: Token): TokenBuffer.Effect =
      yieldSuccess(Pos(token, begin, begin), newlineEncountered)

    def success(p: Pos[Token]): TokenBuffer.Effect =
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
                (tokenBuffer: TokenBuffer) =>
                  tokenBuffer.enqueue(Pos(Error(ScanError.InvalidLiteralNumber), begin, begin))
            }
          case '.' =>
            reader.get() match {
              case Some(c) if CharacterClass.isDigit(c) =>
                reader.unget(c)
                reader.unget('.')
                Literals.tryNumericLiteral(reader, buffer) match {
                  case Some(result) => success(result)
                  case None =>
                    reader.get() // consume the '.'
                    val end = reader.prevIndex
                    (tokenBuffer: TokenBuffer) =>
                      tokenBuffer.enqueue(Pos(Error(ScanError.InvalidLiteralNumber), begin, end))
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
                    reader.get() // consume the '-'
                    reader.tryGet('.') // consume the '.' if present
                    val end = reader.prevIndex
                    (tokenBuffer: TokenBuffer) =>
                      tokenBuffer.enqueue(Pos(Error(ScanError.InvalidLiteralNumber), begin, end))
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
                def skipGarbage(): TokenBuffer.Effect =
                  reader.get() match {
                    case Some(c) =>
                      if (c == '\r' || c == '\n' || CharacterClass.isWhitespace(c) || canStartToken(c)) {
                        reader.unget(c)
                        val end = reader.prevIndex
                        (tokenBuffer: TokenBuffer) =>
                          tokenBuffer.enqueue(Pos(Token.Error(ScanError.InvalidCharacter), begin, end))
                      } else {
                        skipGarbage()
                      }
                    case None =>
                      val end = reader.prevIndex
                      (tokenBuffer: TokenBuffer) =>
                        tokenBuffer.enqueue(Pos(Token.Error(ScanError.InvalidCharacter), begin, end))
                  }

                skipGarbage()
            }
        }

      case None =>
        (tokenBuffer: TokenBuffer) => {
          val isUnbalanced = regionStack.peek match {
            case Some(RegionAttributes.Portal) => portalMode
            case Some(_) => true
            case None => portalMode
          }
          if (isUnbalanced) {
            val pos: Pos[Token] = Pos(Token.Error(ScanError.UnbalancedBraces): Token, begin, begin)
            tokenBuffer.enqueue(pos)
          }
          val endOfInputPos: Pos[Token] = Pos(Token.EndOfInput: Token, begin, begin)
          tokenBuffer.terminate(endOfInputPos)
        }
    }
  }

  private def scanInterpolatedStringPart(multiLine: Boolean, isRaw: Boolean): TokenBuffer.Effect = {
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
        // Escaped $ - handled directly
        yieldSuccess(Pos(Token.InterpolatedPart("$"), begin, reader.prevIndex), newlineEncounteredBefore = None)
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
              def scanIdentifier(): TokenBuffer.Effect = {
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
              (tokenBuffer: TokenBuffer) =>
                tokenBuffer.enqueue(Pos(Token.Error(ScanError.InvalidCharacter), begin, begin))
            case None =>
              (tokenBuffer: TokenBuffer) =>
                tokenBuffer.enqueue(Pos(Token.Error(ScanError.InvalidCharacter), begin, begin))
          }
        }
      }
    } else {
      val partResult = InterpolatedStrings.scanPart(reader, buffer, multiLine, isRaw)
      partResult.value match {
        case Error(error) =>
          (tokenBuffer: TokenBuffer) => {
            // When a fatal literal error occurs, we must exit the interpolated string region
            // to prevent saturation and redundant errors.
            exitRegion(RegionType.InterpolatedString)
            val posToken: Pos[Token] = partResult.withNewValue(Token.Error(error))
            tokenBuffer.enqueue(posToken)
          }
        case token =>
          yieldSuccess(partResult.withNewValue(token), newlineEncounteredBefore = None)
      }
    }
  }

  @tailrec
  private def skipCommentsAndWhitespace(mostRecentNewline: Option[CharIndex] = None): SkipCommentsResult = {
    val wsResult = Whitespace.scanWhitespace(reader)
    val currentNewline = wsResult.indexOfLastNewline.orElse(mostRecentNewline)

    Comments.scanComments(reader) match {
      case CommentResult.NoComment =>
        currentNewline match {
          case Some(index) => SkipCommentsResult.NewLinesEncountered(index)
          case None => SkipCommentsResult.NoNewLinesEncountered
        }

      case CommentResult.Unterminated =>
        SkipCommentsResult.Unterminated

      case CommentResult.LineComment(indexOfNewLine) =>
        skipCommentsAndWhitespace(indexOfNewLine.orElse(currentNewline))

      case CommentResult.BlockComment.MultiLine(indexOfLastNewLine) =>
        skipCommentsAndWhitespace(Some(indexOfLastNewLine))

      case CommentResult.BlockComment.SingleLine =>
        skipCommentsAndWhitespace(currentNewline)
    }
  }

  /**
   * Updates the current tracking regions based on the provided token.
   *
   * @param token the token that was just scanned
   * @return Some(effect) if the region change requires a special side effect,
   *         None otherwise (the token should be enqueued normally).
   */
  private def updateRegions(token: Pos[Token]): Option[TokenBuffer.Effect] =
    token.value match {
      case LParen =>
        enterRegion(RegionAttributes.Parens)
        None
      case LBracket =>
        enterRegion(RegionAttributes.Brackets)
        None
      case LBrace =>
        enterRegion(RegionAttributes.Braces)
        None
      case Case =>
        enterRegion(RegionAttributes.Case)
        None
      case BeginInterpolatedString(interpolator) =>
        enterRegion(RegionAttributes.InterpolatedString(multiLine = false, isRaw = interpolator == Interpolator.Raw))
        None
      case BeginMultiLineInterpolatedString(interpolator) =>
        enterRegion(RegionAttributes.InterpolatedString(multiLine = true, isRaw = interpolator == Interpolator.Raw))
        None
      case BeginInterpolatedEscape =>
        enterRegion(RegionAttributes.InterpolatedEscape)
        None
      case EndInterpolatedString =>
        exitRegion(RegionType.InterpolatedString)
        None
      case RParen =>
        exitRegion(RegionType.Parens)
        None
      case RBracket =>
        exitRegion(RegionType.Brackets)
        None
      case RBrace =>
        regionStack.peek match {
          case Some(RegionAttributes.Portal) =>
            Some((tokenBuffer: TokenBuffer) => {
              val endOfInputPos = Pos(Token.EndOfInput: Token, reader.currentIndex, reader.currentIndex)
              tokenBuffer.terminate(endOfInputPos)
            })
          case Some(RegionAttributes.InterpolatedEscape) =>
            Some((tokenBuffer: TokenBuffer) => {
              exitRegion(RegionType.InterpolatedEscape)
              val endPos = Pos(Token.EndInterpolatedEscape: Token, token.begin, token.end)
              tokenBuffer.enqueue(endPos)
            })
          case _ =>
            exitRegion(RegionType.Braces)
            None
        }
      case RDoubleArrow =>
        exitRegion(RegionType.Case)
        None
      case _ => None
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
