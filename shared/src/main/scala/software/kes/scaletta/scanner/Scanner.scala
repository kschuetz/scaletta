package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerConstants.TripleQuote
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
                            private var regions: List[RegionType]) {
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
    updateRegions(token)
    queue = queue ++ oldQueue
    Success(token)
  }

  private def readNext(): ScannerResult = {
    regions.headOption match {
      case Some(RegionType.InterpolatedString(multiLine, isRaw)) =>
        return scanInterpolatedStringPart(multiLine, isRaw)
      case _ => ()
    }

    buffer.reset()
    var begin = reader.currentIndex
    // TODO scan all comments and whitespace
    // TODO check for newlines in block comments
    if (Comments.scanComments(reader) == CommentResult.Unterminated) {
      val end = reader.currentIndex
      return Error(Pos(ScannerError.UnclosedComment, begin, end))
    }
    val whitespaceResult = Whitespace.scanWhitespace(reader)
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
          case '.' => success1(Token.Dot)
          case ',' => success1(Token.Comma)
          case ';' => success1(Token.Semicolon)
          case '\'' => fromEither(Literals.charLiteral(reader))
          case '"' => fromEither(Literals.stringLiteral(reader, buffer))
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
    } else if (reader.matchSequence("${")) {
      yieldSuccess(Pos(Token.BeginInterpolatedEscape, begin, reader.prevIndex))
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
        // $identifier
        reader.get() match {
          case Some(ch) if CharacterClass.isIdentifierStart(ch) && ch != '$' =>
            buffer.reset()
            buffer.write(ch)

            @tailrec
            def scanId(): ScannerResult = {
              reader.get() match {
                case Some(c) if CharacterClass.isIdentifierInner(c) && c != '$' =>
                  buffer.write(c)
                  scanId()
                case Some(c) =>
                  reader.unget(c)
                  val name = buffer.slice()
                  yieldSuccess(Pos(Token.Identifier.Lower(name), begin + 1, reader.prevIndex))
                case None =>
                  val name = buffer.slice()
                  yieldSuccess(Pos(Token.Identifier.Lower(name), begin + 1, reader.prevIndex))
              }
            }

            scanId()
          case Some(ch) =>
            reader.unget(ch)
            Error(Pos(ScannerError.InvalidCharacter, begin, begin))
          case None =>
            Error(Pos(ScannerError.InvalidCharacter, begin, begin))
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

  private def updateRegions(token: Pos[Token]): Unit =
    token.value match {
      case LParen => enterRegion(RegionType.Parens)
      case LBracket => enterRegion(RegionType.Brackets)
      case LBrace =>
        regions.headOption match {
          case Some(RegionType.InterpolatedEscape) =>
            // Already in escape, but we might have a brace in an expression
            enterRegion(RegionType.Braces)
          case _ =>
            enterRegion(RegionType.Braces)
        }
      case Case => enterRegion(RegionType.Case)
      case BeginInterpolatedString(name) =>
        enterRegion(RegionType.InterpolatedString(multiLine = false, isRaw = name == "raw"))
      case BeginMultiLineInterpolatedString(name) =>
        enterRegion(RegionType.InterpolatedString(multiLine = true, isRaw = name == "raw"))
      case BeginInterpolatedEscape =>
        enterRegion(RegionType.InterpolatedEscape)
      case EndInterpolatedString =>
        exitRegionByClass(classOf[RegionType.InterpolatedString])
      case RParen => exitRegion(RegionType.Parens)
      case RBracket => exitRegion(RegionType.Brackets)
      case RBrace =>
        regions.headOption match {
          case Some(RegionType.InterpolatedEscape) =>
            exitRegion(RegionType.InterpolatedEscape)
            queue = queue :+ Pos(Token.EndInterpolatedEscape: Token, token.begin, token.end)
          case _ =>
            exitRegion(RegionType.Braces)
        }
      case RDoubleArrow => exitRegion(RegionType.Case)
      case _ => ()
    }

  private def exitRegionByClass(cls: Class[_]): Unit =
    regions match {
      case x :: xs =>
        if (cls.isInstance(x)) regions = xs
      case Nil => ()
    }

  private def enterRegion(regionType: RegionType): Unit =
    regions = regionType :: regions

  private def exitRegion(regionType: RegionType): Unit =
    regions match {
      case x :: xs =>
        if (x == regionType) regions = xs
      case Nil => ()
    }
}



