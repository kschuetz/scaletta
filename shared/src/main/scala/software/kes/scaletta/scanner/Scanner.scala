package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerResult._
import software.kes.scaletta.scanner.Token._
import software.kes.scaletta.util.CharBuffer

import scala.annotation.switch

final class Scanner private(reader: CharReader,
                            buffer: CharBuffer,
                            private var prevToken: Token,
                            private var queue: List[Pos[Token]],
                            private var regions: List[RegionType]) {
  def get(): ScannerResult =
    queue match {
      case h :: t =>
        queue = t
        yieldSuccess(h)
      case Nil =>
        readNext()
    }

  private def yieldSuccess(token: Pos[Token]): ScannerResult = {
    prevToken = token.value
    updateRegions(token)
    Success(token)
  }

  private def readNext(): ScannerResult = {
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
    def success1(token: Token): Success =
      Success(Pos(token, begin, begin))

    def fromEither(either: Pos[Either[ScannerError, Token]]): ScannerResult =
      either.value match {
        case Left(error) => Error(either.withNewValue(error))
        case Right(value) => Success(either.withNewValue(value))
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

        }

      case None => ScannerResult.EndOfInput
    }

    ???
  }

  private def updateRegions(token: Pos[Token]): Unit =
    token.value match {
      case LParen => enterRegion(RegionType.Parens)
      case LBracket => enterRegion(RegionType.Brackets)
      case LBrace => enterRegion(RegionType.Braces)
      case Case => enterRegion(RegionType.Case)
      case RParen => exitRegion(RegionType.Parens)
      case RBracket => exitRegion(RegionType.Brackets)
      case RBrace => exitRegion(RegionType.Braces)
      case RDoubleArrow => exitRegion(RegionType.Case)
      case _ => ()
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



