package software.kes.scaletta.scanner

import scala.annotation.switch

object EscapeSequence {
  def scan(reader: CharReader): EscapeResult =
    reader.get() match {
      case Some(ch) =>
        (ch: @switch) match {
          case 'b' => EscapeResult.Success('\b')
          case 't' => EscapeResult.Success('\t')
          case 'n' => EscapeResult.Success('\n')
          case 'f' => EscapeResult.Success('\f')
          case 'r' => EscapeResult.Success('\r')
          case '"' => EscapeResult.Success('"')
          case '\'' => EscapeResult.Success('\'')
          case '\\' => EscapeResult.Success('\\')
          case 'u' => scanUnicodeSequence(reader)
          case '\n' | '\r' =>
            reader.unget(ch)
            EscapeResult.Boundary
          case other =>
            reader.unget(other)
            EscapeResult.Error(ScannerError.InvalidEscapeCharacter)
        }
      case None => EscapeResult.Error(ScannerError.InvalidEscapeCharacter)
    }

  private def scanUnicodeSequence(reader: CharReader): EscapeResult = {
    HexDigits.scanN(4, reader) match {
      case Right(value) => EscapeResult.Success(value.toChar)
      case Left(Some(ch)) if ch == '\n' || ch == '\r' =>
        reader.unget('u')
        EscapeResult.Boundary
      case Left(Some(_)) =>
        reader.unget('u')
        EscapeResult.Error(ScannerError.InvalidEscapeCharacter)
      case Left(None) =>
        reader.unget('u')
        EscapeResult.Error(ScannerError.InvalidEscapeCharacter)
    }
  }
}
