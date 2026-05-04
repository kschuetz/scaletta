package software.kes.scaletta.internal.scanner

import software.kes.scaletta.internal.reader.SourceReader

import scala.annotation.switch

object EscapeSequence {
  def scan(reader: SourceReader): EscapeResult =
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
            EscapeResult.Error(ScanError.InvalidEscapeCharacter)
        }
      case None => EscapeResult.Error(ScanError.InvalidEscapeCharacter)
    }

  private def scanUnicodeSequence(reader: SourceReader): EscapeResult = {
    HexDigits.scanN(4, reader) match {
      case Right(value) => EscapeResult.Success(value.toChar)
      case Left(Some(ch)) if ch == '\n' || ch == '\r' =>
        reader.unget('u')
        EscapeResult.Boundary
      case Left(Some(_)) =>
        reader.unget('u')
        EscapeResult.Error(ScanError.InvalidEscapeCharacter)
      case Left(None) =>
        reader.unget('u')
        EscapeResult.Error(ScanError.InvalidEscapeCharacter)
    }
  }
}
