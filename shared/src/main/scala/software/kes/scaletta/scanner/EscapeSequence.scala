package software.kes.scaletta.scanner

import scala.annotation.switch

object EscapeSequence {
  def scan(reader: CharReader): Option[Char] =
    reader.get() match {
      case Some(ch) =>
        (ch: @switch) match {
          case 'b' => Some('\b')
          case 't' => Some('\t')
          case 'n' => Some('\n')
          case 'f' => Some('\f')
          case 'r' => Some('\r')
          case '"' => Some('"')
          case '\'' => Some('\'')
          case '\\' => Some('\\')
          case 'u' => scanUnicodeSequence(reader)
          case '\n' | '\r' =>
            reader.unget(ch)
            None
          case _ => None
        }
      case None => None
    }

  private def scanUnicodeSequence(reader: CharReader): Option[Char] =
    HexDigits.scanN(4, reader).map(_.toChar)
}
