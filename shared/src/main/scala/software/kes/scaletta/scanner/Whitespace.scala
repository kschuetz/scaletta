package software.kes.scaletta.scanner

sealed trait WhitespaceResult

object WhitespaceResult {
  case object NoWhitespace extends WhitespaceResult

  case object NoNewlines extends WhitespaceResult

  case object OneNewline extends WhitespaceResult

  case object TwoNewlines extends WhitespaceResult
}

object Whitespace {

  import WhitespaceResult._

  def scanWhitespace(reader: CharReader): WhitespaceResult = {
    var loop = true
    var result: WhitespaceResult = NoWhitespace
    var prev: Char = 0
    while (loop) {
      reader.get() match {
        case Some(ch) =>
          if (ch == '\n') {
            result match {
              case NoWhitespace | NoNewlines =>
                result = OneNewline
              case OneNewline =>
                if (prev == '\n') result = TwoNewlines
              case _ => ()
            }
            prev = ch
          } else if (isWhitespace(ch)) {
            if (result == NoWhitespace) {
              result = NoNewlines
            }
            prev = ch
          } else {
            reader.unget(ch)
            loop = false
          }
        case None => loop = false
      }
    }
    result
  }

  private def isWhitespace(ch: Char): Boolean =
    ch.isWhitespace
}
