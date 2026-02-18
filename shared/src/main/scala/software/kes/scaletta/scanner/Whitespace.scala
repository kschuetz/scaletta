package software.kes.scaletta.scanner

sealed trait WhitespaceResult {
  def indexOfLastNewline: Option[CharIndex]
}

object WhitespaceResult {
  case object NoWhitespace extends WhitespaceResult {
    def indexOfLastNewline: Option[CharIndex] = None
  }

  case object NoNewlines extends WhitespaceResult {
    def indexOfLastNewline: Option[CharIndex] = None
  }

  case class Newlines(lastIndex: CharIndex,
                      moreThanOne: Boolean) extends WhitespaceResult {
    def indexOfLastNewline: Option[CharIndex] = Some(lastIndex)
  }
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
                result = Newlines(reader.prevIndex, moreThanOne = false)
              case _: Newlines =>
                if (prev == '\n') result = Newlines(reader.prevIndex, moreThanOne = true)
              case _ => ()
            }
            prev = ch
          } else if (isWhitespace(ch)) {
            if (result == NoWhitespace) {
              result = NoNewlines
            }
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
