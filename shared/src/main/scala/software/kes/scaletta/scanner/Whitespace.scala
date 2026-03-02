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
    var result: WhitespaceResult = NoWhitespace
    var loop = true

    while (loop) {
      reader.get() match {
        case Some('\n') =>
          val index = reader.prevIndex
          reader.recordNewline(reader.currentIndex)
          result = updateResultWithNewline(result, index)
        case Some('\r') =>
          val index = reader.prevIndex
          reader.tryGet('\n')
          reader.recordNewline(reader.currentIndex)
          result = updateResultWithNewline(result, index)
        case Some(ch) if isHorizontalWhitespace(ch) =>
          if (result == NoWhitespace) result = NoNewlines
        case Some(ch) =>
          reader.unget(ch)
          loop = false
        case None =>
          loop = false
      }
    }
    result
  }

  private def updateResultWithNewline(current: WhitespaceResult, index: CharIndex): WhitespaceResult =
    current match {
      case _: Newlines => Newlines(index, moreThanOne = true)
      case _ => Newlines(index, moreThanOne = false)
    }

  private def isHorizontalWhitespace(ch: Char): Boolean =
    ch == ' ' || ch == '\t' || ch == '\u000B' || ch == '\f'
}
