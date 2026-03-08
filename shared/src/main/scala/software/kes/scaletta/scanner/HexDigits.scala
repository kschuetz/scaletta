package software.kes.scaletta.scanner

object HexDigits {

  /**
   * Returns value of digit (0..15), or -1 if not a valid hex digit
   */
  def digitValue(ch: Char): Byte =
    if (ch >= '0' && ch <= '9') {
      (ch - '0').toByte
    } else if (ch >= 'a' && ch <= 'f') {
      (10 + (ch - 'a')).toByte
    } else if (ch >= 'A' && ch <= 'F') {
      (10 + (ch - 'A')).toByte
    } else -1.toByte

  /**
   * @return the character, and the value of the digit (0..15)
   */
  def scanOne(reader: SourceReader): Option[(Char, Byte)] =
    reader.get() match {
      case Some(ch) =>
        val value = digitValue(ch)
        if (value >= 0) Some((ch, value))
        else {
          reader.unget(ch)
          None
        }
      case None => None
    }

  /**
   * Scans exactly N hex digits from the reader.
   *
   * @return Right(value) on success.
   *         Left(Some(ch)) if an invalid hex character was encountered.
   *         Left(None) if end-of-input was reached.
   *         In all failure cases, the reader is backtracked to its original state.
   */
  def scanN(n: Int, reader: SourceReader): Either[Option[Char], Int] = {
    var result = 0
    var digits = List.empty[Char]
    var need = n
    var done = false
    var offending: Option[Option[Char]] = None
    while (need > 0 && offending.isEmpty) {
      reader.get() match {
        case Some(ch) =>
          val value = digitValue(ch)
          if (value >= 0) {
            need -= 1
            digits = ch :: digits
            result = (result << 4) | (value & 0xf)
            if (need == 0) done = true
          } else {
            reader.unget(ch)
            offending = Some(Some(ch))
          }
        case None =>
          offending = Some(None)
      }
    }
    if (done) Right(result)
    else {
      digits.foreach(reader.unget)
      Left(offending.flatten)
    }
  }
}
