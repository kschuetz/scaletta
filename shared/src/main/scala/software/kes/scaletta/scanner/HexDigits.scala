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
  def scanOne(reader: CharReader): Option[(Char, Byte)] =
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

  def scanN(n: Int, reader: CharReader): Option[Int] = {
    var result = 0
    var digits = List.empty[Char]
    var need = n
    var done = false
    while (need > 0) {
      scanOne(reader) match {
        case Some((ch, value)) =>
          need -= 1
          if (need > 0) digits = ch :: digits
          else done = true
          result = (result << 4) | (value & 0xf)
        case None =>
          digits.foreach(reader.unget)
          need = 0
      }
    }
    if (done) Some(result) else None
  }
}
