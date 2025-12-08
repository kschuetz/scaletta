package software.kes.scaletta.scanner

object CharacterClass {
  def isWhitespace(ch: Char): Boolean =
    ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'

  def isDigit(ch: Char): Boolean =
    (ch >= '0' && ch <= '9')

  def isLetter(ch: Char): Boolean = {
    val ct = Character.getType(ch)
    ct == Character.LOWERCASE_LETTER.toInt || ct == Character.UPPERCASE_LETTER.toInt ||
      ct == Character.TITLECASE_LETTER.toInt || ct == Character.LETTER_NUMBER.toInt ||
      ct == Character.MODIFIER_LETTER
  }


  def isIdentifierStart(ch: Char): Boolean =
    ch == '_' || ch == '$' || isLetter(ch)

  def isIdentifierInner(ch: Char): Boolean =
    isIdentifierStart(ch) || isDigit(ch)

  def isOperator(ch: Char): Boolean =
    operators.contains(ch) || {
      val chtp = Character.getType(ch)
      chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
    }

  def isUppercase(ch: Char): Boolean =
    Character.getType(ch) == Character.UPPERCASE_LETTER.toInt

  private lazy val delimiters =
    Set('`', '\'', '"', '.', ';', ',')

  private lazy val parentheses =
    Set('(', ')', '[', ']', '{', '}')

  private lazy val operators =
    (33.toChar to 126.toChar).foldLeft(Set.empty[Char]) {
      case (acc, ch) =>
        if (isIdentifierInner(ch) || delimiters.contains(ch) || parentheses.contains(ch)) acc
        else acc + ch
    }

  def main(args: Array[String]): Unit = {
    (0 to 65535).foreach { n =>
      val ch = n.toChar
      val chtp = Character.getType(ch)

      if (isUppercase(ch)) println(s"${n.toChar}  ${n.toHexString}")
    }

  }
}
