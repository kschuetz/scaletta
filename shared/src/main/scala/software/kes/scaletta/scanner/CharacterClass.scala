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
    isIdentifierStart(ch) || isDigit(ch) || isCombiningMark(ch)

  def isCombiningMark(ch: Char): Boolean = {
    val ct = Character.getType(ch)
    ct == Character.NON_SPACING_MARK.toInt ||
      ct == Character.COMBINING_SPACING_MARK.toInt ||
      ct == Character.ENCLOSING_MARK.toInt
  }

  def isOperator(ch: Char): Boolean = {
    isAsciiOperator(ch) || {
      val chtp = Character.getType(ch)
      chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
    }
  }

  def isUppercase(ch: Char): Boolean =
    Character.getType(ch) == Character.UPPERCASE_LETTER.toInt

  private def isDelimiter(ch: Char): Boolean =
    ch match {
      case '`' |
           '\'' |
           '"' |
           '.' |
           ';' |
           ',' => true
      case _ => false
    }

  private def isParenthesis(ch: Char): Boolean =
    ch match {
      case '(' |
           ')' |
           '[' |
           ']' |
           '{' |
           '}' => true
      case _ => false
    }

  private def isAsciiOperator(ch: Char): Boolean =
    (ch >= 33 && ch <= 126) && ! {
      isIdentifierInner(ch) || isDelimiter(ch) || isParenthesis(ch)
    }

  def main(args: Array[String]): Unit = {
    (0 to 65535).foreach { n =>
      val ch = n.toChar
      val chtp = Character.getType(ch)

      if (isOperator(ch)) println(s"${n.toChar}  ${n.toHexString}")
    }
  }
}
