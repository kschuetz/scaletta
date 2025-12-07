package software.kes.scaletta.scanner

object CharacterClass {
  def isWhitespace(ch: Char): Boolean =
    ch.isWhitespace

  def isIdentifierStart(ch: Char): Boolean =
    ch.isLetter || ch == '_' || ch == '$'

  def isIdentifierInner(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_' || ch == '$'

  def isOperator(ch: Char): Boolean =
    operators.contains(ch)

  def isUppercase(ch: Char): Boolean =
    ch.isUpper

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
    println(operators)
  }
}
