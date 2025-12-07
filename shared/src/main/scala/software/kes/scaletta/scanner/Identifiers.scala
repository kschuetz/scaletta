package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.ScannerError.{EmptyQuotedIdentifier, InvalidEscapeCharacter, UnclosedQuotedIdentifier}
import software.kes.scaletta.util.CharBuffer

import scala.annotation.{switch, tailrec}

// TODO: max length policy
// TODO: Scala reserved word policy?
object Identifiers {

  /**
   * Assumes first character is already in buffer.
   * Can return one of the following:
   *   - Identifier (upper, lower, or operator)
   *   - Reserved word
   */
  def plain(reader: CharReader,
            buffer: CharBuffer): Pos[Token] = {
    val begin = reader.prevIndex

    @tailrec
    def normal(wasUnderscore: Boolean): Unit =
      reader.get() match {
        case Some(ch) =>
          if (CharacterClass.isIdentifierInner(ch)) {
            buffer.write(ch)
            normal(ch == '_')
          } else if (wasUnderscore && CharacterClass.isOperator(ch) && buffer.size > 1) {
            buffer.write(ch)
            operator()
          } else {
            reader.unget(ch)
          }

        case None => ()
      }

    @tailrec
    def operator(): Unit = {
      reader.get() match {
        case Some(ch) =>
          if (ch == '/') {
            // check if comment
            if (reader.peek().exists(c => c == '/' || c == '*')) {
              reader.unget(ch)
            } else {
              buffer.write(ch)
              operator()
            }
          } else if (CharacterClass.isOperator(ch)) {
            buffer.write(ch)
            operator()
          } else {
            reader.unget(ch)
          }

        case None => ()
      }
    }

    val firstChar = buffer.firstChar
    val isOperator = CharacterClass.isOperator(firstChar)
    if (isOperator) {
      operator()
    } else {
      normal(buffer.firstChar == '_')
    }
    val name = buffer.slice()
    val end = reader.prevIndex
    val token = Token.reservedWordByName.get(name) match {
      case Some(reservedWord) => reservedWord
      case None =>
        if (isOperator) Token.Identifier.Operator(name)
        else if (CharacterClass.isUppercase(firstChar)) Token.Identifier.Upper(name)
        else Token.Identifier.Lower(name)
    }
    Pos(token, begin, end)
  }

  /**
   * Assumes empty buffer
   */
  def quoted(reader: CharReader,
             buffer: CharBuffer): Pos[Either[ScannerError, Token]] = {
    val begin = reader.prevIndex

    @tailrec
    def go: Pos[Either[ScannerError, Token]] =
      reader.get() match {
        case Some(ch) =>
          (ch: @switch) match {
            case '\\' => escapeSequence
            case '\n' => Pos(Left(UnclosedQuotedIdentifier), reader.prevIndex)
            case '`' =>
              if (buffer.isEmpty) Pos(Left(EmptyQuotedIdentifier), reader.prevIndex)
              else Pos(Right(Token.Identifier.Quoted(buffer.slice())), begin, reader.prevIndex)
            case _ =>
              buffer.write(ch)
              go
          }
        case None => Pos(Left(UnclosedQuotedIdentifier), reader.prevIndex)
      }

    def escapeSequence: Pos[Either[ScannerError, Token]] =
      EscapeSequence.scan(reader) match {
        case Some(value) =>
          buffer.write(value)
          go
        case None => Pos(Left(InvalidEscapeCharacter), reader.prevIndex)
      }

    go
  }

  def main(args: Array[String]): Unit = {
  }
}
